(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open Compat
open Pos
open Nstream
open Approx_lexer
open Util

type output_elt = Newline | Indent of int | Whitespace of string | Text of string

type 'a output_kind =
  | Numeric of (int -> 'a -> 'a)
  | Print of (string -> 'a -> 'a)
  | Extended of (IndentBlock.t -> output_elt -> 'a -> 'a)

type 'a output = {
  debug: bool;
  config: IndentConfig.t;
  (* Returns true on the lines that should be reindented *)
  in_lines: int -> bool;
  adaptive: bool;
  indent_empty: bool;
  kind: 'a output_kind;
}

let std_output = {
  debug = false;
  config = IndentConfig.default;
  in_lines = (fun _ -> true);
  adaptive = true;
  indent_empty = false;
  kind = Print (fun s () -> print_endline s);
}

(* utility functions *)

let pr_string output block text usr =
  match output.kind with
  | Numeric _ -> usr
  | Print f -> f text usr
  | Extended f -> f block (Text text) usr

let pr_whitespace output block text usr =
  match output.kind with
  | Numeric _ -> usr
  | Print f -> f text usr
  | Extended f -> f block (Whitespace text) usr

let pr_nl output block usr =
  match output.kind with
  | Numeric _ -> usr
  | Print pr -> pr "\n" usr
  | Extended pr -> pr block Newline usr

(* indent functions *)

type indentKind = Normal
                | Empty (* empty line: depending on options, don't indent
                           or try to guess expected indent. *)
                | Padded (* for comment continuations: indent the first
                            line as the following ones*)
                | Fixed of int (* indent to this value, ignoring the block *)

let warn_tabs = ref true

(* must be called exactly once for each line, in order *)
(* let line_debug_counter = ref 0 *)
let print_indent output line blank ?(kind=Normal) block usr =
  (* assert (incr line_debug_counter; line = !line_debug_counter); *)
  if output.in_lines line then
    let indent =
      match kind with
      | Normal -> IndentBlock.indent block
      | Empty ->
          if output.indent_empty then IndentBlock.guess_indent line block
          else  0
      | Padded ->
          IndentBlock.indent block + IndentBlock.padding block
      | Fixed n -> n
    in
    match output.kind with
    | Numeric pr -> pr indent usr
    | Print pr -> pr (String.make indent ' ') usr
    | Extended pr -> pr block (Indent indent) usr
  else (
    if !warn_tabs && String.contains blank '\t' then (
      warn_tabs := false;
      prerr_endline
        "Warning: ocp-indent input contains indentation by tabs, \
         partial indent will be unreliable."
    );
    match output.kind with
    | Numeric _ -> usr
    | Print pr -> pr blank usr
    | Extended pr -> pr block (Whitespace blank) usr
  )

let print_token output block tok usr =
  let orig_start_column = IndentBlock.original_column block in
  let start_column = IndentBlock.offset block in
  (* Handle multi-line tokens (strings, comments) *)
  let rec print_extra_lines line pad last ?(item_cont=false) lines usr =
    match lines with
    | [] -> usr
    | text::next_lines ->
        let usr = usr |> pr_nl output block in
        if not (output.in_lines line) then
          usr
          |> print_indent output line "" block
          |> pr_string output block text
          |> print_extra_lines (line+1) pad text next_lines
        else if String.trim text = "" && tok.token <> OCAMLDOC_VERB then
          usr
          |> print_indent output line "" ~kind:Empty block
          |> print_extra_lines (line+1) pad text next_lines
        else
          let orig_line_indent = count_leading_spaces text in
          let orig_offset = orig_line_indent - orig_start_column in
          let text =
            String.sub text orig_line_indent
              (String.length text - orig_line_indent)
          in
          let indent_value, item_cont =
            match pad with
            | None -> orig_line_indent, false
            | Some pad -> match tok.token with
                | STRING _ ->
                    if ends_with_escape last then
                      if is_prefix "\"" text || is_prefix "\\ " text
                      then start_column, item_cont
                      else start_column + pad, item_cont
                    else orig_line_indent, item_cont
                | COMMENT | COMMENTCONT ->
                    let is_item =
                      is_prefix "- " text && not (is_prefix "- :" text)
                    in
                    let n =
                      if is_prefix "*" text then 1 else
                      if not is_item && item_cont then pad + 2
                      else pad
                    in
                    let item_cont = is_item || item_cont && text <> "" in
                    let n =
                      if output.config.IndentConfig.i_strict_comments || is_item
                      then n else max orig_offset n
                    in
                    let n = if next_lines = [] && text = "*)" then 0 else n in
                    start_column + n, item_cont
                | QUOTATION opening ->
                    if is_prefix "{" opening then orig_line_indent, item_cont
                    else
                      (start_column +
                       if next_lines = [] && text = ">>" then 0
                       else max orig_offset pad),
                      item_cont
                | _ -> start_column + max orig_offset pad, item_cont
          in
          usr
          |> print_indent output line "" ~kind:(Fixed indent_value) block
          |> pr_string output block text
          |> print_extra_lines (line+1) pad ~item_cont text next_lines
  in
  let line = Region.start_line tok.region in
  let text, next_lines =
    if line = Region.end_line tok.region then (Lazy.force tok.substr), []
    else match string_split '\n' (Lazy.force tok.substr) with
      | [] -> assert false
      | hd::tl -> hd,tl
  in
  let pad =
    if next_lines = [] then None
    else match tok.token with
      | STRING _ ->
          (match String.trim text with
           | "\"" | "\"\\" -> None
           | _ -> Some 1 (* length of '"' *))
      | COMMENT ->
          (match String.trim text with
           | "(*" when not output.config.IndentConfig.i_strict_comments -> None
           | _ -> Some (IndentBlock.padding block))
      | COMMENTCONT ->
          Some (IndentBlock.padding block)
      | OCAMLDOC_VERB -> None
      | QUOTATION opening ->
          let oplen = String.length opening in
          let textlen = String.length text in
          if oplen = textlen then None
          else
            Some
              (oplen +
               count_leading_spaces
                 (String.sub text oplen (textlen - oplen - 1)))
      | _ -> Some 2
  in
  usr
  |> pr_string output block text
  |> print_extra_lines (line+1) pad text next_lines

(* [block] is the current indentation block
   [stream] is the token stream *)
let rec loop output block stream usr =
  match Nstream.next stream with
  | None -> usr (* End of file *)
  | Some (t, stream) ->
      let line = Region.start_line t.region in
      let last_line = line - t.newlines in
      (* handle leading blanks (output other lines right now, whitespace in
         front of the current token and on the same line is handled later) *)
      let blank, usr =
        let rec indent_between line blanks usr = match blanks with
          | [] -> assert false
          | bl::[] -> bl, usr |> pr_nl output block
          | bl::blanks ->
              usr
              |> pr_nl output block
              |> print_indent output line bl ~kind:Empty block
              |> indent_between (line + 1) blanks
        in
        let blanks = string_split '\n' (Lazy.force t.between) in
        match blanks with
        | [] -> assert false
        | bl::[] -> bl, usr (* no newline *)
        | bl::blanks ->
            usr
            |> (if last_line = 0 then print_indent output 1 "" ~kind:Empty block
                else fun usr -> usr)
            |> pr_whitespace output block bl
            |> indent_between (last_line + 1) blanks
      in
      (* Compute block and indent *)
      let block = IndentBlock.update output.config block stream t in
      (* Update block according to the indent in the file if before the
         handled region *)
      let block =
        if output.adaptive && not (output.in_lines line)
        then IndentBlock.reverse block
        else block
      in
      if output.debug then IndentBlock.dump block;
      (* Handle token *)
      let at_line_start = t.newlines > 0 in
      let usr =
        if at_line_start then
          let kind = match t.token with
            | COMMENT when is_prefix "(*\n" (Lazy.force t.substr) ->
                Fixed (String.length blank)
            | OCAMLDOC_VERB -> Padded
            | EOF -> Empty
            | COMMENTCONT when (Lazy.force t.substr <> "*)") -> Padded
            | _ -> Normal
          in
          usr
          |> print_indent output line blank ~kind block
        else
          usr
          |> pr_whitespace output block blank
      in
      let usr = usr |> print_token output block t in
      match t.token with EOF -> usr
                       | _ -> usr |> loop output block stream

let proceed output stream block usr =
  usr |> loop output block stream
