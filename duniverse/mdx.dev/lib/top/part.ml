(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Mdx.Compat

module Part = struct

  type t =
    { name: string;
      sep_indent: string; (** Whitespaces before the [@@@part] separator *)
      body: string; }

  let v ~name ~sep_indent ~body = { name; sep_indent; body }
  let name {name;_} = name
  let sep_indent {sep_indent;_} = sep_indent
  let body {body;_} = body

end

(** Remove empty strings at the beginning of a list *)
let rec remove_empty_heads = function
  | "" :: tl -> remove_empty_heads tl
  | l -> l

let trim_empty_rev l =
  remove_empty_heads (List.rev (remove_empty_heads l))

module Parse_parts =
struct

  let part_statement_re =
    let open Re in
    let ws = rep space in
    compile @@ whole_string @@ seq [
      group ws; str "[@@@"; ws; str "part"; ws;
      str "\""; group (rep1 any); str "\"";
      ws; str "]"; ws; opt (str ";;"); ws;
    ]

  let next_part ~name ~sep_indent = fun lines_rev ->
    let body = String.concat "\n" (trim_empty_rev lines_rev) in
    Part.v ~name ~sep_indent ~body

  let next_part_of_groups groups =
    let sep_indent = Re.Group.get groups 1 in
    let name = Re.Group.get groups 2 in
    next_part ~name ~sep_indent

  let rec parse_parts input make_part lines =
    match input_line input with
    | exception End_of_file -> [make_part lines]
    | line ->
      match Re.exec_opt part_statement_re line with
      | None -> parse_parts input make_part (line :: lines)
      | Some groups ->
        let next_part = next_part_of_groups groups in
        make_part lines :: parse_parts input next_part []

  let of_file name =
    let input = open_in name in
    parse_parts input (next_part ~name:"" ~sep_indent:"") []

end

type file = Part.t list

let read file = Parse_parts.of_file file

let find file ~part = match part with
  | Some part ->
    (match List.find_opt (fun p -> String.equal (Part.name p) part) file with
     | Some p -> Some [Part.body p]
     | None   -> None )
  | None      ->
    List.fold_left (fun acc p -> Part.body p :: [""] @ acc) [] file
    |> List.rev
    |> fun x -> Some x

let rec replace_or_append part_name body = function
  | p :: tl when String.equal (Part.name p) part_name ->
    { p with body } :: tl
  | p :: tl ->
    p :: replace_or_append part_name body tl
  | [] ->
    [{ name = part_name; sep_indent = ""; body }]

let replace file ~part ~lines =
  let part = match part with None -> "" | Some p -> p in
  replace_or_append part (String.concat "\n" lines) file

let contents file =
  let lines =
    List.fold_left (fun acc p ->
        let body =  Part.body p in
        match Part.name p with
        | "" -> body :: acc
        | n  ->
          let indent = Part.sep_indent p in
          body :: ("\n" ^ indent ^ "[@@@part \"" ^ n ^ "\"] ;;\n") :: acc
      ) [] file
  in
  let lines = List.rev lines in
  let lines = String.concat "\n" lines in
  String.trim lines ^ "\n"
