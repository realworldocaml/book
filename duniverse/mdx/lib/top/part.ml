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

open Mdx.Migrate_ast
open Mdx.Compat

module Part = struct

  type t =
    { name: string;
      body: string; }

  let v ~name ~body = { name; body }
  let name {name;_} = name
  let body {body;_} = body

end

module Lexbuf = struct

  open Lexing

  type t = {
    contents: string;
    lexbuf  : lexbuf;
  }

  let initial_pos name = {
    pos_fname = name;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0;
  }

  let v ~fname contents =
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- initial_pos fname;
    Location.input_name := fname;
    { contents; lexbuf }

  let of_file fname =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    v ~fname result

end

module Phrase = struct

  open Lexing
  open Parsetree

  type kind = Code | Part of string

  exception Cannot_parse_payload of Location.t

  let string_of_location
      {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum};_}
    =
    Printf.sprintf "%s, line %d, col %d" pos_fname pos_lnum (pos_cnum - pos_bol)

  let payload_constants loc = function
    | PStr [{pstr_desc = Pstr_eval (expr, _); _}] ->
      let one {pexp_loc; pexp_desc; _} = match pexp_desc with
        | Pexp_apply ({pexp_desc = Pexp_ident ident; _},
                      [Asttypes.Nolabel, {pexp_desc = Pexp_constant const; _}]) ->
          (pexp_loc, Some ident, const)
        | Pexp_constant const -> (pexp_loc, None, const)
        | _ -> raise (Cannot_parse_payload pexp_loc)
      in
      let rec consts = function
        | {pexp_desc=Pexp_sequence(e, rest); _} -> one e :: consts rest
        | e -> [one e]
      in
      consts expr
    | PStr [] -> []
    | _ -> raise (Cannot_parse_payload loc)

  let payload_strings loc = function
    | PStr [] -> []
    | x ->
      let aux = function
        | _, Some {Location.txt = Longident.Lident "ocaml"; _},
          Pconst_string (str, _) -> (`OCaml, str)
        | _, None, Pconst_string (str, _) -> (`Raw, str)
        | loc, _, _ -> raise (Cannot_parse_payload loc)
      in
      List.map aux (payload_constants loc x)

  let kind_impl = function
    | {pstr_desc = Pstr_attribute (name, payload); pstr_loc}
      when name.Asttypes.txt = "part" ->
      begin match payload_strings pstr_loc payload with
        | [`Raw, part] -> Part part
        | _ ->
          prerr_endline
            (string_of_location pstr_loc ^ ": cannot parse [@@@part] payload");
          Code
        | exception (Cannot_parse_payload loc) ->
          prerr_endline
            (string_of_location loc ^ ": cannot parse [@@@part] payload");
          Code
      end
    | _ -> Code

    let kind_intf = function
    | {psig_desc = Psig_attribute (name, payload); psig_loc}
      when name.Asttypes.txt = "part" ->
      begin match payload_strings psig_loc payload with
        | [`Raw, part] -> Part part
        | _ ->
          prerr_endline
            (string_of_location psig_loc ^ ": cannot parse [@@@part] payload");
          Code
        | exception (Cannot_parse_payload loc) ->
          prerr_endline
            (string_of_location loc ^ ": cannot parse [@@@part] payload");
          Code
      end
    | _ -> Code


  (* by default, [structure_item] locations do not contain the [;;] token,
     so here we try to extend the location when this is needed. *)
  let shift_semi_semi doc loc =
    let str = doc.Lexbuf.contents in
    let stop = loc.pos_cnum in
    let rec aux n =
      if n+1 >= String.length str then loc
      else match str.[n], str.[n+1] with
        | '\n', _  -> aux (n+1)
        | ';', ';' -> { loc with pos_cnum = n + 2 }
        | _, _ -> loc
    in
    aux stop

  let body_impl doc s =
    let start = match s with
      | s::_ -> Some s.pstr_loc.loc_start.pos_cnum
      | _    -> None
    in
    let stop = match List.rev s with
      | s::_ -> Some (shift_semi_semi doc s.pstr_loc.loc_end).pos_cnum
      | _    -> None
    in
    match start, stop with
    | Some start, Some stop ->
      String.sub doc.Lexbuf.contents start (stop - start)
    | _ -> ""

  let body_intf doc s =
    let start = match s with
      | s::_ -> Some s.psig_loc.loc_start.pos_cnum
      | _    -> None
    in
    let stop = match List.rev s with
      | s::_ -> Some (shift_semi_semi doc s.psig_loc.loc_end).pos_cnum
      | _    -> None
    in
    match start, stop with
    | Some start, Some stop ->
      String.sub doc.Lexbuf.contents start (stop - start)
    | _ -> ""

  let parts ~body doc phrases =
    let rec aux parts part strs = function
      | (s, Code) :: rest -> aux parts part (s :: strs) rest
      | (_, Part name) :: rest ->
        let body = body doc (List.rev strs) in
        let parts = Part.v ~name:part ~body :: parts in
        aux parts name [] rest
      | [] ->
        let parts =
          if part <> "" || strs <> [] then
            let body = body doc (List.rev strs) in
            Part.v ~name:part ~body :: parts
          else
            if List.length parts = 0 then
              [Part.v ~name:"" ~body:""]
            else
              parts
        in
        List.rev parts
    in
    aux [] "" [] phrases

  let handle_syntax_error e =
#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 8
      (* The function is now Parse.prepare_error, but it is not
         exposed; luckily enough, it is register to print the
         exception. *)
      Fmt.failwith "Cannot parse: %s" (Printexc.to_string (Syntaxerr.Error e))
#else
      Fmt.failwith "Cannot parse: %a" Syntaxerr.report_error e
#endif

  let read_impl doc =
    try
      let strs = Parse.implementation doc.Lexbuf.lexbuf in
      List.map (fun x -> x, kind_impl x) strs
    with Syntaxerr.Error e ->
      handle_syntax_error e

  let read_intf doc =
    try
      let strs = Parse.interface doc.Lexbuf.lexbuf in
      List.map (fun x -> x, kind_intf x) strs
    with Syntaxerr.Error e ->
      handle_syntax_error e

end

type file =
  | Parts of Part.t list
  | Body of (exn * string)

let read_impl lexbuf =
  Phrase.(parts ~body:body_impl lexbuf (read_impl lexbuf))

let read_intf lexbuf =
  Phrase.(parts ~body:body_intf lexbuf (read_intf lexbuf))

let read file =
  let lexbuf = Lexbuf.of_file file in
  let read = match Filename.extension file with
    | ".ml"  -> read_impl
    | ".mli" -> read_intf
    | s      -> Fmt.failwith "unknown extension: %s" s
  in
  try
    lexbuf
    |> read
    |> fun x -> Parts x
  with e ->
    Body (e, lexbuf.Lexbuf.contents)

let err_parse_error (e, _) =
  Fmt.failwith "Parse error: %a" Fmt.exn e

let find file ~part = match file, part with
  | Body (_, s), None      -> Some [s]
  | Body b, _ -> err_parse_error b
  | Parts parts, Some part ->
    (match List.find_opt (fun p -> String.equal (Part.name p) part) parts with
     | Some p -> Some [Part.body p]
     | None   -> None )
  | Parts parts, None      ->
    List.fold_left (fun acc p -> Part.body p :: [""] @ acc) [] parts
    |> List.rev
    |> fun x -> Some x

let replace file ~part ~lines = match file, part with
  | Body (e, _), None -> Body (e, String.concat "\n" lines)
  | Body b     , _    -> err_parse_error b
  | Parts parts, _    ->
    let part = match part with None -> "" | Some p -> p in
    List.map (fun p ->
        let name = Part.name p in
        if String.equal name part then
          { p with body = String.concat "\n" lines }
        else
          p
      ) parts
    |> fun x -> Parts x

let contents = function
  | Body (_, s) -> String.trim s ^ "\n"
  | Parts parts ->
    let lines =
      List.fold_left (fun acc p ->
          let body =  Part.body p in
          match Part.name p with
          | "" -> body :: acc
          | n  -> body :: ("\n[@@@part \"" ^ n ^ "\"] ;;\n") :: acc
        ) [] parts
    in
    let lines = List.rev lines in
    let lines = String.concat "\n" lines in
    String.trim lines ^ "\n"
