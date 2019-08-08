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
      body: string; }

  let v ~name ~body = { name; body }
  let name {name;_} = name
  let body {body;_} = body

end

(** Remove empty strings at the beginning of a list *)
let rec remove_empty_heads = function
  | "" :: tl -> remove_empty_heads tl
  | l -> l

module Parse_parts =
struct

  let part_statement_re =
    let open Re in
    let ws = rep space in
    compile @@ whole_string @@ seq [
      ws; str "[@@@"; ws; str "part"; ws;
      str "\""; group (rep1 any); str "\"";
      ws; str "]"; ws; opt (str ";;"); ws;
    ]

  let make_part ~name ~lines =
    let body = String.concat "\n" (List.rev (remove_empty_heads lines)) in
    Part.v ~name ~body

  let rec parse_parts input name lines =
    match input_line input with
    | exception End_of_file -> [make_part ~name ~lines]
    | line ->
      match Re.exec_opt part_statement_re line with
      | None -> parse_parts input name (line :: lines)
      | Some groups ->
        let part = make_part ~name ~lines in
        let new_name = Re.Group.get groups 1 in
        part :: parse_parts input new_name []

  let of_file name =
    let input = open_in name in
    parse_parts input "" []

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
    [{ name = part_name; body }]

let replace file ~part ~lines =
  let part = match part with None -> "" | Some p -> p in
  replace_or_append part (String.concat "\n" lines) file

let contents file =
  let lines =
    List.fold_left (fun acc p ->
        let body =  Part.body p in
        match Part.name p with
        | "" -> body :: acc
        | n  -> body :: ("\n[@@@part \"" ^ n ^ "\"] ;;\n") :: acc
      ) [] file
  in
  let lines = List.rev lines in
  let lines = String.concat "\n" lines in
  String.trim lines ^ "\n"
