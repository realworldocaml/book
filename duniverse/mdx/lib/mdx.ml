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

let src = Logs.Src.create "ocaml-mdx"

module Lexer_mdx = Lexer_mdx

module Log = (val Logs.src_log src : Logs.LOG)

module Output = Output
module Cram = Cram
module Deprecated = Deprecated
module Document = Document
module Toplevel = Toplevel
module Library = Library
module Ocaml_delimiter = Ocaml_delimiter
module Part = Part
module Block = Block
module Migrate_ast = Migrate_ast
module Mli_parser = Mli_parser
module Compat = Compat
module Util = Util
module Prelude = Prelude
module Syntax = Syntax
module Label = Label
module Dep = Dep
module Ocaml_env = Ocaml_env
include Document
open Util.Result.Infix

let section_of_line = function
  | Section s -> Some s
  | Text _ -> None
  | Block b -> b.section

let filter_section re (t : t) =
  match
    List.filter
      (fun l ->
        match section_of_line l with
        | None -> false
        | Some (_, s) -> Re.execp re s)
      t
  with
  | [] -> None
  | l -> Some l

let parse l =
  List.map
    (function
      | `Text t -> Text t | `Section s -> Section s | `Block b -> Block b)
    l

let parse_lexbuf file_contents syntax l =
  match syntax with
  | Syntax.Mli -> Mli_parser.parse_mli file_contents
  | Normal -> Lexer_mdx.markdown_token l >>| parse
  | Cram -> Lexer_mdx.cram_token l >>| parse

let parse_file syntax f =
  let l = snd (Misc.init f) in
  parse_lexbuf f syntax l

let of_string syntax s =
  match syntax with
  | Syntax.Mli -> Mli_parser.parse_mli s
  | Syntax.Normal | Syntax.Cram -> parse_lexbuf s syntax (Lexing.from_string s)

let dump_line ppf (l : line) =
  match l with
  | Block b -> Fmt.pf ppf "Block %a" Block.dump b
  | Section (d, s) -> Fmt.pf ppf "Section (%d, %S)" d s
  | Text s -> Fmt.pf ppf "Text %S" s

let dump = Fmt.Dump.list dump_line

type expect_result = Identical | Differs

let run_str ~syntax ~f file =
  let file_contents, lexbuf = Misc.init file in
  parse_lexbuf file_contents syntax lexbuf >>| fun items ->
  Log.debug (fun l -> l "run @[%a@]" dump items);
  let corrected = f file_contents items in
  let has_changed = corrected <> file_contents in
  let result = if has_changed then Differs else Identical in
  (result, corrected)

let write_file ~outfile content =
  let oc = open_out_bin outfile in
  output_string oc content;
  close_out oc

let run_to_stdout ?(syntax = Normal) ~f infile =
  run_str ~syntax ~f infile >>| fun (_, corrected) -> print_string corrected

let run_to_file ?(syntax = Normal) ~f ~outfile infile =
  run_str ~syntax ~f infile >>| fun (_, corrected) ->
  write_file ~outfile corrected

let run ?(syntax = Normal) ?(force_output = false) ~f infile =
  let outfile = infile ^ ".corrected" in
  run_str ~syntax ~f infile >>| fun (test_result, corrected) ->
  match (force_output, test_result) with
  | true, _ | false, Differs -> write_file ~outfile corrected
  | false, Identical -> if Sys.file_exists outfile then Sys.remove outfile
