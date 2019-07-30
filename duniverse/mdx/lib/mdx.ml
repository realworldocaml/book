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
module Log = (val Logs.src_log src : Logs.LOG)

module Output = Output
module Cram = Cram
module Toplevel = Toplevel
module Library = Library
module Block = Block
module Migrate_ast = Migrate_ast
module Compat = Compat
module Util = Util
module Prelude = Prelude

type line =
  | Section of (int * string)
  | Text    of string
  | Block   of Block.t

type t = line list

let dump_line ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "Block %a" Block.dump b
  | Section (d, s) -> Fmt.pf ppf "Section (%d, %S)" d s
  | Text s         -> Fmt.pf ppf "Text %S" s

let dump = Fmt.Dump.list dump_line

let pp_line ?syntax ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "%a\n" (Block.pp ?syntax) b
  | Section (d, s) -> Fmt.pf ppf "%s %s\n" (String.make d '#') s
  | Text s         -> Fmt.pf ppf "%s\n" s

let pp ?syntax ppf t =
  Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") (pp_line ?syntax)) t

let to_string = Fmt.to_to_string pp

let section_of_line = function
  | Section s -> Some s
  | Text _    -> None
  | Block b   -> b.section

let filter_section re (t: t) =
  match
    List.filter (fun l -> match section_of_line l with
        | None        -> false
        | Some (_, s) -> Re.execp re s
      )  t
  with
  | [] -> None
  | l  -> Some l

let parse l =
  List.map (function
      | `Text t -> Text t
      | `Section s -> Section s
      | `Block b   -> Block b
    ) l

type syntax = Syntax.t =
  | Normal
  | Cram

let parse_lexbuf syntax l = parse (Lexer.token syntax l)
let parse_file syntax f = parse (Lexer.token syntax (snd (Misc.init f)))
let of_string syntax s = parse_lexbuf syntax (Lexing.from_string s)

let eval = function
  | Section _ | Text _ as x -> x
  | Block t as x ->
    let t' = Block.eval t in
    if t == t' then x else Block t'

let run ?(syntax=Normal) ?(force_output=false) ~f n =
  Misc.run_expect_test ~force_output n ~f:(fun c l ->
      let items = parse_lexbuf syntax l in
      let items = List.map eval items in
      Log.debug (fun l -> l "run @[%a@]" dump items);
      f c items
    )
