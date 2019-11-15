open Core
open Async

let (/) = Filename.concat

type part = {
  title   : string;
  chapters: string list;
} [@@deriving sexp]

type t = [ `part of part | `chapter of string] list [@@deriving sexp]

let read dir =
  let f = dir / "toc.scm" in
  Reader.file_contents f >>| fun contents ->
  let s = Sexplib.Sexp.scan_sexps (Lexing.from_string contents) in
  t_of_sexp (Sexplib.Sexp.List s)

let get ?(repo_root=".") () =
  let book_dir = repo_root/"book" in
  read book_dir
