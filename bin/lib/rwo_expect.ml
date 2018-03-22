open Core
open Async

module Raw_script = struct
  type part =
    { name : string; content : string; }
    [@@deriving sexp]

  type t = part list
    [@@deriving sexp]

  let of_file ~filename =
    Monitor.try_with_or_error (fun () -> Reader.file_lines filename)
    >>|? fun lines ->
    let add_part parts name lines =
      { name ; content = String.concat ~sep:"\n" (List.rev lines) } :: parts
    in
    let is_part line =
      match List.map ~f:String.strip (String.split line ~on:'"') with
      | ["(* part"; name; "*)"]
      | ["[@@@part"; name; "]"]
      | ["[@@@part"; name; "];;"]
        -> Some name
      | _ -> None
    in
    let rec split_parts parts name lines = function
      | [] -> add_part parts name lines
      | line :: rest ->
        match is_part line with
        | None ->
          split_parts parts name (line :: lines) rest
        | Some name' ->
          let parts = add_part parts name lines in
          split_parts parts name' [] rest
    in
    split_parts [] "" [] lines

end

module Chunk = Ocaml_topexpect.Chunk
module Part = Ocaml_topexpect.Part

module Mlt = struct

  include Ocaml_topexpect.Document

  let of_file ~filename =
    Monitor.try_with_or_error (fun () -> Reader.file_contents filename)
    >>|? fun contents ->
    let lexbuf = Ocaml_topexpect.Lexbuf.v ~fname:filename contents in
    let phrases = Ocaml_topexpect.Phrase.read_all lexbuf in
    Ocaml_topexpect.Phrase.document lexbuf ~matched:true phrases

end

module Cram = struct

  type t = Cram.t [@@deriving sexp]
  let contents = Cram.to_string ~hide:true
  let part = Cram.part

  let of_file ~filename =
    Monitor.try_with_or_error (fun () -> Reader.file_contents filename)
    >>|? fun contents ->
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <-
      { pos_fname = filename
      ; pos_cnum  = 0
      ; pos_lnum  = 1
      ; pos_bol   = 0
      };
    Cram.parse_lexbuf lexbuf

end
