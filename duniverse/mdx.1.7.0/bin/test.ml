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

let src = Logs.Src.create "cram.test"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) x y = match x with "." -> y | _ -> Filename.concat x y

let split_exe_extension path =
  let exe = ".exe" in
  if Filename.check_suffix path exe then (Filename.chop_extension path, exe)
  else (path, "")

let run (`Setup ()) _ _ _ _ _ _ _ _ _ _ =
  let base = Filename.basename Sys.argv.(0) in
  let dir = Filename.dirname Sys.argv.(0) in
  let cmd =
    match base with
    | "main.exe" -> dir / "test" / "main.exe"
    | x when String.length x > 6 && String.sub x 0 6 = "ocaml-" ->
        let x_without_ext, x_ext = split_exe_extension x in
        (dir / x_without_ext) ^ "-test" ^ x_ext
    | x ->
        let x_without_ext, x_ext = split_exe_extension x in
        (dir / "ocaml-") ^ x_without_ext ^ "-test" ^ x_ext
  in
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  argv.(0) <- cmd;
  Log.debug (fun l -> l "executing %a" Fmt.(Dump.array string) argv);
  Unix.execvp cmd argv

open Cmdliner

let cmd : int Term.t * Term.info =
  let doc = "Test markdown files." in
  ( Term.(
      pure run $ Cli.setup $ Cli.non_deterministic $ Cli.silent_eval
      $ Cli.syntax $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude
      $ Cli.prelude_str $ Cli.file $ Cli.section $ Cli.root $ Cli.force_output
      $ Cli.output),
    Term.info "test" ~doc )
