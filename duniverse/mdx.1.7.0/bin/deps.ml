(*
 * Copyright (c) 2020 Ulysse Gérard <ulysse@tarides.com>
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

open Mdx.Util.Result.Infix

let run (`Setup ()) (`Syntax syntax) (`File file) =
  let syntax =
    match (syntax, Mdx.Syntax.infer ~file) with
    | Some s, _ | None, Some s -> s
    | None, None ->
        Printf.eprintf
          "[mdx] Fatal error: could not infer syntax from filename %s, use the \
           --syntax option to specify a syntax.\n"
          file;
        exit 1
  in
  Mdx.parse_file syntax file >>! fun doc ->
  let deps = Mdx.Dep.of_lines doc in
  let deps = List.map Mdx.Dep.to_sexp deps in
  Printf.printf "%s" (Mdx.Util.Csexp.to_string (List deps));
  0

let cmd =
  let open Cmdliner in
  let doc =
    "List the dependencies of the input file. This command is meant to be used \
     by dune only. There are no stability guarantees."
  in
  (Term.(pure run $ Cli.setup $ Cli.syntax $ Cli.file), Term.info "deps" ~doc)
