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

open Cmdliner

let cmds = [ Test.cmd; Pp.cmd; Rule.cmd; Deps.cmd ]

let main (`Setup ()) = `Help (`Pager, None)

let main =
  let doc = "Execute markdown files." in
  let exits = Term.default_exits in
  let man = [] in
  ( Term.(ret (const main $ Cli.setup)),
    Term.info "ocaml-mdx" ~version:"%%VERSION%%" ~doc ~exits ~man )

let main () = Term.(exit_status @@ eval_choice main cmds)

let main () =
  if String.compare Sys.argv.(0) "mdx" == 0 then
    Format.eprintf
      "\x1b[0;1mWarning\x1b[0m: 'mdx' is deprecated and will one day be removed.\n\
      \    Use 'ocaml-mdx' instead\n\
       %!";
  main ()

let () = main ()
