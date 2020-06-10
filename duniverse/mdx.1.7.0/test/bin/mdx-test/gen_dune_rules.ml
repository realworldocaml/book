(*
 * Copyright (c) 2019 Craig Ferguson <me@craigfe.io>
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
open Gen_rule_helpers

(** Tests that the result of 'cd <dir_name> && ocaml-mdx test [options] test-case.md'
    is equal to '<dir_name>/test-case.md.expected' if it exists or to
    '<dir>/test-case.md' otherwise. *)
let pp_expect_action fmt dir =
  Fmt.pf fmt
    {|
  (with-stdout-to %%{target}
   (chdir %s
    (run ocaml-mdx test --output - %a%s)))|}
    dir.dir_name pp_options dir.options dir.test_file

(** Tests that 'cd <dir> && ocaml-mdx test [options] <file>' exits with a
    failing code and that its output is equal to the content of the
    '.expected' file in <dir>. *)
let pp_failure_action fmt dir =
  Fmt.pf fmt
    {|
  (with-outputs-to %%{target}
   (chdir %s
    (system "! ocaml-mdx test %a%s")))|}
    dir.dir_name pp_options dir.options dir.test_file

let () = run { pp_expect_action; pp_failure_action }
