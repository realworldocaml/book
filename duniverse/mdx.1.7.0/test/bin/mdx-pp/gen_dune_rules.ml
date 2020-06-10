open Gen_rule_helpers

(** Tests that the results of 'cd <dir> && ocaml-mdx pp test-case.md' is equal
    to the content of the '.expected' file in <dir>. *)
let pp_expect_action fmt dir =
  Fmt.pf fmt
    {|
  (with-stdout-to %%{target}
   (chdir %s
    (run ocaml-mdx pp %a%s)))|}
    dir.dir_name pp_options dir.options dir.test_file

(** Tests that 'cd <dir> && ocaml-mdx pp [options] <file>' exits with a
    failing code and that its output is equal to the content of the
    '.expected' file in <dir>. *)
let pp_failure_action fmt dir =
  Fmt.pf fmt
    {|
  (with-outputs-to %%{target}
   (chdir %s
    (system "! ocaml-mdx pp %a%s")))|}
    dir.dir_name pp_options dir.options dir.test_file

let () = run { pp_expect_action; pp_failure_action }
