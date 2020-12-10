(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

let tests = "missing-files" >::: [
  test "without-flag" begin fun () ->
    run "echo 'let () = ()' > source.ml";
    compile (with_bisect () ^ " -package findlib.dynload") "_scratch/source.ml";
    run "./a.out";
    report "html -o report" ~r:"> /dev/null 2> /dev/null || touch failed";
    run "[ -f failed ]"
  end;

  test "with-flag" begin fun () ->
    run "echo 'let () = ()' > source.ml";
    compile (with_bisect () ^ " -package findlib.dynload") "_scratch/source.ml";
    run "./a.out";
    report "html --ignore-missing-files -o report"
  end
]
