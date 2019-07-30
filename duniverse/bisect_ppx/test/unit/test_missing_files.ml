(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

let tests = "missing-files" >::: [
  test "without-flag" begin fun () ->
    run "echo 'let () = ()' > source.ml";
    compile (with_bisect () ^ " -package findlib.dynload") "_scratch/source.ml";
    run "./a.out";
    report "-html report" ~r:"2> /dev/null || touch failed";
    run "[ -f failed ]"
  end;

  test "with-flag" begin fun () ->
    run "echo 'let () = ()' > source.ml";
    compile (with_bisect () ^ " -package findlib.dynload") "_scratch/source.ml";
    run "./a.out";
    report "-ignore-missing-files -html report"
  end
]
