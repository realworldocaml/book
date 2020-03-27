(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

(* If opening A in b.ml shadows ___bisect_visit___, running a.out will fail with
   Index out of bounds. *)
let tests = "multiple-modules-open" >::: [
  test "open" begin fun () ->
    compile ((with_bisect ()) ^ " -c") "fixtures/open/a.ml";
    compile ((with_bisect ()) ^ " a.cm[ox]") "fixtures/open/b.ml";
    run "./a.out"
  end;
]
