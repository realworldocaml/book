(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

(* If including A in b.ml causes a duplicate module name error, compilation of
   b.ml will fail. If including A in b.ml shadows ___bisect_visit___, running
   ./a.out will fail. *)
let tests = "include" >::: [
  test "include" begin fun () ->
    compile ((with_bisect ()) ^ " -c") "fixtures/include/a.ml";
    compile ((with_bisect ()) ^ " a.cm[ox]") "fixtures/include/b.ml";
    run "./a.out"
  end;
]
