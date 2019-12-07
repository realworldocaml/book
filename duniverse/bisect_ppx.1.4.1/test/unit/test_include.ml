(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



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
