(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



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
