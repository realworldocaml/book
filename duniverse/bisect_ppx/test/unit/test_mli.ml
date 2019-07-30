(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

let tests = "mli" >::: [
  test "mli" begin fun () ->
    compile
      ((with_bisect ()) ^ " -dsource") "fixtures/mli/source.mli" ~r:"2> output";
    diff_ast "fixtures/mli/source.reference.mli"
  end;
]
