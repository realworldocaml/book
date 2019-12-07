(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

let tests = "top-level" >::: [
  test "batch" (fun () ->
    compile
      ((with_bisect ()) ^ " -dsource")
      "fixtures/top-level/source.ml" ~r:"2> output";
    diff_ast "fixtures/top-level/batch.reference.ml");

  test "stdin" (fun () ->
    run ("cat ../fixtures/top-level/source.ml | ocaml " ^
         "-ppx '../../../../install/default/lib/bisect_ppx/ppx.exe --as-ppx' " ^
         "-stdin > /dev/null");
    run "! ls bisect0001.out 2> /dev/null")
]
