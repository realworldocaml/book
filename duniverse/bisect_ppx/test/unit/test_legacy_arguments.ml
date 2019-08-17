(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

let tests = "legacy-arguments" >::: [
  test "modes" begin fun () ->
    run "echo 'let () = ()' > source.ml";
    compile (with_bisect_args "-mode safe") "_scratch/source.ml";
    compile (with_bisect_args "-mode fast") "_scratch/source.ml";
    compile (with_bisect_args "-mode faster") "_scratch/source.ml"
  end
]
