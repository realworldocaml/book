(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

let tests = "mli" >::: [
  test "mli" begin fun () ->
    compile
      ((with_bisect ()) ^ " -dsource") "fixtures/mli/source.mli" ~r:"2> output";
    diff_ast "fixtures/mli/source.reference.mli"
  end;
]
