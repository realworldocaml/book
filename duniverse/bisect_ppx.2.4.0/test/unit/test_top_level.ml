(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



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
         "-ppx '" ^ dune_build_directory ^
          "/install/default/lib/bisect_ppx/ppx.exe --as-ppx' " ^
         "-stdin > /dev/null");
    run "! ls bisect0001.coverage 2> /dev/null")
]
