(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

(* OCaml 4.02 and 4.03 order output of warnings 12 and 28 differently. To get
   around that, these tests sort the output lines. *)
let sorted_diff () =
  run "sort < output.raw > output";
  run "sort < ../fixtures/warnings/source.reference.ml > reference";
  let reference = Filename.concat (test_directory ()) "reference" in
  diff ~preserve_as:"warnings/source.reference.ml" reference

let tests = "warnings" >::: [
  test "default" begin fun () ->
    compile
      ((with_bisect ()) ^ " -w +A")
      "fixtures/warnings/source.ml" ~r:"2> output.raw";
    sorted_diff ()
  end;
]
