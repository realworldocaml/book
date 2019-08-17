(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

(* OCaml 4.02 and 4.03 order output of warnings 12 and 28 differently. To get
   around that, these tests sort the output lines. *)
let sorted_diff () =
  run "sort < output.raw > output";
  run "sort < ../fixtures/warnings/source.reference.ml > reference";
  diff ~preserve_as:"warnings/source.reference.ml" "_scratch/reference"

let tests = "warnings" >::: [
  test "default" begin fun () ->
    compile
      ((with_bisect ()) ^ " -w +A")
      "fixtures/warnings/source.ml" ~r:"2> output.raw";
    sorted_diff ()
  end;
]
