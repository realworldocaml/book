(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

(* Needed because of https://github.com/johnwhitington/ppx_blob/issues/1. *)
let _ppx_tools_workaround source destination =
  run ("cat " ^ source ^ " | grep -v '\\[WARNING\\]' > " ^ destination)

let tests = "ppx-integration" >::: [
  test "bisect_then_blob" begin fun () ->
    if_package "ppx_blob";

    compile ((with_bisect ()) ^ " -package ppx_blob -dsource")
      "fixtures/ppx-integration/blob.ml" ~r:"2> buggy_output";
    _ppx_tools_workaround "buggy_output" "output";
    diff_ast "fixtures/ppx-integration/bisect_then_blob.reference.ml"
  end;

  test "blob_then_bisect" begin fun () ->
    if_package "ppx_blob";

    compile ("-package ppx_blob " ^ (with_bisect ()) ^ " -dsource")
      "fixtures/ppx-integration/blob.ml" ~r:"2> buggy_output";
    _ppx_tools_workaround "buggy_output" "output";
    diff_ast "fixtures/ppx-integration/blob_then_bisect.reference.ml"
  end;

  test "bisect_then_deriving" begin fun () ->
    if_package "ppx_deriving";

    compile ((with_bisect ()) ^ " -package ppx_deriving.show -dsource")
      "fixtures/ppx-integration/deriving.ml" ~r:"2> output";
    normalize_source "output" "output";
    diff_ast "fixtures/ppx-integration/bisect_then_deriving.reference.ml"
  end;

  test "deriving_then_bisect" begin fun () ->
    if_package "ppx_deriving";

    compile ("-package ppx_deriving.show " ^ (with_bisect ()) ^ " -dsource")
      "fixtures/ppx-integration/deriving.ml" ~r:"2> output";
    normalize_source "output" "output";
    diff_ast "fixtures/ppx-integration/deriving_then_bisect.reference.ml"
  end;

  test "deriving_then_bisect_report" begin fun () ->
    if_package "ppx_deriving";

    compile ("-package ppx_deriving.show " ^ (with_bisect ()))
      "fixtures/ppx-integration/deriving.ml";
    run "./a.out > /dev/null";
    report "-text -" ~r:"| grep -v '<!--.*Bisect' > output";
    diff "fixtures/ppx-integration/deriving_then_bisect_report.reference.ml"
  end;

  test "attributes" begin fun () ->
    compile
      ((with_bisect ()) ^ " -dsource")
      "fixtures/ppx-integration/attributes.ml"
      ~r:"2> output";
    diff_ast "fixtures/ppx-integration/attributes.reference.ml"
  end;
]
