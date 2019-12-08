(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open Test_helpers

let tests =
  test "ounit-integration" begin fun () ->
    compile ((with_bisect ()) ^ " -package oUnit")
      "fixtures/ounit-integration/test.ml";
    run "./a.out > /dev/null";
    report "-csv output";
    diff "fixtures/ounit-integration/reference.csv"
  end
