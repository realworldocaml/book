(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Unsigned


(*
  Test marshalling and unmarshalling custom integers
*)
let test_integer_marshalling _ =
  let v = (
    UInt8.zero, UInt16.zero, UInt32.zero, UInt64.zero,
    UInt8.one, UInt16.one, UInt32.one, UInt64.one,
    UInt8.of_string "100", UInt16.of_string "1000",
    UInt32.of_string "10000", UInt64.of_string "100000",
    UInt8.max_int, UInt16.max_int, UInt32.max_int, UInt64.max_int
  ) in
  assert_equal v Marshal.(from_string (to_string v []) 0)


let suite = "Marshal tests" >:::
  ["integer marshalling"
    >:: test_integer_marshalling;
  ]


let _ =
  run_test_tt_main suite
