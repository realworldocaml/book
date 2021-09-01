(*
 * Copyright (c) 2021 Nomadic Labs
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let () = print_endline "Hexstring test: ?"

let () =
  assert Signed.Int.(to_hexstring (of_string "0x23") = "23");
  assert Signed.Int32.(to_hexstring (of_string "0x23") = "23");
  assert Signed.Int64.(to_hexstring (of_string "0x23") = "23");
  assert Unsigned.UInt8.(to_hexstring (of_string "0x23") = "23");
  assert Unsigned.UInt16.(to_hexstring (of_string "0x23") = "23");
  assert Unsigned.UInt32.(to_hexstring (of_string "0x23") = "23");
  assert Unsigned.UInt64.(to_hexstring (of_string "0x23") = "23");
  ()

let () =
  assert Signed.Int32.(to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  assert Signed.Int64.(to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  assert Unsigned.UInt32.(to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  assert Unsigned.UInt64.(to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  ()

let () =
  assert Signed.Int.(to_hexstring (of_string "0x0") = "0");
  assert Signed.Int32.(to_hexstring (of_string "0x0") = "0");
  assert Signed.Int64.(to_hexstring (of_string "0x0") = "0");
  assert Unsigned.UInt8.(to_hexstring (of_string "0x0") = "0");
  assert Unsigned.UInt16.(to_hexstring (of_string "0x0") = "0");
  assert Unsigned.UInt32.(to_hexstring (of_string "0x0") = "0");
  assert Unsigned.UInt64.(to_hexstring (of_string "0x0") = "0");
  ()

let () = print_endline "Hexstring test: ✓"
