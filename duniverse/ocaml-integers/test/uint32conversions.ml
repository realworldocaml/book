(*
 * Copyright (c) 2021 Nomadic Labs
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let () = print_endline "UInt32-Int32 conversion test: ?"

(* Asserting that signed->unsigned conversion is trivial when within range *)
let () =
  let check i32 =
     let s32 = Int32.to_string i32 in
     Unsigned.UInt32.(equal (of_int32 i32) (of_string s32))
  in
  assert (check 0l);
  assert (check 1l);
  assert (check 23l);
  assert (check 112783646l);
  assert (check Int32.max_int)

(* Asserting that unsinged->signed conversion is trivial when within range *)
let () =
   let check u32 =
      let s32 = Unsigned.UInt32.to_string u32 in
      Int32.equal (Unsigned.UInt32.to_int32 u32) (Int32.of_string s32)
   in
   assert (check Unsigned.UInt32.zero);
   assert (check Unsigned.UInt32.one);
   assert (check (Unsigned.UInt32.of_string "23"));
   assert (check (Unsigned.UInt32.of_string "112783646"));
   assert (check (Unsigned.UInt32.of_string "2147483647"));
   ()

(* Asserting that signed->unsigned->signed roundtrips *)
let () =
  assert Unsigned.UInt32.(to_int32 (of_int32 23l) = 23l);
  assert Unsigned.UInt32.(to_int32 (of_int32 0l) = 0l);
  assert Unsigned.UInt32.(to_int32 (of_int32 (-1l)) = -1l);
  assert Unsigned.UInt32.(to_int32 (of_int32 (-23l)) = -23l);
  assert Unsigned.UInt32.(to_int32 (of_int32 Int32.max_int) = Int32.max_int);
  assert Unsigned.UInt32.(to_int32 (of_int32 Int32.min_int) = Int32.min_int);
  ()

(* Asserting that unsigned->signed->unsigned roundtrips *)
let () =
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 (of_string "0"))) (of_string "0"));
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 (of_string "23"))) (of_string "23"));
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 (of_string "2147483647"))) (of_string "2147483647"));
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 (of_string "2147483648"))) (of_string "2147483648"));
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 (of_string "2147493648"))) (of_string "2147493648"));
  assert Unsigned.UInt32.(equal (of_int32 (to_int32 max_int)) max_int);
  ()

let () = print_endline "UInt32-Int32 conversion test: ✓"
