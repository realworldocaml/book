(*
 * Copyright (c) 2021 Nomadic Labs
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let () = print_endline "UInt64-Int64 conversion test: ?"

(* Asserting that signed->unsigned conversion is trivial when within range *)
let () =
  let check i64 =
     let s64 = Int64.to_string i64 in
     Unsigned.UInt64.(equal (of_int64 i64) (of_string s64))
  in
  assert (check 0L);
  assert (check 1L);
  assert (check 23L);
  assert (check 709182743098L);
  assert (check Int64.max_int)

(* Asserting that unsinged->signed conversion is trivial when within range *)
let () =
   let check u64 =
      let s64 = Unsigned.UInt64.to_string u64 in
      Int64.equal (Unsigned.UInt64.to_int64 u64) (Int64.of_string s64)
   in
   assert (check Unsigned.UInt64.zero);
   assert (check Unsigned.UInt64.one);
   assert (check (Unsigned.UInt64.of_string "23"));
   assert (check (Unsigned.UInt64.of_string "709182743098"));
   assert (check (Unsigned.UInt64.of_string "9223372036854775807"));
   ()

(* Asserting that signed->unsigned->signed roundtrips *)
let () =
  assert Unsigned.UInt64.(to_int64 (of_int64 23L) = 23L);
  assert Unsigned.UInt64.(to_int64 (of_int64 0L) = 0L);
  assert Unsigned.UInt64.(to_int64 (of_int64 (-1L)) = -1L);
  assert Unsigned.UInt64.(to_int64 (of_int64 (-23L)) = -23L);
  assert Unsigned.UInt64.(to_int64 (of_int64 Int64.max_int) = Int64.max_int);
  assert Unsigned.UInt64.(to_int64 (of_int64 Int64.min_int) = Int64.min_int);
  ()

(* Asserting that unsigned->signed->unsigned roundtrips *)
let () =
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 (of_string "0"))) (of_string "0"));
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 (of_string "23"))) (of_string "23"));
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 (of_string "9223372036854775807"))) (of_string "9223372036854775807"));
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 (of_string "9223372036854775808"))) (of_string "9223372036854775808"));
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 (of_string "9223372036954775808"))) (of_string "9223372036954775808"));
  assert Unsigned.UInt64.(equal (of_int64 (to_int64 max_int)) max_int);
  ()

let () = print_endline "UInt64-Int64 conversion test: ✓"
