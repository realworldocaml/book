(*
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

(*
 *  Using the closure API of libffi is error prone due to differences
 *  in endianess and calling conventions.
 *)

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  let test x f = assert_equal x (f (fun () -> x))

  let test_signed_ints _ =
    test 127 callback_returns_int8_t;
    test (-128) callback_returns_int8_t;
    test (-1) callback_returns_int8_t;

    test 32767 callback_returns_int16_t;
    test (-32768) callback_returns_int16_t;
    test (-1) callback_returns_int16_t;

    test Int32.max_int callback_returns_int32_t;
    test Int32.min_int callback_returns_int32_t;
    test (Int32.of_int (-1)) callback_returns_int32_t;

    test Int64.max_int callback_returns_int64_t;
    test Int64.min_int callback_returns_int64_t;
    test (Int64.of_int (-1)) callback_returns_int64_t

  let test_unsigned_ints _ =
    test Unsigned.UInt8.max_int callback_returns_uint8_t;
    test Unsigned.UInt8.one callback_returns_uint8_t;

    test Unsigned.UInt16.max_int callback_returns_uint16_t;
    test Unsigned.UInt16.one callback_returns_uint16_t;

    test Unsigned.UInt32.max_int callback_returns_uint32_t;
    test Unsigned.UInt32.one callback_returns_uint32_t;

    test Unsigned.UInt64.max_int callback_returns_uint64_t;
    test Unsigned.UInt64.one callback_returns_uint64_t

  let float_cmp a b =
    if a = b then 0 else
    let abs_a = abs_float a in
    let abs_b = abs_float b in
    let diff = abs_float ( a -. b ) in
    let epsilon = 1.1e-7 in
    if diff /. (min (abs_a +. abs_b) max_float) < epsilon then 0
    else compare a b

  let test_float _ =
    let test x =
      assert_equal 0 (float_cmp x (callback_returns_float (fun () -> x ) ))
    in
    test 1.e7;
    test 1.e-3;
    test 3.e+38

  let test_double _ =
    let test x =
      assert_equal 0 (float_cmp x (callback_returns_double (fun () -> x ) ))
    in
    test 1.e7;
    test 1.e-3;
    test 1.e+307

  let test_bool _ =
    test true callback_returns_bool;
    test false callback_returns_bool

end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)

let suite = "Closure endianess tests" >:::
  ["test_signed_ints (foreign)"
   >:: Foreign_tests.test_signed_ints;

   "test_signed_ints (stubs)"
   >:: Stub_tests.test_signed_ints;

   "test_unsigned_ints (foreign)"
   >:: Foreign_tests.test_unsigned_ints;

   "test_unsigned_ints (stubs)"
   >:: Stub_tests.test_unsigned_ints;

   "test_float (foreign)"
   >:: Foreign_tests.test_float;

   "test_float (stubs)"
   >:: Stub_tests.test_float;

   "test_double (foreign)"
   >:: Foreign_tests.test_double;

   "test_double (stubs)"
   >:: Stub_tests.test_double;

   "test_bool (foreign)"
   >:: Foreign_tests.test_bool;

   "test_bool (stubs)"
   >:: Stub_tests.test_bool;
  ]

let _ =
  run_test_tt_main suite
