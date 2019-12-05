(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Tests for binding variadic functions. *)

open OUnit2
open Ctypes
[@@@warning "-6"]

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open Signed
  open Unsigned
  open M

  (*
    Test calling snprintf.
  *)
  let test_snprintf _ =
    let bufsz = 128 in
    let write snprintf apply =
      let buf = allocate_n char bufsz in
      let _ : int = apply (snprintf buf bufsz) in
      coerce (ptr char) string buf
    in
    begin
      assert_equal "an int: 100."
        (write snprintf_int
           (fun k -> k "an int: %d." 100));

      assert_equal "a char A and a uint 33."
        (write snprintf_char_unsigned
           (fun k -> k "a char %c and a uint %u." 'A' (UInt.of_int 33)));

      let ref_string =
        match Sys.word_size with
          | 32 -> "a long long 2147483647 and an int -4."
          | 64 -> "a long long 9223372036854775807 and an int -4."
          | n  -> failwith (Printf.sprintf "This test doesn't yet support word size %d" n)
      in
      let format_string =
        match Sys.os_type with
          | "Win32" -> "a long long %I64d and an int %d."
          | _ -> "a long long %lld and an int %d."
      in
      assert_equal ref_string
        (write snprintf_longlong_int
           (fun k -> k format_string
             (LLong.of_nativeint Nativeint.max_int) (-4)));

      assert_equal "a string abcde and an unsigned short ffd."
        (write snprintf_string_ushort
           (fun k -> k "a string %s and an unsigned short %hx."
             "abcde" (UShort.of_int 0xffd)));
    end
end


module Stub_tests = Common_tests(Generated_bindings)

let suite = "Variadic tests" >:::
  ["snprintf"
    >:: Stub_tests.test_snprintf;
  ]


let _ =
  run_test_tt_main suite
