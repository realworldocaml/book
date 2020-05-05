(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Unsigned

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test retrieving max caml ints from C.
  *)
  let test_max_caml_int _ =
    assert_equal max_int (max_caml_int ())
      ~printer:string_of_int
end

(*
  Test UInt64.of_int.
*)
let test_uint64_of_int _ =
  begin
    assert_equal max_int (UInt64.to_int (UInt64.of_int max_int))
      ~printer:string_of_int
  end

(*
  Test storing and reading camlints.
*)
let test_store_caml_int _ =
  begin
    let p = allocate camlint max_int in
    assert_equal max_int !@p
      ~printer:string_of_int
  end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Integer tests" >:::
  ["UInt64.of_int"
    >:: test_uint64_of_int;

   "max_caml_int (foreign)"
   >:: Foreign_tests.test_max_caml_int;

   "max_caml_int (stubs)"
   >:: Stub_tests.test_max_caml_int;

   "storing camlint"
   >:: test_store_caml_int;
  ]



let _ =
  run_test_tt_main suite
