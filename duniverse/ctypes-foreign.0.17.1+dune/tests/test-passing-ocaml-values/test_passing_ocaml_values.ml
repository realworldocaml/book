(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign


let testlib = Dl.(dlopen ~filename:"../clib/dlltest_functions_stubs.so" ~flags:[RTLD_NOW])

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test passing OCaml strings directly to C.
  *)
  let test_passing_strings _ =
    let input = "abcdefghijklmnopqrstuvwxyz" in
    let len = String.length input in
    let buf = String.make len 'Z' in
    let _ = memcpy_string_string
      (ocaml_string_start buf)
      (ocaml_string_start input)
      (Unsigned.Size_t.of_int len)
    in begin
      assert_equal buf input
    end;

    let bbuf = Bytes.create len in
    let binput = Bytes.of_string input in
    let _ = memcpy_bytes_bytes
      (ocaml_bytes_start bbuf)
      (ocaml_bytes_start binput)
      (Unsigned.Size_t.of_int len)
    in begin
      assert_equal bbuf binput
    end;
    
    let arr = CArray.make char len in
    let () = String.iteri (CArray.set arr) input in
    let buf = String.make len 'Z' in
    let _ = memcpy_string_ptr
      (ocaml_string_start buf)
      (coerce (ptr char) (ptr void) (CArray.start arr))
      (Unsigned.Size_t.of_int len)
    in begin
      assert_equal buf input
    end


  (*
    Test pointer arithmetic on OCaml values.
   *)
  let test_pointer_arithmetic _ =
    let s = ocaml_string_start "abcdefghijklmnopqrstuvwxyz" in
    begin
      assert_equal s (s +@ 0);

      assert_equal (ptr_diff s (s +@ 10)) 10;

      assert_equal s ((s +@ 10) -@ 10);

      assert_equal
        (strdup (ocaml_string_start "klmnopqrstuvwxyz"))
        (strdup (s +@ 10))
    end
end


(*
  Test that OCaml values do not reside in addressable memory.
*)
let test_ocaml_types_rejected_as_pointer_reference_types _ =
  assert_raises IncompleteType
    (fun () -> allocate ocaml_string (ocaml_string_start ""))


(*
  Test that OCaml values cannot be used as return types.
*)
let strdup =
  if Sys.os_type = "Win32" then
    "_strdup"
  else
    "strdup"

let test_ocaml_types_rejected_as_return_types _ =
  assert_raises IncompleteType
    (fun () -> Foreign.foreign strdup (string @-> returning ocaml_string))


(*
  Test that pointers to OCaml values cannot be dereferenced.
*)
let test_pointers_to_ocaml_types_cannot_be_dereferenced _ =
  let p = allocate_n char ~count:10 in
  let po = coerce (ptr char) (ptr ocaml_string) p in

  begin
    assert_raises IncompleteType
      (fun () -> !@po);

    assert_raises IncompleteType
      (fun () -> po <-@ ocaml_string_start "");
  end


(*
  Test that [funptr] does not support ocaml_string return values.
*)
let test_no_higher_order_ocaml_string_support _ =
  begin
    assert_raises IncompleteType
      (fun () -> funptr (void @-> returning ocaml_string))
  end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Tests passing OCaml values" >:::
  ["passing strings (foreign)"
    >:: Foreign_tests.test_passing_strings;

   "passing strings (stubs)"
    >:: Stub_tests.test_passing_strings;

   "pointer arithmetic on OCaml values (foreign)"
    >:: Foreign_tests.test_pointer_arithmetic;

   "pointer arithmetic on OCaml values (stubs)"
    >:: Stub_tests.test_pointer_arithmetic;

   "ocaml_string values aren't addressable"
    >:: test_ocaml_types_rejected_as_pointer_reference_types;

   "ocaml_string can't be used as a return type"
    >:: test_ocaml_types_rejected_as_return_types;

   "pointers to ocaml_string values cannot be dereferenced"
    >:: test_pointers_to_ocaml_types_cannot_be_dereferenced;

   "no higher-order ocaml_string support"
    >:: test_no_higher_order_ocaml_string_support;
  ]



let _ =
  run_test_tt_main suite
