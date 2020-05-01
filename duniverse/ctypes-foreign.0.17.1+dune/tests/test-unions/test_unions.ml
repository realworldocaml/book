(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Unsigned


let testlib = Dl.(dlopen ~filename:"../clib/dlltest_functions_stubs.so" ~flags:[RTLD_NOW])


(* 
   Check that using a union to inspect the representation of a float (double)
   value gives the same result as Int64.of_bits.

     union u {
       double  f;
       int64_t i;
     };
*)
let test_inspecting_float _ =
  let module M = struct
    type u
    let utyp : u union typ = union "u"
    let (-:) ty label = field utyp label ty
    let f = double  -: "f"
    let i = int64_t -: "i"
    let () = seal utyp

    let pi = 3.14
    let e = 2.718
    let u = make utyp
     
    (* Write through the double; read through the int64_t *)
    let () = setf u f pi
    let repr = getf u i
    let () = assert_equal (Int64.bits_of_float pi) repr

    (* Write through the int64_t; read through the double *)
    let () = setf u i (Int64.bits_of_float e)
    let e' = getf u f
    let () = assert_equal e e'

  end in ()


(* 
   Use a union with the following type to detect endianness

     union e {
       int64_t       i;
       unsigned char c[sizeof int64_t];
     };
*)
let test_endian_detection _ =
  let module M = struct
    type e
    let etyp : e union typ = union "e"
    let (-:) ty label = field etyp label ty
    let i = int64_t                      -: "i"
    let c = array (sizeof int64_t) uchar -: "c"
    let () = seal etyp

    let updated_char_index =
      if Sys.big_endian then  sizeof int64_t - 1 else 0

    let e = make etyp

    let () = setf e i 1L

    let arr = getf e c

    module Array = CArray
    let () = assert_equal
      ~msg:"the byte that we expected to change was changed"
      arr.(updated_char_index) UChar.one

    let () = for i = 1 to sizeof int64_t - 1 do
        if i <> updated_char_index then
          assert_equal ~msg:"only the top or the bottom byte was changed"
            UChar.zero arr.(i)
      done
  end in ()


module Build_foreign_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                               and type 'a return = 'a) =
struct
  open Functions
  module M = Common(S)
  open M
  (* Check that unions are tail-padded sufficiently to satisfy the alignment
     requirements of all their members.
  *)
  let test_union_padding _ =
    let module M = struct
      let mkPadded : int64 -> padded union =
        fun x ->
          let u = make padded in
          setf u i x;
          u

      let arr = CArray.of_list padded [
        mkPadded 1L;
        mkPadded 2L;
        mkPadded 3L;
        mkPadded 4L;
        mkPadded 5L;
      ]

      let sum = sum_union_components
        (CArray.start arr)
        (Unsigned.Size_t.of_int (CArray.length arr))

      let () = assert_equal
        ~msg:"padded union members accessed correctly"
        15L sum
        ~printer:Int64.to_string
    end in ()
end

module Build_stub_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                            and type 'a return = 'a) =
struct
  open Functions
  include Build_foreign_tests(S)
  module N = Functions.Stubs(S)
  open N

  (* Check that unions can be passed and returned by value.
  *)
  let test_passing_unions_by_value _ =
    let module M = struct
      let mkPadded : int64 -> padded union =
        fun x ->
          let u = make padded in
          setf u i x;
          u

      let u = add_unions (mkPadded 20L) (mkPadded 30L)

      let () = assert_equal
        ~msg:"unions passed by value"
        50L (getf u i)
        ~printer:Int64.to_string
    end in ()
end


module Build_struct_stub_tests
    (S : Ctypes.TYPE
          with type 'a typ = 'a Ctypes.typ
           and type ('a, 's) field = ('a, 's) Ctypes.field) =
struct
  module M = Types.Struct_stubs(S)

  let retrieve_size name = 
    let f = Foreign.foreign ~from:testlib name (void @-> returning size_t) in
    Unsigned.Size_t.to_int (f ())
  let sizeof_u1 = retrieve_size "sizeof_u1"
  let alignmentof_u1 = retrieve_size "alignmentof_u1"

  let sizeof_u2 = retrieve_size "sizeof_u2"
  let alignmentof_u2 = retrieve_size "alignmentof_u2"

  (*
    Test that union layout retrieved from C correctly accounts for missing
    fields.
  *)
  let test_missing_fields _ =
    begin
      assert_equal sizeof_u1
        (sizeof M.u1);

      assert_equal alignmentof_u1
        (alignment M.u1);
    end


  (* Test that we can retrieve information for unions without tags that are
     identified through typedefs, e.g.
         typedef union { int x; float y; } u;
   *)
  let test_tagless_unions _ =
    begin
      assert_equal sizeof_u2
        (sizeof M.u2);

      assert_equal alignmentof_u2
        (alignment M.u2);
    end
end


module Struct_stubs_tests = Build_struct_stub_tests(Generated_struct_bindings)

(* Check that the address of a union is equal to the addresses of each
   of its members.
*)
let test_union_address _ =
  let module M = struct
    type u
    let u : u union typ = union "u"
    let (-:) ty label = field u label ty
    let i = int64_t                      -: "i"
    let c = char                         -: "c"
    let s = ptr (structure "incomplete") -: "s"
    let () = seal u

    let up = addr (make u)

    let () = begin

      assert_equal
        (to_voidp up) (to_voidp (up |-> i));

      assert_equal
        (to_voidp up) (to_voidp (up |-> c));

      assert_equal
        (to_voidp up) (to_voidp (up |-> s));
    end
  end in ()


(*
  Test that attempting to update a sealed union is treated as an error.
*)
let test_updating_sealed_union _ =
  let utyp = union "sealed" in
  let _ = field utyp "_" int in
  let () = seal utyp in

  assert_raises (ModifyingSealedType "sealed")
    (fun () -> field utyp "_" char)


(*
  Test that fields can be added to views over unions.
*)
let test_adding_fields_through_views _ =
  let module M = struct
    let union_u = union "union_u"
    let u = typedef union_u "u"
    let _x = field u "x" int
    let _y = field u "y" float
    let () = seal u
  end in ()


(*
  Test that attempting to seal an empty union is treated as an error.
*)
let test_sealing_empty_union _ =
  let empty = union "empty" in

  assert_raises (Unsupported "union with no fields")
    (fun () -> seal empty)


module Foreign_tests = Build_foreign_tests(Tests_common.Foreign_binder)
module Stub_tests = Build_stub_tests(Generated_bindings)


let suite = "Union tests" >:::
  ["inspecting float representation"
    >:: test_inspecting_float;

   "detecting endianness"
    >:: test_endian_detection;

   "union padding (foreign)"
    >:: Foreign_tests.test_union_padding;

   "union padding (stubs)"
    >:: Stub_tests.test_union_padding;

   "passing unions by value (stubs)"
    >:: Stub_tests.test_passing_unions_by_value;

   "union address"
    >:: test_union_address;

   "updating sealed union"
    >:: test_updating_sealed_union;

   "sealing empty union"
    >:: test_sealing_empty_union;

   "fields can be added to views over unions"
   >:: test_adding_fields_through_views;

   "sealing empty union"
    >:: test_sealing_empty_union;

   "test adding fields to tagless unions"
   >:: Struct_stubs_tests.test_tagless_unions;

   (* "test layout of unions with missing fields" *)
   (* >:: Struct_stubs_tests.test_missing_fields; *)
  ]


let _ =
  run_test_tt_main suite
