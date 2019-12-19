(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes_memory_stubs

[@@@warning "-6-33"]

(* Tests for the low-level module on which the public high-level
   interface is based.  
*)


let make_unmanaged ~reftyp p = Ctypes_ptr.Fat.make ~managed:None ~reftyp p


(* Call the C function

        double fabs(double)
*)
let test_fabs _ =
  Ctypes_ffi_stubs.(
    let double_ffitype = primitive_ffitype Ctypes_primitive_types.Double in
    let callspec = allocate_callspec
      ~check_errno:false
      ~runtime_lock:false
      ~thread_registration:false
    in
    let arg_1_offset = add_argument callspec double_ffitype in
    let () = prep_callspec callspec Libffi_abi.(abi_code default_abi)
      double_ffitype in
    
    let dlfabs = Ctypes_ptr.Raw.of_nativeint (Dl.dlsym "fabs") in
    let dlfabs_fat = make_unmanaged dlfabs
        ~reftyp:Ctypes.(double @-> returning double) in

    let fabs x =
      call "fabs" dlfabs_fat callspec
        (fun p _values ->
          write Ctypes_primitive_types.Double x
            Ctypes_ptr.(make_unmanaged ~reftyp:Ctypes_static.Void (Raw.(add p (of_int arg_1_offset)))))
        (fun p -> read Ctypes_primitive_types.Double (make_unmanaged ~reftyp:Ctypes_static.Void p))
    in

    assert_equal 2.0 (fabs (-2.0)) ~printer:string_of_float;
    assert_equal 12.0 (fabs (12.0)) ~printer:string_of_float;
    assert_equal 0.0 (fabs 0.0) ~printer:string_of_float;
  )


(* Call the C function

        double pow(double, double)
*)
let test_pow _ =
  Ctypes_ffi_stubs.(
    let double_ffitype = primitive_ffitype Ctypes_primitive_types.Double in
    let callspec = allocate_callspec
      ~check_errno:false
      ~runtime_lock:false
      ~thread_registration:false
    in
    let arg_1_offset = add_argument callspec double_ffitype in
    let arg_2_offset = add_argument callspec double_ffitype in
    let () = prep_callspec callspec Libffi_abi.(abi_code default_abi) 
      double_ffitype in
    
    let dlpow = Ctypes_ptr.Raw.of_nativeint (Dl.dlsym "pow") in
    let dlpow_fat = make_unmanaged dlpow
        ~reftyp:Ctypes.(double @-> double @-> returning double) in

    let pow x y =
      call "pow" dlpow_fat callspec
        (fun buffer _values ->
          write Ctypes_primitive_types.Double x
            Ctypes_ptr.(make_unmanaged ~reftyp:Ctypes_static.Void (Raw.(add buffer (of_int arg_1_offset))));
          write Ctypes_primitive_types.Double y
            Ctypes_ptr.(make_unmanaged ~reftyp:Ctypes_static.Void (Raw.(add buffer (of_int arg_2_offset)))))
        (fun p -> read Ctypes_primitive_types.Double (make_unmanaged ~reftyp:Ctypes_static.Void p))
    in

    assert_equal 8.0 (pow 2.0 3.0);
    assert_equal 1.0 (pow 10.0 0.0);
  )


let suite = "Raw interface tests" >:::
  ["test_abs"
    >:: test_fabs;

   "test_pow"
   >:: test_pow
  ]


let _ =
  run_test_tt_main suite
