(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
  Test that primitives are passable.
*)
let test_primitives_are_passable _ =
  let _ = void @-> returning void 
  and _ = char @-> returning char
  and _ = schar @-> returning schar
  and _ = float @-> returning float
  and _ = double @-> returning double
  and _ = int   @-> returning int  
  and _ = nativeint @-> returning nativeint
  and _ = int8_t @-> returning int8_t
  and _ = short @-> returning short
  and _ = int16_t @-> returning int16_t
  and _ = int32_t @-> returning int32_t
  and _ = int64_t @-> returning int64_t
  and _ = uchar @-> returning uchar
  and _ = uint8_t @-> returning uint8_t
  and _ = uint16_t @-> returning uint16_t
  and _ = uint32_t @-> returning uint32_t
  and _ = uint64_t @-> returning uint64_t
  and _ = size_t @-> returning size_t
  and _ = ushort @-> returning ushort
  and _ = uint @-> returning uint
  and _ = ulong @-> returning ulong
  and _ = ullong @-> returning ullong
  in ()


(*
  Test that unions are not passable
*)
let test_unions_are_not_passable _ =
  let module M = struct
    type u

    let u : u union typ = union "u"
    let (-:) ty label = field u label ty
    let _c = int    -: "c"
    let _f = double -: "f"
    let _p = ptr u  -: "p"
    let () = seal u

    let _ = begin
      (* union types can be used as argument types *)
      ignore (u @-> returning void);

      assert_raises ~msg:"Foreign rejects union types as argument types"
        (Unsupported "libffi does not support passing unions")
        (fun () -> Foreign.funptr (u @-> returning void));
      
      (* union types can be used as return types *)
      ignore (u @-> returning void);

      assert_raises ~msg:"Foreign rejects union types as return types"
        (Unsupported "libffi does not support passing unions")
        (fun () -> Foreign.funptr (void @-> returning u));
    end
  end in ()

(*
  Test the passability of long double values
*)
let test_ldouble_not_passable _ =
  assert_raises ~msg:"Foreign rejects ldouble type as argument"
    (Unsupported "libffi does not support passing long double")
    (fun () -> Foreign.funptr (ldouble @-> returning void));

  assert_raises ~msg:"Foreign rejects ldouble type as return type"
    (Unsupported "libffi does not support passing long double")
    (fun () -> Foreign.funptr (void @-> returning ldouble))

(*
  Test the passability of complex values
*)
let test_complex_value_passability _ =
  (* complex32 can be used as an argument type *)
  ignore (complex32 @-> returning void);

  (* complex64 can be used as an argument type *)
  ignore (complex64 @-> returning void);

  (* complexld can be used as an argument type *)
  ignore (complexld @-> returning void);

  assert_raises ~msg:"Foreign rejects complex32 type as argument"
    (Unsupported "libffi does not support passing float _Complex")
    (fun () -> Foreign.funptr (complex32 @-> returning void));

  assert_raises ~msg:"Foreign rejects complex64 type as argument"
    (Unsupported "libffi does not support passing double _Complex")
    (fun () -> Foreign.funptr (complex64 @-> returning void));
  
  assert_raises ~msg:"Foreign rejects complexld type as argument"
    (Unsupported "libffi does not support passing long double _Complex")
    (fun () -> Foreign.funptr (complexld @-> returning void));
  
  (* complex32 can be used as a return type *)
  ignore (void @-> returning complex32);

  (* complex64 can be used as a return type *)
  ignore (void @-> returning complex64);

  (* complexld can be used as a return type *)
  ignore (void @-> returning complexld);

  assert_raises ~msg:"Foreign rejects complex32 type as return type"
    (Unsupported "libffi does not support passing float _Complex")
    (fun () -> Foreign.funptr (void @-> returning complex32));

  assert_raises ~msg:"Foreign rejects complex64 type as return type"
    (Unsupported "libffi does not support passing double _Complex")
    (fun () -> Foreign.funptr (void @-> returning complex64));

  assert_raises ~msg:"Foreign rejects complexld type as return type"
    (Unsupported "libffi does not support passing long double _Complex")
    (fun () -> Foreign.funptr (void @-> returning complexld))


(*
  Test that arrays are not passable
*)
let test_arrays_are_not_passable _ =
  assert_raises ~msg:"Array type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> array 1 int @-> returning void);
      
  assert_raises ~msg:"Array type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (array 1 int))


(*
  Test that bigarrays are not passable
*)
let test_bigarrays_are_not_passable _ =
  assert_raises ~msg:"bigarray type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> bigarray genarray [|1|] Bigarray.int @-> returning void);
      
  assert_raises ~msg:"bigarray1 type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> bigarray array1 1 Bigarray.int @-> returning void);
      
  assert_raises ~msg:"bigarray2 type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> bigarray array2 (1, 2) Bigarray.int @-> returning void);
      
  assert_raises ~msg:"bigarray3 type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> bigarray array3 (1, 2, 3) Bigarray.int @-> returning void);

  assert_raises ~msg:"bigarray type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (bigarray genarray [|1|] Bigarray.int));

  assert_raises ~msg:"bigarray1 type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (bigarray array1 1 Bigarray.int));

  assert_raises ~msg:"bigarray2 type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (bigarray array2 (1, 2) Bigarray.int));

  assert_raises ~msg:"bigarray3 type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (bigarray array3 (1, 2, 3) Bigarray.int))


(*
  Test that pointers are passable
*)
let test_pointers_are_passable _ =
  (* Pointers to primitives are passable *)
  let _ = ptr void @-> returning (ptr void)
  and _ = ptr int @-> returning (ptr int)
  and _ = ptr (ptr int) @-> returning (ptr (ptr int))
  in

  (* Pointers to unpassable types are passable *)
  let module M = struct
    type s1 and u

    let s1 : s1 structure typ = structure "s1"
    let _ = field s1 "_" int
    let _ = field s1 "_" (ptr s1)
    let () = seal s1

    let u : u union typ = union "u"
    let _ = field u "_" int
    let () = seal u
  end in
  let open M in

  let _ = ptr s1 @-> returning (ptr s1)
  and _ = ptr u @-> returning (ptr u) in
  ()


(*
  Test that function pointers are passable
*)
let test_function_pointers_are_passable _ =
  (* Pointers to primitives are passable *)
  ignore (Foreign.funptr (int @-> returning int)
          @-> returning (Foreign.funptr (int @-> returning int)))


(*
  Test that values of abstract types are not passable
*)
let test_abstract_values_are_not_passable _ = begin
  assert_raises ~msg:"Abstract type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () ->
      (abstract ~name:"abstract" ~size:1 ~alignment:1) @-> returning void);

  assert_raises ~msg:"Abstract type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () ->
      void @-> returning (abstract ~name:"abstract" ~size:1 ~alignment:1));
end


(*
  Test struct passability.  Structs are passable unless they contain
  unpassable members (unions, arrays, abstract types, or unpassable structs).
*)
let test_struct_passability _ =
  let module M = struct
    type s1 and s2 and s3 and s4 and s5 and s6 and u

    let s1 : s1 structure typ = structure "s1"
    let (-:) ty label = field s1 label ty
    let _ = int -: "_"
    let _ = double -: "_"
    let _ = ptr s1 -: "_"
    let _ = Foreign.funptr (int @-> returning int) -: "_"
    let () = seal s1

    let s2 : s2 structure typ = structure "s2"
    let (-:) ty label = field s2 label ty
    let _ = s1 -: "_"
    let _ = double -: "_"
    let _ = ptr (array 10 int) -: "_"
    let () = seal s2

    let s3 : s3 structure typ = structure "s3"
    let (-:) ty label = field s3 label ty
    let _ = array 10 (ptr char) -: "_"
    let () = seal s3

    let s4 : s4 structure typ = structure "s4"
    let (-:) ty label = field s4 label ty
    let _ = s3 -: "_"
    let () = seal s4

    let u : u union typ = union "u"
    let (-:) ty label = field u label ty
    let _ = int -: "_"
    let () = seal u

    let s5 : s5 structure typ = structure "s5"
    let (-:) ty label = field s5 label ty
    let _ = u -: "_"
    let () = seal s5

    let s6 : s6 structure typ = structure "s6"
    let (-:) ty label = field s6 label ty
    let _ = abstract ~name:"abstract" ~size:1 ~alignment:1 -: "_"
    let () = seal s6

    let _ = begin
      (* Struct types can be argument types *)
      ignore (s1 @-> returning void);
      ignore (s2 @-> returning void);

      (* Struct types can be return types *)
      ignore (void @-> returning s1);
      ignore (void @-> returning s2);

      (* Structs with array members can be arguments *)
      ignore (s3 @-> returning void);

      assert_raises
        ~msg:"Foreign rejects structs with array members as arguments"
        (Unsupported "libffi does not support passing arrays")
        (fun () -> Foreign.funptr (s3 @-> returning void));

      (* Structs with array members can be return types *)
      ignore (void @-> returning s3);

      assert_raises
        ~msg:"Foreign rejects structs with array members as return types"
        (Unsupported "libffi does not support passing arrays")
        (fun () -> Foreign.funptr (void @-> returning s3));


      assert_raises
        ~msg:"Foreign rejects structs with unpassable struct members as arguments"
        (Unsupported "libffi does not support passing arrays")
        (fun () -> Foreign.funptr (s4 @-> returning void));

      assert_raises
        ~msg:"Foreign rejects structs with unpassable struct members as return types"
        (Unsupported "libffi does not support passing arrays")
        (fun () -> Foreign.funptr (void @-> returning s4));
 
      (* Structs with union members can be arguments *)
      ignore (s5 @-> returning void);

      assert_raises
        ~msg:"Foreign rejects structs with union members as arguments"
        (Unsupported "libffi does not support passing unions")
        (fun () -> Foreign.funptr (s5 @-> returning void));
 
      (* Structs with union members can be return types *)
      ignore (void @-> returning s5);

      assert_raises
        ~msg:"Foreign rejects structs with union members as return types"
        (Unsupported "libffi does not support passing unions")
        (fun () -> Foreign.funptr (void @-> returning s5));

      (* Structs with abstract members can be arguments *)
      ignore (s6 @-> returning void);

      assert_raises
        ~msg:"Foreign rejects structs with abstract members as arguments"
        (Unsupported "libffi does not support passing values of abstract type")
        (fun () -> Foreign.funptr (s6 @-> returning void));
 
      ignore (void @-> returning s6);

      assert_raises
        ~msg:"Foreign rejects structs with abstract members as return types"
        (Unsupported "libffi does not support passing values of abstract type")
        (fun () -> Foreign.funptr (void @-> returning s6));
    end
  end in ()


(*
  Test passability of incomplete types.  Trying to use an incomplete type
  in a function specification should give rise to an error.
*)
let test_incomplete_passability _ =
  let s = structure "incomplete"
  and u = union "incomplete"
  in begin
    assert_raises IncompleteType
      (fun () -> s @-> returning void);
    
    assert_raises IncompleteType
      (fun () -> void @-> returning s);
    
    assert_raises IncompleteType
      (fun () -> u @-> returning void);
    
    assert_raises IncompleteType
      (fun () -> void @-> returning u);
  end


(*
  Test that OCaml values cannot be passed to C functions that are called
  without the OCaml runtime lock.
*)
let test_ocaml_values_are_not_passable_when_releasing_the_lock _ =
  begin
    assert_raises (Unsupported "Unsupported argument type when releasing runtime lock")
      (fun () ->
         Foreign.foreign "puts" (ocaml_string @-> returning int)
           ~release_runtime_lock:true);

    let module Bindings (F:Cstubs.FOREIGN) =
    struct F.(foreign "puts" (ocaml_string @-> returning int)) end
    in

    assert_raises (Unsupported "Unsupported argument type when releasing runtime lock")
      (fun () ->
         Cstubs.write_c ~prefix:"tests" ~concurrency:Cstubs.unlocked
           Format.str_formatter (module Bindings));

    assert_raises (Unsupported "Unsupported argument type when releasing runtime lock")
      (fun () ->
         Cstubs.write_c ~prefix:"tests" ~concurrency:Cstubs.lwt_jobs
           Format.str_formatter (module Bindings));

    assert_raises (Unsupported "Unsupported argument type when releasing runtime lock")
      (fun () ->
         Cstubs.write_c ~prefix:"tests" ~concurrency:Cstubs.lwt_preemptive
           Format.str_formatter (module Bindings));
  end


let suite = "Passability tests" >:::
  ["primitives are passable"
    >:: test_primitives_are_passable;

   "unions are not passable"
    >:: test_unions_are_not_passable;

   "complex values passability"
    >:: test_complex_value_passability;

   "long doubles are not passble"
    >:: test_ldouble_not_passable;

   "arrays are not passable"
    >:: test_arrays_are_not_passable;

   "bigarrays are not passable"
    >:: test_bigarrays_are_not_passable;

   "pointers are passable"
    >:: test_pointers_are_passable;

   "function pointers are passable"
    >:: test_function_pointers_are_passable;

   "abstract values are not passable"
    >:: test_abstract_values_are_not_passable;

   "struct passability"
    >:: test_struct_passability;

   "incomplete types are not passable"
    >:: test_incomplete_passability;

   "ocaml values are not passable when the runtime lock is released"
    >:: test_ocaml_values_are_not_passable_when_releasing_the_lock;
  ]


let _ =
  run_test_tt_main suite
