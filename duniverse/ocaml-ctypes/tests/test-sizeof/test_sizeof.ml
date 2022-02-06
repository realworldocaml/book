(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes

[@@@warning "-27-32"]

(*
  Test some relationships between the sizes of primitive types.
*)
let test_sizeof_primitives _ = begin
  assert_equal ~msg:"sizeof (char) == 1"
    (sizeof char) 1;

  assert_equal ~msg:"sizeof (unsigned char) == 1"
    (sizeof uchar) 1;

  assert_equal ~msg:"sizeof (signed char) == 1"
    (sizeof schar) 1;

  assert_bool "sizeof (char) <= sizeof (int)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (float) <= sizeof (double)"
    (sizeof float <= sizeof double);

  assert_bool "sizeof (double) <= sizeof (long double)"
    (sizeof double <= sizeof ldouble);

  assert_bool "sizeof (short) <= sizeof (int)"
    (sizeof short <= sizeof int);

  assert_bool "sizeof (int) <= sizeof (long)"
    (sizeof int <= sizeof long);

  assert_bool "sizeof (long) <= sizeof (long long)"
    (sizeof long <= sizeof llong);

  assert_bool "sizeof (double complex) <= sizeof (long double complex)"
    (sizeof complex64 <= sizeof complexld);

  assert_equal ~msg:"2 * sizeof (int32_t) == sizeof (int64_t)"
    (2 * sizeof int32_t) (sizeof int64_t);

  assert_equal ~msg:"2 * sizeof (int16_t) == sizeof (int32_t)"
    (2 * sizeof int16_t) (sizeof int32_t);

  assert_equal ~msg:"2 * sizeof (int8_t) == sizeof (int16_t)"
    (2 * sizeof int8_t) (sizeof int16_t);

  assert_bool "sizeof (int16_t) <= sizeof (int)"
    (sizeof int16_t <= sizeof int);

  assert_bool "sizeof (int32_t) <= sizeof (long)"
    (sizeof int32_t <= sizeof long);

  assert_bool "sizeof (int64_t) <= sizeof (long long)"
    (sizeof int64_t <= sizeof llong);

  assert_equal ~msg:"sizeof (short) == sizeof (unsigned short)"
    (sizeof short) (sizeof ushort);

  assert_equal ~msg:"sizeof (int) == sizeof (unsigned int)"
    (sizeof int) (sizeof uint);

  assert_equal ~msg:"sizeof (long) == sizeof (unsigned long)"
    (sizeof long) (sizeof ulong);

  assert_equal ~msg:"sizeof (long long) == sizeof (unsigned long long)"
    (sizeof llong) (sizeof ullong);
end



(*
  Test some properties of the sizes of unions.
*)
let test_sizeof_unions _ =
  let int_char = union "int_char" in
  let _ = field int_char "_" int in
  let _ = field int_char "_" char in
  let _ = seal int_char in
  
  assert_equal (sizeof int) (sizeof int_char);


  let char17 = union "char17" in
  let _ = field char17 "_" (array 17 char) in
  let _ = seal char17 in
  
  assert_equal 17 (sizeof char17)


(*
  Test some properties of the sizes of structs.
*)
let test_sizeof_structs _ =
  let module M = struct
    (* We don't expect homogeneous structs consisting of words to have
       any padding. *)
    type h
    let () =
      for i = 1 to 10 do
        let homogeneous : h structure typ = structure "h" in
        for _j = 1 to i do
          ignore (field homogeneous "_" int);
        done;
        seal homogeneous;
        assert_equal (i * sizeof int) (sizeof homogeneous)
      done

  end in ()


(*
  Test the size of abstract types.
*)
let test_sizeof_abstract _ =
  for i = 1 to 10 do
    assert_equal
      i (sizeof (abstract ~name:"abstract" ~size:i ~alignment:(11 - i)))
  done


(*
  Test that taking the size of an incomplete type is treated as an error.
*)
let test_sizeof_incomplete _ = begin
  assert_raises IncompleteType
    (fun () -> sizeof (structure "incomplete"));

  assert_raises IncompleteType
    (fun () -> sizeof (union "incomplete"));
end
  

(*
  Test that taking the size of void is treated as an error.
*)
let test_sizeof_void _ =
  assert_raises IncompleteType
    (fun () -> sizeof void)
 

(*
  Test that [sizeof] treats OCaml types as incomplete.
*)
let test_sizeof_ocaml_string _ =
  assert_raises IncompleteType
    (fun () -> sizeof ocaml_string)


(*
  Test the behaviour of sizeof on array types.
*)
let test_sizeof_arrays _ = begin
  assert_equal ~msg:"The size of an array is the sum of the size of its members"
    (12 * (sizeof int8_t)) (sizeof (array 12 int8_t));

  assert_equal ~msg:"Arrays of arrays are correctly sized"
    (5 * 7 * (sizeof nativeint)) (sizeof (array 7 (array 5 nativeint)))
end
 

(*
  Test the behaviour of sizeof on bigarray types.
*)
let test_sizeof_bigarrays _ =
  let module M = struct
    module B = Bigarray
    type k = K : ('a, 'b) Bigarray.kind * int -> k
    let kind_sizes = [
      K (B.float32, 4);
      K (B.float64, 8);
      K (B.int8_signed, 1);
      K (B.int8_unsigned, 1);
      K (B.int16_signed, 2);
      K (B.int16_unsigned, 2);
      K (B.int32, 4);
      K (B.int64, 8);
      K (B.int, sizeof (ptr void));
      K (B.nativeint, sizeof (ptr void));
      K (B.complex32, 8);
      K (B.complex64, 16);
      K (B.char, 1);
    ]

    let () = begin
      (* Genarray.t sizes *)
      List.iter (fun (K (kind, size)) ->
        assert_equal (2 * 3 * 5 * size) (sizeof (bigarray genarray [|2; 3; 5|] kind)))
        kind_sizes;

      (* Array1.t sizes *)
      List.iter (fun (K (kind, size)) ->
        assert_equal (7 * size) (sizeof (bigarray array1 7 kind)))
        kind_sizes;

      (* Array2.t sizes *)
      List.iter (fun (K (kind, size)) ->
        assert_equal (2 * 3 * size) (sizeof (bigarray array2 (2, 3) kind)))
        kind_sizes;

      (* Array3.t sizes *)
      List.iter (fun (K (kind, size)) ->
        assert_equal (2 * 3 * 5 * size) (sizeof (bigarray array3 (2, 3, 5) kind)))
        kind_sizes;
    end
  end in
  ()
 

(*
  Test that all pointers have equal size.
*)
let test_sizeof_pointers _ = begin
  let pointer_size = sizeof (ptr void) in
  assert_equal pointer_size (sizeof (ptr void));
  assert_equal pointer_size (sizeof (ptr int));
  assert_equal pointer_size (sizeof (Foreign.funptr (int @-> returning int)));
  assert_equal pointer_size (sizeof (ptr (ptr void)));
  let module M = struct
    type t
    let t : t structure typ = structure "t"
    let c = field t "c" int
    let f = field t "f" double
    let () = seal t
  end in
  assert_equal pointer_size (sizeof (ptr M.t))
end


(*
  Test that the size of a view type is the same as the underlying type.
*)
let test_sizeof_views _ = begin
  let const c x = c in
  let vint = view ~read:(const [1]) ~write:(const 0) int
  and vchar = view ~read:(const ["1"]) ~write:(const 'a') char
  and vvoid = view ~read:(const (fun () -> ())) ~write:(const ()) void
  in
  assert_equal (sizeof int) (sizeof vint);
  assert_equal (sizeof char) (sizeof vchar);
  assert_raises IncompleteType (fun () -> sizeof vvoid);
end


let suite = "sizeof tests" >:::
  ["sizeof primitives"
   >:: test_sizeof_primitives;
   
   "sizeof structs"
   >:: test_sizeof_structs;
   
   "sizeof unions"
   >:: test_sizeof_unions;

   "sizeof abstract"
   >:: test_sizeof_abstract;

   "sizeof incomplete"
   >:: test_sizeof_incomplete;

   "sizeof void"
   >:: test_sizeof_void;

   "sizeof considers ocaml_string incomplete"
    >:: test_sizeof_ocaml_string;

   "sizeof arrays"
   >:: test_sizeof_arrays;

   "sizeof bigarrays"
   >:: test_sizeof_bigarrays;

   "sizeof pointers"
   >:: test_sizeof_pointers;

   "sizeof views"
   >:: test_sizeof_views;
  ]


let _ =
  run_test_tt_main suite
