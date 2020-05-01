(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes

[@@@warning "-32-34"]

(*
  Test some relationships between the alignment requirements of primitive types.
*)
let test_primitive_alignment _ = begin
  assert_equal ~msg:"alignmentof(char) == 1"
    (alignment char) 1;

  assert_equal ~msg:"alignmentof(signed char) == 1"
    (alignment schar) 1;

  assert_equal ~msg:"alignmentof(unsigned char) == 1"
    (alignment uchar) 1;

  assert_equal ~msg:"alignmentof(short) == alignmentof(unsigned short)"
    (alignment short) (alignment ushort);

  assert_equal ~msg:"alignmentof(int) == alignmentof(unsigned int)"
    (alignment int) (alignment uint);

  assert_equal ~msg:"alignmentof(long) == alignmentof(unsigned long)"
    (alignment long) (alignment ulong);

  assert_equal ~msg:"alignmentof(long long) == alignmentof(unsigned long long)"
    (alignment llong) (alignment ullong);

  assert_equal ~msg:"alignmentof(int8_t) == alignmentof(uint8_t)"
    (alignment int8_t) (alignment uint8_t);

  assert_equal ~msg:"alignmentof(int16_t) == alignmentof(uint16_t)"
    (alignment int16_t) (alignment uint16_t);

  assert_equal ~msg:"alignmentof(int32_t) == alignmentof(uint32_t)"
    (alignment int32_t) (alignment uint32_t);

  assert_equal ~msg:"alignmentof(int64_t) == alignmentof(uint64_t)"
    (alignment int64_t) (alignment uint64_t);

  assert_equal ~msg:"alignmentof(complex32) == alignmentof(float)"
    (alignment complex32) (alignment float);

  assert_equal ~msg:"alignmentof(complex64) == alignmentof(double)"
    (alignment complex64) (alignment double);

  assert_equal ~msg:"alignmentof(complexld) == alignmentof(ldouble)"
    (alignment complexld) (alignment ldouble);
end


(*
  Test the alignment of abstract types
*)
let test_abstract_alignment _ =
  for i = 1 to 10 do
    assert_equal
      i (alignment (abstract ~name:"abstract" ~size:(11 - i) ~alignment:i))
  done


(*
  Test that requesting the alignment of an incomplete type raises an exception.
*)
let test_incomplete_alignment _ =
  assert_raises IncompleteType
    (fun () -> alignment void);

  let module M = struct
    let t = structure "t"
    let i = field t "i" int

    let () =
      assert_raises IncompleteType
        (fun () -> alignment t)
  end in

  let module M = struct
    let u = union "u"
    let i = field u "i" int

    let () =
      assert_raises IncompleteType
        (fun () -> alignment u)
  end in
  ()


(*
  Test that [alignment] treats OCaml types as incomplete.
*)
let test_alignment_ocaml_string _ =
  assert_raises IncompleteType
    (fun () -> alignment ocaml_string)


(* 
   Test that the alignment of a struct is equal to the maximum
   alignment of its members.
*)
let test_struct_alignment _ = 
  let module M = struct
    type a and b and u

    let maximum = List.fold_left max 0

    let struct_a = structure "A"
    let (-:) ty label = field struct_a label ty
    let _ = char   -: "_"
    let _ = int    -: "_"
    let _ = double -: "_"
    let () = seal struct_a

    let () = assert_equal
      (maximum [alignment char;
                alignment int;
                alignment double])
      (alignment struct_a)

    let abs = abstract ~name:"abs" ~size:33 ~alignment:33
    let charish = view ~read:(fun _ -> ()) ~write:(fun () -> 'c') char

    let struct_b = structure "A"
    let (-:) ty label = field struct_b label ty
    let _ = charish                        -: "_"
    let _ = Foreign.funptr (int @-> returning int) -: "_"
    let _ = abs                            -: "_"
    let _ = double                         -: "_"
    let () = seal struct_b

    let () = assert_equal
      (maximum [alignment charish;
                alignment (Foreign.funptr (int @-> returning int));
                alignment abs;
                alignment double])
      (alignment struct_b)
  end in ()


(*
  Test that structs are properly tail-padded.  For example, suppose a 32-bit
  architecture with 8-bit bytes and word-aligned ints and the following
  definitions:

     struct A { char a; int b; char c; };
     struct B { struct A d; char e; }

  Then we should have the following layouts:

     A: a---bbbbc---
     B: A-----------e---

  and the following sizes:

     sizeof (struct A) == 12
     sizeof (struct B) == 16
*)
let test_struct_tail_padding _ = 
  let module M = struct
    type a and b and u

    let struct_a = structure "A"
    let (-:) ty label = field struct_a label ty
    let a = char -: "a"
    let b = int  -: "b"
    let c = char -: "c"
    let () = seal (struct_a : a structure typ)

    let u = union "U"
    let (-:) ty label = field u label ty
    let x = char -: "x"
    let () = seal (u : u union typ)

    let struct_b = structure "B"
    let (-:) ty label = field struct_b label ty
    let d = struct_a -: "d"
    let e = u        -: "e"
    let () = seal (struct_b : b structure typ)

    let char_ptr p = from_voidp char (to_voidp p)

    let va = make struct_a and vb = make struct_b
    let pa = addr va and pb = addr vb

    let () = begin
      assert_equal
        ~msg:"offsetof (A, a) == 0"
        (offsetof a) 0
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(A, b) == alignmentof(int)"
        (offsetof b) (alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"((char *)&pa->b - (char *)&pa->a) == alignmentof(int)"
        (ptr_diff (char_ptr (pa |-> a)) (char_ptr (pa |-> b)))
        (alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(A, c) == 2 * alignmentof(int)"
        (offsetof c) (2 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"sizeof(struct A) == 3 * alignmentof(int)"
        (sizeof struct_a)  (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(B, e) == 3 * alignmentof(int)"
        (offsetof e) (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"((char *)&pb->e - (char *)&pb->d) == 3 * alignmentof(int)"
        (ptr_diff (char_ptr (pb |-> d)) (char_ptr (pb |-> e)))
        (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"sizeof(struct B) == 4 * alignmentof(int)"
        (sizeof struct_b) (4 * alignment int)
        ~printer:string_of_int;
    end
  end in ()


(* 
   Test that the alignment of a bigarray is the same as the alignment
   of its element type.
*)
let test_bigarray_alignment _ =
  let module M = struct
    module B = Bigarray
    type k = K : ('a, 'b) Bigarray.kind * int -> k
    let kind_alignments = [
      K (B.float32, alignment float);
      K (B.float64, alignment double);
      K (B.int8_signed, alignment int8_t);
      K (B.int8_unsigned, alignment uint8_t);
      K (B.int16_signed, alignment int16_t);
      K (B.int16_unsigned, alignment uint16_t);
      K (B.int32, alignment int32_t);
      K (B.int64, alignment int64_t);
      K (B.int, alignment (ptr void));
      K (B.nativeint, alignment (ptr void));
      K (B.complex32, alignment complex32);
      K (B.complex64, alignment complex64);
      K (B.char, alignment char);
    ]

    let () = begin
      (* Genarray.t alignments *)
      List.iter (fun (K (kind, ealign)) ->
        assert_equal ealign (alignment (bigarray genarray [|2; 3; 5|] kind)))
        kind_alignments;

      (* Array1.t alignments *)
      List.iter (fun (K (kind, ealign)) ->
        assert_equal ealign (alignment (bigarray array1 7 kind)))
        kind_alignments;

      (* Array2.t alignments *)
      List.iter (fun (K (kind, ealign)) ->
        assert_equal ealign (alignment (bigarray array1 7 kind)))
        kind_alignments;

      (* Array3.t alignments *)
      List.iter (fun (K (kind, ealign)) ->
        assert_equal ealign (alignment (bigarray array3 (2, 3, 5) kind)))
        kind_alignments;
    end
  end in
  ()


let suite = "Alignment tests" >:::
  ["struct tail padding"
    >:: test_struct_tail_padding;

   "primitive alignment"
   >:: test_primitive_alignment;

   "struct alignment"
   >:: test_struct_alignment;

   "alignment of abstract types"
   >:: test_abstract_alignment;

   "alignment of incomplete types"
   >:: test_incomplete_alignment;

   "alignment considers ocaml_string incomplete"
    >:: test_alignment_ocaml_string;

   "alignment of bigarray types"
   >:: test_bigarray_alignment;
  ]


let _ =
  run_test_tt_main suite
