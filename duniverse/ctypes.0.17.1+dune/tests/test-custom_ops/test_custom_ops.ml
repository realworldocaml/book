(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes

let hash = Hashtbl.hash


(*
  Test hashing and equality for managed buffers.

  Hashing and equality are based on the addresses of malloc-allocated
  objects, so even structurally-equal values should have different
  hashes and compare unequal.
*)
let test_managed_buffer_hashing_and_equality _ =
  let i1 = allocate int 20 in
  let i2 = allocate int 20 in
  assert_equal !@i1 !@i2;
  assert_equal (hash i1) (hash i1);
  assert_bool
    "equal-but-not-identical objects have distinct hashes"
    (hash i1 <> hash i2);
  assert_bool
    "equal-but-not-identical objects do not compare equal"
    (i1 <> i2)


(*
  Test type info hashing and equality.

  Equality is structural, so distinct but structurally-equal values
  should have equal hashes and compare equal.
*)

let test_type_info_hashing_and_equality _ =
  let module M = struct
    type s
    let s : s structure typ = structure "s"
    let _ = begin
      ignore (field s "d" double);
      ignore (field s "p" (ptr void));
      seal s
    end
      
    type t
    let t : t structure typ = structure "s"
    let _ = begin
      ignore (field t "d" double);
      ignore (field t "p" (ptr void));
      seal t
    end
      
    let () = begin
      (* Pointer equality is structural. *)
      assert_equal ~msg:"Equal pointer types have equal hashes"
        (hash (ptr double)) (hash (ptr double));

      assert_equal ~msg:"Equal pointer types compare equal"
        (ptr double) (ptr double);

      (* Array equality is structural. *)
      assert_equal ~msg:"Equal array types have equal hashes"
        (hash (array 3 (array 4 int))) (hash (array 3 (array 4 int)));

      assert_equal ~msg:"Equal array types compare equal"
        (array 3 (array 4 int)) (array 3 (array 4 int));

      assert_bool "Distinct array types do not compare equal"
        (array 3 (array 4 int) <> array 3 (array 5 int));

      (* Structure equality is structural *)
      assert_equal (hash s) (hash s);

      assert_bool
        "equal-but-not-identical structure types have equal hashes"
        (hash s = hash t);

      assert_bool
        "equal-but-not-identical structure types compare equal"
        (Obj.repr s = Obj.repr t);
    end
  end in ()


let suite = "Custom ops tests" >:::
  ["managed buffer hashing and equality" >::
     test_managed_buffer_hashing_and_equality;

   "type info hashing and equality" >::
     test_type_info_hashing_and_equality;
  ]


let _ =
  run_test_tt_main suite
