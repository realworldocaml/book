(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(* 
   Check coercions between pointers.
*)
let test_pointer_coercions _ =
  let module M = struct
    type boxed_type = T : 'a typ -> boxed_type
    let types = [
      T void;
      T int8_t;
      T uint16_t;
      T int;
      T float;
      T short;
      T complex64;
      T (ptr double);
      T string;
      T (bigarray array1 10 Bigarray.int32);
      T (array 5 int32_t);
      T (structure "s");
      T (union "u");
      T (abstract ~name:"a" ~size:12 ~alignment:4);
    ]

    (* Check that we can construct a coercion between any two pointer types *)
    let () =
      ListLabels.iter types
        ~f:(fun (T t1) ->
          ListLabels.iter types
            ~f:(fun (T t2) ->
              let _fn = coerce (ptr t1) (ptr t2) in ()))

    (* Check that pointer coercions are value-preserving. *)
    let v = 10
    let p = allocate int v
    let p' = coerce (ptr float) (ptr int) (coerce (ptr int) (ptr float) p)

    let () = assert_equal p p'
  end in ()


(*
  Check that coercions between a pointer to a struct and a pointer to
  its first member succeed.
*)
let test_struct_first_member_coercions _ =
  let module M = struct
    let s = structure "s"
    let f = field s "f" double
    let _i = field s "i" int
    let () = seal s

    let () = begin
      let v = make s in
      let p = coerce (ptr s) (ptr double) (addr v) in

      setf v f 5.5;
      assert_equal !@p 5.5;

      p <-@ 6.6;
      assert_equal (getf v f) 6.6
    end
  end in ()


(* 
   Check that coercions between a pointer to a union and a pointer to
   a member succeed.
*)
let test_union_coercions _ =
  let module M = struct
    let u = union "u"
    let f = field u "f" double
    let i = field u "i" int
    let () = seal u

    let () = begin
      let v = make u in
      let pf = coerce (ptr u) (ptr double) (addr v) in
      let pi = coerce (ptr u) (ptr int) (addr v) in

      setf v f 5.5;
      assert_equal !@pf 5.5;

      pi <-@ 12;
      assert_equal (getf v i) 12;

      setf v i 14;
      assert_equal !@pi 14;
      
      pf <-@ 6.6;
      assert_equal (getf v f) 6.6;
    end

  end in ()


(* 
   Check coercions between views.
*)
let test_view_coercions _ =
  let module M = struct
    type 'a variant = V of 'a
    let unV (V v) = v and inV v = V v
    let variant_view v = view v ~read:inV ~write:unV

    type 'a record = { r : 'a }
    let record_view v = view v
      ~read:(fun r -> {r})
      ~write:(fun {r} -> r)
      
    let pintvv = variant_view (variant_view (ptr int))
    let pintr = record_view (ptr int)
      
    let () = begin
      let pi = allocate int 100 in
      let v = allocate pintvv (V (V pi)) in

      assert_equal
        !@((coerce pintvv pintr !@v).r)
        100
    end

  end in ()


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (* 
     Check coercions between functions.
  *)
  let test_function_coercions _ =
    let isize_t = view size_t
      ~read:Unsigned.Size_t.to_int ~write:Unsigned.Size_t.of_int in
    let memchr' = coerce_fn
      (ptr void @-> int @-> size_t @-> returning (ptr void))
      (string @-> int8_t @-> isize_t @-> returning string_opt)
      memchr in
    begin
      assert_equal
        (memchr' "foobar" (Char.code 'b') 4)
        (Some "bar")
      ;
      assert_equal
        (memchr' "foobar" (Char.code 'b') 2)
        None
      ;
    end
end

(* 
   Check that identity coercions are cost-free.
*)
let test_identity_coercions _ =
  let f = fun x _y -> x in
  let fn = int @-> float @-> returning int in
  let f' = coerce_fn fn fn f in
  assert_bool "identity coercions are free" (f' == f)

let test_unsigned_coercions _ =
  assert_equal (Unsigned.UInt8.of_int 256) (Unsigned.UInt8.of_int 0);
  assert_equal (Unsigned.UInt16.of_int (1 lsl 16))
  (Unsigned.UInt16.of_int 0)

(* 
   Check that coercions between unsupported types raise an exception
*)
let test_unsupported_coercions _ =
  let module M = struct
    type boxed_type = T : 'a typ -> boxed_type
    let types = [
      T int8_t,
      [T uint16_t; T float; T complex64; T (bigarray array1 10 Bigarray.int32);
       T (array 5 int32_t); T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T uint16_t,
      [T int8_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T int,
      [T uint16_t; T float; T complex64; T (bigarray array1 10 Bigarray.int32);
       T (array 5 int32_t); T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T float,
      [T int8_t; T uint16_t; T int; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T ldouble,
      [T int8_t; T uint16_t; T int; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T short,
      [T uint16_t; T float; T complex64; T (bigarray array1 10 Bigarray.int32);
       T (array 5 int32_t); T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T complex64,
      [T int8_t; T uint16_t; T int; T float; T short;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T complexld,
      [T int8_t; T uint16_t; T int; T short; 
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T (bigarray array1 10 Bigarray.int32),
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T (array 5 int32_t),
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T (structure "s"),
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T (union "u"),
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T (abstract ~name:"a" ~size:12 ~alignment:4),
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];

      T ocaml_string,
      [T int8_t; T uint16_t; T int; T float; T short; T complex64;
       T (bigarray array1 10 Bigarray.int32); T (array 5 int32_t);
       T (structure "s"); T (union "u");
       T (abstract ~name:"a" ~size:12 ~alignment:4)];
    ]

    (* None of the types in the list are currently intercoercible. *)
    let () = 
      ListLabels.iter types ~f:(fun (T t1, ts) ->
      ListLabels.iter ts ~f:(fun (T t2) ->
              try 
                let _ : _ -> _ = coerce t1 t2 in
                assert_failure "coercion unexpectedly succeeded"
              with Uncoercible _ -> ()))
  end in ()


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Coercsion tests" >:::
  ["test pointer coercions"
    >:: test_pointer_coercions;

   "test struct first member coercions"
    >:: test_struct_first_member_coercions;

   "test union coercions"
    >:: test_union_coercions;

   "test view coercions"
    >:: test_view_coercions;

   "test function coercions (foreign)"
    >:: Foreign_tests.test_function_coercions;

   "test function coercions (stubs)"
    >:: Stub_tests.test_function_coercions;

   "test identity coercions"
    >:: test_identity_coercions;

   "test unsupported coercions"
    >:: test_unsupported_coercions;

   "test unsigned integer coersions"
    >:: test_unsigned_coercions
  ]


let _ =
  run_test_tt_main suite
