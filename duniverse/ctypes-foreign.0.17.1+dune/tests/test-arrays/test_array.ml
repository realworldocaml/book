(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(* TODO not needed anymore?
 * let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW]) *)


(*
  Creating multidimensional arrays, and reading and writing elements.
*)
let test_multidimensional_arrays _ =
  let module Array = CArray in
  (* one dimension *)
  let one = Array.make int 10 in
  
  for i = 0 to Array.length one - 1 do
    one.(i) <- i
  done;

  for i = 0 to Array.length one - 1 do
    assert_equal i one.(i)
  done;

  (* two dimensions *)
  let two = Array.make (array 5 char) 10 in
  let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in

  for i = 0 to 9 do
    for j = 0 to 4 do
      two.(i).(j) <- s.[i + j]
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 4 do
      assert_equal two.(i).(j) s.[i + j]
        ~printer:(String.make 1)
    done
  done;

  (* three dimensions *)
  let three = Array.make (array 2 (array 5 float)) 10 in
  let float = Stdlib.float in

  for i = 0 to 9 do
    for j = 0 to 1 do
      for k = 0 to 4 do
      three.(i).(j).(k) <- float i *. float j -. float k
      done
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 1 do
      for k = 0 to 4 do
        assert_equal three.(i).(j).(k) (float i *. float j -. float k)
          ~printer:string_of_float
      done
    done
  done;

  (* four *)
  let four = Array.make (array 3 (array 2 (array 5 int32_t))) 10 in

  for i = 0 to 9 do
    for j = 0 to 2 do
      for k = 0 to 1 do
        for l = 0 to 4 do
          four.(i).(j).(k).(l)
          <- Int32.(mul (sub (of_int i) (of_int j)) (add (of_int k) (of_int l)))
        done
      done
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 2 do
      for k = 0 to 1 do
        for l = 0 to 4 do
          assert_equal four.(i).(j).(k).(l)
          Int32.(mul (sub (of_int i) (of_int j)) (add (of_int k) (of_int l)))
            ~printer:Int32.to_string
        done
      done
    done
  done

(*
  Test the CArray.iter function
 *)
let test_iter _ =
  let r = ref 0 in
  let a = CArray.of_list int [1; 2; 3] in
  let () = CArray.iter (fun v -> r := !r + v) a in
  assert_equal !r 6;

  let r = ref 0 in
  let a = CArray.of_list int [] in
  let () = CArray.iter (fun _ -> assert false) a in
  assert_equal !r 0


(*
  Test the CArray.map function
 *)
let test_map _ =
  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.map float float_of_int a in
  assert_equal [1.0; 2.0; 3.0] (CArray.to_list r);
    
  let a = CArray.of_list int [] in
  let r = CArray.map string (fun _ -> assert false) a in
  assert_equal (CArray.length r) 0


(*
  Test the CArray.mapi function
 *)
let test_mapi _ =
  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.mapi int (+) a in
  assert_equal [1; 3; 5] (CArray.to_list r);
    
  let a = CArray.of_list int [] in
  let r = CArray.mapi string (fun _ _ -> assert false) a in
  assert_equal (CArray.length r) 0


(*
  Test the CArray.fold_left function
 *)
let test_fold_left _ =
  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.fold_left (Printf.sprintf "%s%d") "." a in
  assert_equal ".123" r;
    
  let a = CArray.of_list int [] in
  let r = CArray.fold_left (fun _ -> assert false) [] a in
  assert_equal r []


(*
  Test the CArray.fold_right function
 *)
let test_fold_right _ =
  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.fold_right (Printf.sprintf "%d%s") a "." in
  assert_equal "123." r;
    
  let a = CArray.of_list int [] in
  let r = CArray.fold_right (fun _ -> assert false) a [] in
  assert_equal r []

(*
  Test the CArray.copy function
 *)
let test_copy _ =
  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.copy a in

  begin
    assert_equal [1; 2; 3] (CArray.to_list a);
    assert_equal [1; 2; 3] (CArray.to_list r);
    CArray.set r 0 10;
    assert_equal [1; 2; 3] (CArray.to_list a);
    assert_equal [10; 2; 3] (CArray.to_list r);
    CArray.set a 1 20;
    assert_equal [1; 20; 3] (CArray.to_list a);
    assert_equal [10; 2; 3] (CArray.to_list r);
  end


(*
  Test the CArray.sub function
 *)
let test_sub _ =
  let a = CArray.of_list int [1; 2; 3] in

  assert_raises (Invalid_argument "CArray.sub") begin fun () ->
    CArray.sub a ~pos:(-1) ~length:1
  end;

  assert_raises (Invalid_argument "CArray.sub") begin fun () ->
    CArray.sub a ~pos:1 ~length:4
  end;
  
  assert_raises (Invalid_argument "CArray.sub") begin fun () ->
    CArray.sub a ~pos:1 ~length:(-1)
  end;

  let r = CArray.sub a ~pos:1 ~length:2 in 
  assert_equal [2; 3] (CArray.to_list r);
    
  let r = CArray.sub a ~pos:1 ~length:0 in
  assert_equal [] (CArray.to_list r);

  let a = CArray.of_list int [1; 2; 3] in
  let r = CArray.sub a ~pos:1 ~length:2 in
  begin
    CArray.set r 0 10;
    assert_equal [1; 2; 3] (CArray.to_list a);
    assert_equal [10; 3] (CArray.to_list r);
  end


(*
  Test the CArray.of_string function
*)
let test_of_string _ =
  let s = "abcdefghiABCDEFGHI" in
  let a = CArray.of_string s in
  let s' = coerce (ptr char) string (CArray.start a) in
  assert_equal s s';

  let s = "" in
  let a = CArray.of_string s in
  let s' = coerce (ptr char) string (CArray.start a) in
  assert_equal s s'


(*
  Test that creating an array initializes all elements appropriately.
*)
let test_array_initialiation _ =
  let module Array = CArray in
  let int_array = Array.make int ~initial:33 10 in
  for i = 0 to Array.length int_array - 1 do
    assert_equal 33 int_array.(i)
  done;

  let int_array_array = Array.make (array 10 int) ~initial:int_array 5 in
  for i = 0 to Array.length int_array_array - 1 do
    for j = 0 to Array.length int_array_array.(i) - 1 do
      assert_equal 33 int_array_array.(i).(j)
    done
  done


(*
  Test that creating arrays of elements of incomplete type fails.
*)
let test_arrays_of_incomplete_type _ =
  let module M = struct
    let () = assert_raises IncompleteType
      (fun () -> CArray.make void 10)

    let s = structure "s"
    let () = assert_raises IncompleteType
      (fun () -> CArray.make s 10)
  end in ()


(*
  Test that OCaml types cannot be used to build arrays.
*)
let test_ocaml_types_rejected_as_array_elements _ =
  assert_raises IncompleteType
    (fun () -> CArray.make ocaml_string 10)


(*
  Test that creating an array initializes all elements appropriately.
*)
let test_pointer_to_array_arithmetic _ =
  let module Array = CArray in
  (* int ( * )[3] *)
  let p = allocate_n (array 3 int) ~count:4 in
  p <-@ Array.of_list int [1; 2; 3];
  (p +@ 1) <-@ Array.of_list int [4; 5; 6];
  (p +@ 2) <-@ Array.of_list int [7; 8; 9];
  (p +@ 3) <-@ Array.of_list int [10; 11; 12];
  let q = p in
  assert_equal 8 (!@(q +@ 2)).(1);
  assert_equal 12 (!@(q +@ 3)).(2);
  assert_equal 1 (!@(q +@ 0)).(0);
  let a = Array.from_ptr p 4 in
  assert_equal 8 a.(2).(1);
  assert_equal 12 a.(3).(2);
  assert_equal 1 a.(0).(0)


(*
  Test bounds checks for CArray.get
*)
let test_bounds_checks_get _ =
  let module Array = CArray in
  let c = CArray.of_list int [1;2;3] in

  assert_raises (Invalid_argument "index out of bounds") begin fun () ->
    c.(-1);
  end;

  assert_raises (Invalid_argument "index out of bounds") begin fun () ->
    c.(CArray.length c);
  end

(*
  Test bounds checks for CArray.set
*)
let test_bounds_checks_set _ =
  let module Array = CArray in
  let c = CArray.of_list int [1;2;3] in

  assert_raises (Invalid_argument "index out of bounds") begin fun () ->
    c.(-1) <- 0;
  end;

  assert_raises (Invalid_argument "index out of bounds") begin fun () ->
    c.(CArray.length c) <- 0;
  end


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test passing pointer to array of structs.
  *)
  let test_passing_pointer_to_array_of_structs _ =
    let box_int x =
      let v = make s in
      setf v tag 'i';
      let pd = v @. data in
      (pd |-> i) <-@ x;
      v
    in

    let box_double x =
      let v = make s in
      setf v tag 'd';
      let pd = v @. data in
      (pd |-> d) <-@ x;
      v
    in

    let sum = 
      accepts_pointer_to_array_of_structs
        (from_voidp
           (array 5 s)
           (to_voidp
              (CArray.start
                 (CArray.of_list s
                    [box_int 10;
                     box_double 3.5;
                     box_int 12;
                     box_double (-14.1);
                     box_double (103.25)]))))
    in
    assert_equal
      (103.25 +. (-14.1) +. 12.0 +. 3.5 +. 10.0)
      sum
end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)

let suite = "Array tests" >:::
  ["multidimensional arrays"
    >:: test_multidimensional_arrays;

   "CArray.iter "
    >:: test_iter;

   "CArray.map "
    >:: test_map;

   "CArray.mapi "
    >:: test_mapi;

   "CArray.fold_left"
    >:: test_fold_left;

   "CArray.fold_right"
    >:: test_fold_right;

   "CArray.copy"
    >:: test_copy;

   "CArray.sub"
    >:: test_sub;

   "CArray.of_string"
   >:: test_of_string;

   "array initialization"
    >:: test_array_initialiation;

   "arrays of incomplete type"
    >:: test_arrays_of_incomplete_type;

   "ocaml_string cannot be used to build arrays"
    >:: test_ocaml_types_rejected_as_array_elements;

   "pointer to array arithmetic"
    >:: test_pointer_to_array_arithmetic;

   "bounds checks (get)"
    >:: test_bounds_checks_get;

   "bounds checks (set)"
    >:: test_bounds_checks_set;

   "passing pointer to array of structs (foreign)"
    >:: Foreign_tests.test_passing_pointer_to_array_of_structs;

   "passing pointer to array of structs (stubs)"
    >:: Stub_tests.test_passing_pointer_to_array_of_structs;
  ]


let _ =
  run_test_tt_main suite
