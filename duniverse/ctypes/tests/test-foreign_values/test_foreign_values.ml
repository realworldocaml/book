(*
 * Copyright (c) 2013-2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Common(S)
  open M

  (*
    Retrieve a struct exposed as a global value. 
  *)
  let test_retrieving_struct _ =
    let p = CArray.start (getf !@global_struct str) in
    let stringp = from_voidp string (to_voidp (allocate (ptr char) p)) in
    begin
      let expected = "global string" in
      assert_equal expected !@stringp;
      assert_equal
        (Unsigned.Size_t.of_int (String.length expected))
        (getf !@global_struct len)
    end


  (*
    Store a reference to an OCaml function as a global function pointer.
  *)
  let test_global_callback _ =
    begin
      assert_equal !@plus None;

      plus <-@ Some (+);

      assert_equal (sum 1 10) 55;

      plus <-@ None;
    end

  (* Access an array exposed as a global value *)
  let test_retrieving_array _ =
    let sarr = !@string_array in
    begin
      assert_equal "Hello" (CArray.get sarr 0);
      assert_equal "world" (CArray.get sarr 1);
    end;

    let iarr = !@int_array in
    begin
      let expected_ints = Bigarray.(Array1.create int32 c_layout 5) in
      for i = 0 to 4 do
	Bigarray.Array1.set expected_ints i (Int32.of_int i)
      done;
      assert_equal expected_ints iarr
    end

end


module Make_stub_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                           and type 'a return = 'a) =
struct
  module N = Functions.Stubs(S)
  open N

  (*
    Read environment variables from the 'environ' global.
  *)
  let test_environ _ =
    let parse_entry s =
      match Str.(bounded_split (regexp "=") s 2), "" with
        [k; v], _ | [k], v -> (String.uppercase_ascii k, v)
      | _ -> Printf.ksprintf failwith "Parsing %S failed" s
    in
    let rec copy_environ acc env =
      match !@env with
        None -> acc
      | Some s -> copy_environ (parse_entry s :: acc) (env +@ 1)
    in
    begin
      let environment = copy_environ [] !@environ in

      assert_equal ~printer:(fun x -> x)
        (List.assoc "HOME" environment)
        (Sys.getenv "HOME")
    end
end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests =
struct
  include Common_tests(Generated_bindings)
  include Make_stub_tests(Generated_bindings)
end


let suite = "Foreign value tests" >:::
  ["retrieving global struct (foreign)"
    >:: Foreign_tests.test_retrieving_struct;

   "global callback function (foreign)"
    >:: Foreign_tests.test_global_callback;

   "retrieving global array (foreign)"
    >:: Foreign_tests.test_retrieving_array;

   "retrieving global struct (stubs)"
    >:: Stub_tests.test_retrieving_struct;

   "retrieving global array (stubs)"
    >:: Stub_tests.test_retrieving_array;

   "global callback function (stubs)"
    >:: Stub_tests.test_global_callback;

   "reading from 'environ' (stubs)"
    >:: Stub_tests.test_environ;
  ]


let _ =
  run_test_tt_main suite
