(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign

(* Explicitly raise on leaked funptrs. *)

let detect_funptr_leaks ?(expected_number_of_leaked_funptrs=0) f =
  let number_of_leaked_funptrs = ref 0 in
  Foreign.report_leaked_funptr := (fun _msg ->
    number_of_leaked_funptrs := !number_of_leaked_funptrs + 1);
  Gc.full_major ();
  Gc.full_major ();
  assert_equal 0 !number_of_leaked_funptrs;
  let res = f () in
  Gc.full_major ();
  Gc.full_major ();
  assert_equal expected_number_of_leaked_funptrs !number_of_leaked_funptrs;
  res
;;


module Callback = Functions.Callback
module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)

  let make_f () : (int -> int) * ([`Live|`Released] -> unit) =
    let closure_status = ref `Live in
    let f = !(ref (+)) 1 in
    Gc.finalise (fun _ -> closure_status := `Released) f;
    f, (fun status -> 
      Gc.full_major ();
      Gc.full_major ();
      assert_equal status !closure_status)
  ;;

  let test_of_fun_and_free _ =
    detect_funptr_leaks (fun () ->
      let f, assert_closure =
        let f, assert_closure = make_f () in
        Callback.of_fun f, assert_closure
      in
      assert_closure `Live;
      assert_equal 3 (M.call_dynamic_funptr f 2);
      Callback.free f;
      assert_closure `Released)
  ;;

  let test_of_fun_and_leak _ =
    let assert_closure = 
      detect_funptr_leaks ~expected_number_of_leaked_funptrs:1 (fun () ->
        let f, assert_closure =
          let f, assert_closure = make_f () in
          Callback.of_fun f, assert_closure
        in
        assert_closure `Live;
        assert_equal 3 (M.call_dynamic_funptr f 2);
        assert_closure)
    in
    assert_closure `Live
  ;;

  let test_with_fun _ =
    detect_funptr_leaks (fun () ->
      let assert_closure = 
        let f, assert_closure = make_f () in
        assert_equal 3 (Callback.with_fun f
                          (fun f -> M.call_dynamic_funptr f 2));
        assert_closure
      in
      assert_closure `Released)
  ;;

  let test_opt_none _ =
    detect_funptr_leaks (fun () ->
      assert_equal 0 (M.call_dynamic_funptr_opt None 2))
  ;;

  let test_opt_some _ =
    detect_funptr_leaks (fun () ->
      assert_equal 3 (Callback.with_fun ((+) 1)
                        (fun f -> M.call_dynamic_funptr_opt (Some f) 2)))
  ;;

  let test_save_and_free _ =
    detect_funptr_leaks (fun () ->
      M.save_dynamic_funptr_opt (None);
      assert_equal 0 (M.call_saved_dynamic_funptr 2);
      let f, assert_closure =
        let f, assert_closure = make_f () in
        Callback.of_fun f, assert_closure
      in
      assert_closure `Live;
      M.save_dynamic_funptr_opt (Some f);
      assert_closure `Live;
      assert_equal 3 (M.call_saved_dynamic_funptr 2);
      assert_closure `Live;
      Callback.free f;
      assert_closure `Released;
    );
    M.save_dynamic_funptr_opt (None)
  ;;

  let test_save_and_leak _ =
    let assert_closure = 
      detect_funptr_leaks ~expected_number_of_leaked_funptrs:1 (fun () ->
      M.save_dynamic_funptr_opt (None);
      assert_equal 0 (M.call_saved_dynamic_funptr 2);
      let f, assert_closure =
        let f, assert_closure = make_f () in
        Callback.of_fun f, assert_closure
      in
      assert_closure `Live;
      M.save_dynamic_funptr_opt (Some f);
      assert_closure)
    in
    (* Technically this is undefined behaviour, but the library should handle this in
       the least surprising way possible (leaking but not crashing). *)
    assert_closure `Live;
    assert_equal 3 (M.call_saved_dynamic_funptr 2);
    M.save_dynamic_funptr_opt (None);
    assert_equal 0 (M.call_saved_dynamic_funptr 2);
    assert_closure `Live;
  ;;

  let test_struct _ =
    detect_funptr_leaks (fun () ->
      let t = make M.simple_closure in
      let f = Callback.of_fun ((+)1) in
      setf t M.simple_closure_f f;
      setf t M.simple_closure_n 2;
      assert_equal 3 (M.call_dynamic_funptr_struct t);
      Callback.free f
    )

  let test_struct_ptr _ =
    detect_funptr_leaks (fun () ->
      let t = make M.simple_closure in
      let p = addr t in
      let f = Callback.of_fun ((+)1) in
      p |-> M.simple_closure_f <-@ f;
      p |-> M.simple_closure_n <-@ 2;
      assert_equal 3 (M.call_dynamic_funptr_struct_ptr p);
      Callback.free f
    )

end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)

let suite = "Dynamic-Funptr tests" >:::
  ["test_of_fun_and_free (foreign)"
   >:: Foreign_tests.test_of_fun_and_free;

   "test_of_fun_and_free (stubs)"
   >:: Stub_tests.test_of_fun_and_free;

   "test_of_fun_and_leak (foreign)"
   >:: Foreign_tests.test_of_fun_and_leak;

   "test_of_fun_and_leak (stubs)"
   >:: Stub_tests.test_of_fun_and_leak;

   "test_with_fun (foreign)"
   >:: Foreign_tests.test_with_fun;

   "test_with_fun (stubs)"
   >:: Stub_tests.test_with_fun;

   "test_opt_none (foreign)"
   >:: Foreign_tests.test_opt_none;

   "test_opt_none (stubs)"
   >:: Stub_tests.test_opt_none;

   "test_opt_some (foreign)"
   >:: Foreign_tests.test_opt_some;

   "test_opt_some (stubs)"
   >:: Stub_tests.test_opt_some;

   "test_save_and_free (foreign)"
   >:: Foreign_tests.test_save_and_free;

   "test_save_and_free (stubs)"
   >:: Stub_tests.test_save_and_free;

   "test_save_and_leak (foreign)"
   >:: Foreign_tests.test_save_and_leak;

   "test_save_and_leak (stubs)"
   >:: Stub_tests.test_save_and_leak;

   "test_struct (foreign)"
   >:: Foreign_tests.test_struct;

   "test_struct (stubs)"
   >:: Stub_tests.test_struct;

   "test_struct_ptr (foreign)"
   >:: Foreign_tests.test_struct_ptr;

   "test_struct_ptr (stubs)"
   >:: Stub_tests.test_struct_ptr;
  ]

let _ =
  run_test_tt_main suite

