(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-35"]
open OUnit2
open Ctypes


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  let test_object_lifetime _ =
    let iters = 20000 in
    let l = [(); (); (); (); (); (); (); (); (); ()] in
    let alloc =       (fun () ->
	for i = 0 to iters do
	  for i = 0 to 200; do ignore (Array.make 10 ()) done;
	  ignore (Array.make 1000 ());
	  if i mod 1000 = 0 then (Gc.compact ());
	done) in
    let allocators =
      List.map (Thread.create alloc) l
    in
    let size = 100 in
    let mutate () =
      for i = 0 to iters do
	check_ones
	  (CArray.start (CArray.make int ~initial:1 size))
	  (Unsigned.Size_t.of_int size);
	for i = 0 to 200; do
	  ignore (Array.make 10 ())
	done;
      done
    in
    let mutators =
      List.map (Thread.create mutate) l
    in
    List.iter Thread.join allocators;
    List.iter Thread.join mutators
      
end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)

let suite = "Lifetime tests" >:::
  ["objects persist throughout C calls (foreign)"
    >:: Foreign_tests.test_object_lifetime;

   "objects persist throughout C calls (stubs)"
    >:: Stub_tests.test_object_lifetime;
  ]

let _ =
  run_test_tt_main suite
