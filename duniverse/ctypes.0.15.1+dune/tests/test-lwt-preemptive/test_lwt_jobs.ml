(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Structures = Types.Struct_stubs(Generated_struct_bindings)
module Bindings = Functions.Stubs(Generated_bindings)

(*
  Test the Lwt binding to "sqrt".
 *)
let test_sqrt _ =
  Lwt_main.run
    Lwt.((Bindings.sqrt 9.0).Generated_bindings.lwt >>= fun x ->
         return (assert (x = 3.0)))

(*
  Test that objects remain alive during the Lwt job call.
 *)
let test_object_lifetime _ =
  let call = 
    let open Bigarray in 
    let b = Array1.create int32 c_layout 3 in
    begin
      b.{0} <- 1l;
      b.{1} <- 2l;
      b.{2} <- 3l;
    end;
    (Bindings.sum_int_array
       (bigarray_start array1 b)
       (Unsigned.Size_t.of_int 3)).Generated_bindings.lwt
  in
  begin
    Gc.compact ();
    Gc.compact ();
    Lwt_main.run
      (Lwt.(call >>= fun n ->
            assert_equal 6l n ~printer:Int32.to_string;
            return ()))
  end


(*
  Test that strings remain alive during the Lwt job call.
 *)
let test_string_lifetime _ =
  let s = make Structures.stat in
  let call = (Bindings.stat (Bytes.to_string (Bytes.of_string ".")) (addr s)).Generated_bindings.lwt
  in
  begin
    Gc.compact ();
    Gc.compact ();
    Lwt_main.run
      (Lwt.(call >>= fun i ->
            assert_equal 0 i;
            assert_equal Structures.ifdir
              (PosixTypes.Mode.logand
                 Structures.ifmt
                 (getf s Structures.st_mode));
            return ()))
  end


(*
  Test calling functions with many arguments.
 *)
let test_six_args _ =
  let open Lwt.Infix in
  Lwt_main.run
    ((Bindings.sixargs 1 2 3 4 5 6).Generated_bindings.lwt >>= fun i ->
     assert_equal (1 + 2 + 3 + 4 + 5 + 6) i;
     Lwt.return ())


(*
  Test calling functions with no arguments.
 *)
let test_no_args _ =
  let open Lwt.Infix in
  Lwt_main.run
    ((Bindings.return_10 ()).Generated_bindings.lwt >>= fun i ->
     assert_equal 10 i;
     Lwt.return ())

(*
  Test calling functions that return void.
 *)
let test_return_void _ =
  let open Lwt.Infix in
  Lwt_main.run
    (let x_p = allocate_n ~count:1 int in
     (Bindings.return_void x_p).Generated_bindings.lwt >>= fun () ->
     assert_equal 10 (!@ x_p);
     Lwt.return ())


let suite = "Lwt job tests" >:::
  ["calling sqrt"
    >:: test_sqrt;

   "object lifetime"
    >:: test_object_lifetime;

   "string lifetime"
    >:: test_string_lifetime;

   "functions with many arguments"
    >:: test_six_args;

   "functions with no arguments"
    >:: test_no_args;

   "functions that return void"
    >:: test_return_void;
  ]



let _ =
  run_test_tt_main suite
