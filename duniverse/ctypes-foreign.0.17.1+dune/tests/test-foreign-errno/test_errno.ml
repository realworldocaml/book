(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


let us x = if Sys.os_type <> "Win32" then x else "_" ^ x

(*
   Call close() with a bogus file descriptor and check that an exception
   is raised.
*)
let test_errno_exception_raised _ =
  let close = Foreign.foreign (us "close") ~check_errno:true
    (int @-> returning int) in
  assert_raises (Unix.Unix_error(Unix.EBADF, us "close", ""))
    (fun () -> close (-300))
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_int_return_errno_exception_raised _ =
  let unlikely_to_exist =
    if Sys.os_type <> "Win32" then
      "/unlikely_to_exist"
    else
      "C:\\unlikely_to_exist"
  in
  let chdir = Foreign.foreign (us "chdir") ~check_errno:true
    (string @-> returning int) in
  assert_raises (Unix.Unix_error(Unix.ENOENT, us "chdir", ""))
    (fun () -> chdir unlikely_to_exist)
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_errno_no_exception_raised _ =
  let chdir = Foreign.foreign (us "chdir") ~check_errno:true
    (string @-> returning int) in
  assert_equal 0 (chdir (Sys.getcwd ()))

    

let suite = "foreign+errno tests" >:::
  ["Exception from close"
    >:: test_errno_exception_raised;

   "Exception from chdir"
   >:: test_int_return_errno_exception_raised;

   "No exception from chdir"
   >:: test_errno_no_exception_raised;
  ]


let _ =
  if Sys.os_type = "Win32" then
    (*
      Ugly workaround because oUnit raises an error, if there are
      any changes in the environment.

      There are two ways to access the environments on windows:
       - through the native Windows API.
       - through the crt lib. The crt uses the environment for interprocess
         communication, but hides it from the end user.
      Since OCaml 4.07 the native Windows API is used by Unix.environment,
      therefore the tricks of the crt lib are visible.
    *)
    Sys.chdir "."; (* udpate environment *)
  run_test_tt_main suite
