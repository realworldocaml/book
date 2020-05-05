(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign

let missing = "_60d2dd04_1b66_4b79_a2ea_8375157da563"

let test_missing _ = 
  let miss = foreign missing ~stub:true (int @-> int @-> (returning int)) in
  begin try ignore (miss 2 3); assert_failure "should raise" with _exn -> () end;
  try
    let _miss = foreign missing ~stub:false (int @-> int @-> (returning int)) in
    assert_failure "should raise"
  with _exn -> ()

  
let suite = 
  "Foreign value stubs" >::: 
  [
    "missing symbols"
    >:: test_missing;
  ]

let _ = run_test_tt_main suite
