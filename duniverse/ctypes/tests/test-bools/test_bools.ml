(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Common(S)

  (*
    Test passing bool values.
  *)
  let test_passing_bools _ =
    begin
      assert_equal false (M.bool_and false false);
      assert_equal false (M.bool_and false true);
      assert_equal false (M.bool_and true false);
      assert_equal true (M.bool_and true true);
    end
end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Bool tests" >:::
  ["passing bools (foreign)"
   >:: Foreign_tests.test_passing_bools;

   "passing bools (stubs)"
   >:: Stub_tests.test_passing_bools;
  ]


let _ =
  run_test_tt_main suite
