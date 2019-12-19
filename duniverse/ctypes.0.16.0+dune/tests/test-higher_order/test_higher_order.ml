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
  module M = Functions.Stubs(S)
  open M

  (*
     Call a C function of type

         int (int ( * )(int, int), int, int)

     passing various OCaml functions of type

         int -> int -> int

     as the first argument.
  *)
  let test_higher_order_basic _ =
    (* higher_order_1 f x y returns true iff f x y == x + y *)
    assert_equal 1 (higher_order_1 ( + ) 2 3);
    assert_equal 0 (higher_order_1 ( * ) 2 3);
    assert_equal 0 (higher_order_1 min 2 3);
    assert_equal 1 (higher_order_1 min (-3) 0)


  (*
    Call a C function of type 

         int (int ( * )(int ( * )(int, int), int, int),
              int ( * )(int, int),
              int, int)

    passing OCaml functions of type

         (int -> int -> int) -> int -> int -> int
         int -> int -> int

    as the first and second arguments.
  *)
  let test_higher_higher_order _ =
    let acceptor op x y = op x (op x y) in
    assert_equal 10 (higher_order_3 acceptor ( + ) 3 4);
    assert_equal 36 (higher_order_3 acceptor ( * ) 3 4)

  (*
    Call a C function of type
        int (char( * )(void))
    and check that the char returned by the function pointer is handled
    correctly
  *)
  let test_function_pointer_returning_char _ =
    assert_equal 1 (callback_returns_char_a (fun () -> 'a'))

  (*
    Call a C function of type

         int ( *(int))(int)

    (i.e. a function that returns a pointer-to-function) and ensure that we can
    call the returned function from OCaml.
  *)
  let test_returning_pointer_to_function _ =
    let add = returning_funptr 0 in

    let times = returning_funptr 1 in

    assert_equal 22 (add 10 12);
    assert_equal 15 (times 3 5);
    assert_equal 101 (add 100 1);
    assert_equal 0 (times 0 12)


  (*
    Call a C function of type

         int (int ( * ( * )(int))(int), int)

    (i.e. a function whose first argument is a pointer-to-function
    returning a pointer-to-function.)
  *)
  let test_callback_returns_pointer_to_function _ =
    let callback = function
      | 0 -> ( + ) 10
      | 1 -> ( * ) 13
      | _ -> invalid_arg "callback"
    in

    assert_equal 280 (callback_returns_funptr callback 0)

  (*
    Call an OCaml function through a C function pointer of type

         void ( * )(void)
  *)
  let test_zero_argument_callbacks _ =
    let counter = ref 0 in
    let callback () = let c = !counter in incr counter; c in
    let () = register_callback callback in

    begin
      assert_equal !counter 0;
      call_registered_callback 5 !counter;
      assert_equal !counter 5;
      call_registered_callback 3 !counter;
      assert_equal !counter 8;
    end


  (*
    Retrieve a function pointer from C and pass it back to C using
    static_funptr.
  *)
  let test_static_funptr _ =
    let add = returning_funptr_static 0
    and mul = returning_funptr_static 1 in

    begin
      assert_equal 1 (higher_order_1_static add 2 3);
      assert_equal 0 (higher_order_1_static mul 2 3);
    end
end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Higher-order tests" >:::
  ["test_higher_order_basic (foreign)"
   >:: Foreign_tests.test_higher_order_basic;

   "test_higher_order_basic (stubs)"
   >:: Stub_tests.test_higher_order_basic;

   "test_higher_higher_order (foreign)"
   >:: Foreign_tests.test_higher_higher_order;

   "test_higher_higher_order (stubs)"
   >:: Stub_tests.test_higher_higher_order;

   "test_function_pointer_returning_char (stubs)"
   >:: Stub_tests.test_function_pointer_returning_char;

   "test_function_pointer_returning_char (foreign)"
   >:: Foreign_tests.test_function_pointer_returning_char;

   "test_returning_pointer_to_function (foreign)"
   >:: Foreign_tests.test_returning_pointer_to_function;

   "test_returning_pointer_to_function (stubs)"
   >:: Stub_tests.test_returning_pointer_to_function;

   "test_callback_returns_pointer_to_function (foreign)"
   >:: Foreign_tests.test_callback_returns_pointer_to_function;

   "test_callback_returns_pointer_to_function (stubs)"
   >:: Stub_tests.test_callback_returns_pointer_to_function;

   "test_zero_argument_callbacks (foreign)"
   >:: Foreign_tests.test_zero_argument_callbacks;

   "test_zero_argument_callbacks (stubs)"
   >:: Stub_tests.test_zero_argument_callbacks;

   "test_static_funptr (foreign)"
   >:: Foreign_tests.test_static_funptr;

   "test_static_funptr (stubs)"
   >:: Stub_tests.test_static_funptr;
  ]


let _ =
  run_test_tt_main suite
