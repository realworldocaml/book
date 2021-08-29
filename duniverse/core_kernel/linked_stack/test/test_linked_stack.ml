open! Core_kernel
open! Linked_stack

let%test_module _ = (module Base_test_helpers.Test_container.Test_S1 (Linked_stack))

module Test_stack = Base_test_helpers.Test_stack

let%test_module _ = (module Test_stack.Test (Test_stack.Debug (Linked_stack)))
