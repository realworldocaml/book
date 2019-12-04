open! Core_kernel
open! Import
open! Linked_stack
include Test_container.Test_S1 (Linked_stack)
module Test_stack = Base_test.Test_stack
include Test_stack.Test (Test_stack.Debug (Linked_stack))
