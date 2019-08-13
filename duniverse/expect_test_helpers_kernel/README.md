September 2018

`Expect_test_helpers_kernel` is a library intended for use with expect
tests, i.e. the `let%expect_test` syntax.  It is an extension of the
`Expect_test_helpers_base` library with functionality dependent on
`Core_kernel`, in particular stable types and allocation tests that
use the `Core_kernel.Gc` module.  

`Expect_test_helpers_kernel` does not use Unix or Async.  It is
suitable for use in JavaScript.  Also see the `Expect_test_helpers`
library, which extends `Expect_test_helpers_kernel` to work in Async
and with additional helper functions that make use of Unix processes.
