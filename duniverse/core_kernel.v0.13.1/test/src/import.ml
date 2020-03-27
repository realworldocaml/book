open! Core_kernel
include Expect_test_helpers_kernel
module Test_container = Base_test.Test_container
module Variant = Variantslib.Variant

let () = Sexp.of_int_style := `Underscores
