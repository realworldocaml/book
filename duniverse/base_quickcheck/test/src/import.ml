include (Base : module type of Base with module Export := Base.Export)
include Expect_test_helpers_kernel
include Base_quickcheck
include Base_quickcheck_test_helpers
include Generator.Let_syntax
