include Base

include (
  Base_quickcheck :
    module type of Base_quickcheck with module Export := Base_quickcheck.Export)

include Base_quickcheck_test_helpers
include Expect_test_helpers_kernel
