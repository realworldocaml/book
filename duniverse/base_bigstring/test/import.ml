include Base
include Stdio
include Base_for_tests
include Expect_test_helpers_core

module Quickcheck = struct
  include Core_kernel.Quickcheck
  module Bool = Core_kernel.Bool
  module Char = Core_kernel.Char
  module Int = Core_kernel.Int
  module Int32 = Core_kernel.Int32
  module Int64 = Core_kernel.Int64
  module List = Core_kernel.List
  module Nativeint = Core_kernel.Nativeint
  module String = Core_kernel.String
end

module Core_kernel = struct end
[@@deprecated "[since 1970-01] Don't use Core_kernel in Base tests. Use Base."]
