include Base
include Stdio
include Base_for_tests
include Expect_test_helpers_core

module Quickcheck = struct
  include Core.Quickcheck
  module Bool = Core.Bool
  module Char = Core.Char
  module Int = Core.Int
  module Int32 = Core.Int32
  module Int64 = Core.Int64
  module List = Core.List
  module Nativeint = Core.Nativeint
  module String = Core.String
end

module Core_kernel = struct end
[@@deprecated "[since 1970-01] Don't use Core_kernel in Base tests. Use Base."]
