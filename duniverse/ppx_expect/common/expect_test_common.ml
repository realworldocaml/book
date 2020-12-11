module Std = struct
  module File = File
  module Expectation = Expectation
end
[@@deprecated "[since 2020-03] use [Expect_test_common] instead"]

include Std [@@alert "-deprecated"]
