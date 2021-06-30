module Std = struct
  module Choose_tag = Choose_tag
  module Cst = Cst
  module Fmt = Fmt
  module Lexer = Lexer
  module Matcher = Matcher
  module Reconcile = Reconcile
end
[@@deprecated "[since 2020-03] use [Expect_test_matcher] instead"]

include Std [@@alert "-deprecated"]
