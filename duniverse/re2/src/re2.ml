include Regex
module Parser = Parser

module Std = struct
  module Parser = Parser
  module Re2 = Regex
end
[@@deprecated "[since 2018-02] Use [Re2.foo] instead of [Re2.Std.Re2.foo] \
               or [Re2.Parser.foo] instead of [Re2.Std.Parser.foo]. \
               The [Re2.Std] sub-module is no longer needed."]

module Regex = Regex
[@@deprecated "[since 2018-02] Use [Re2.foo] instead of [Re2.Regex.foo]."]
