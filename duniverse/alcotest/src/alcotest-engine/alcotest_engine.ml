module V1 = struct
  module Test = Test
  module Core = Core.V1
  module Cli = Cli.V1
end

module Monad = Monad
module Platform = Platform

module Private = struct
  module Pp = Pp
end
