module Foo =
struct
  let instrumented = ()

  [@@@coverage off]
  let not_instrumented = ()
end

[@@@coverage off]
include Foo
