let instrumented = ()

[@@@coverage off]

let not_instrumented = ()

module Nested_1 =
struct
  let also_not_instrumented = ()
end

[@@@coverage on]

let instrumented_again = ()

module Nested_2 =
struct
  let instrumented_3 = ()

  [@@@coverage off]

  let not_instrumented_3 = ()
end

let instrumented_4 = ()
