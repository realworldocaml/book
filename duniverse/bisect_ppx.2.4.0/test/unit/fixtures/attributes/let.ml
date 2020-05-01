let instrumented = ()

let not_instrumented = ()
  [@@coverage off]

let instrumented_again = ()

let instrumented_3 = ()
and not_instrumented_2 = ()
  [@@coverage off]

let not_instrumented_3 = ()
  [@@coverage off]
and instrumented_4 = ()
