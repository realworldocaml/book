[@@@ocaml.text "/*"]
module Bisect_visit___let___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\128" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "let.ml" ~point_count:0 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___let___ml
[@@@ocaml.text "/*"]
let instrumented = ()
let not_instrumented = ()[@@coverage off]
let instrumented_again = ()
let instrumented_3 = ()
and not_instrumented_2 = ()[@@coverage off]
let not_instrumented_3 = ()[@@coverage off]
and instrumented_4 = ()
