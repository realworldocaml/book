[@@@ocaml.text "/*"]
module Bisect_visit___floating___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\128" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "floating.ml" ~point_count:0 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___floating___ml
[@@@ocaml.text "/*"]
let instrumented = ()
[@@@coverage off]
let not_instrumented = ()
module Nested_1 = struct let also_not_instrumented = () end
[@@@coverage on]
let instrumented_again = ()
module Nested_2 =
  struct
    let instrumented_3 = ()
    [@@@coverage off]
    let not_instrumented_3 = ()
  end
let instrumented_4 = ()
