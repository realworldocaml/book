[@@@ocaml.text "/*"]
module Bisect_visit___pack___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\128" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "pack.ml" ~point_count:0 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___pack___ml
[@@@ocaml.text "/*"]
module type EMPTY  = sig  end
let _ = ((module struct  end) : (module EMPTY))
