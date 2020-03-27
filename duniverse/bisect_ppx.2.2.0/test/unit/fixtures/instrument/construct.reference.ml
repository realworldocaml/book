[@@@ocaml.text "/*"]
module Bisect_visit___construct___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\005\000\000\000\002\000\000\000\005\000\000\000\005\144\160\000n@" in
      let `Staged cb =
        Bisect.Runtime.register_file "construct.ml" ~point_count:1
          ~point_definitions in
      cb
  end
open Bisect_visit___construct___ml
[@@@ocaml.text "/*"]
type foo =
  | A 
  | B of unit 
let _ = A
let _ =
  B
    (let ___bisect_result___ = print_endline "foo" in
     ___bisect_visit___ 0; ___bisect_result___)
