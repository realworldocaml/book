[@@@ocaml.text "/*"]
module Bisect_visit___for___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\014\000\000\000\005\000\000\000\017\000\000\000\017\192\160dC\160pB\160|A\160\000H@" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "for.ml" ~point_count:4 ~point_definitions in
      cb
  end
open Bisect_visit___for___ml
[@@@ocaml.text "/*"]
let () =
  for i =
    let ___bisect_result___ = succ 0 in
    ___bisect_visit___ 3; ___bisect_result___ to
    let ___bisect_result___ = succ 1 in
    ___bisect_visit___ 2; ___bisect_result___ do
    ___bisect_visit___ 1;
    (let ___bisect_result___ = print_endline "foo" in
     ___bisect_visit___ 0; ___bisect_result___)
  done
