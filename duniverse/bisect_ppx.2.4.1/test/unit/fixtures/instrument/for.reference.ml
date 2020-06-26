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
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___for___ml
[@@@ocaml.text "/*"]
let () =
  for i = ___bisect_post_visit___ 3 (succ 0) to
    ___bisect_post_visit___ 2 (succ 1) do
    ___bisect_visit___ 1; ___bisect_post_visit___ 0 (print_endline "foo")
  done
