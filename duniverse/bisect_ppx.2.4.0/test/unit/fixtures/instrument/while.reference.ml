[@@@ocaml.text "/*"]
module Bisect_visit___while___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\007\000\000\000\003\000\000\000\t\000\000\000\t\160\160nA\160z@" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "while.ml" ~point_count:2 ~point_definitions in
      cb
  end
open Bisect_visit___while___ml
[@@@ocaml.text "/*"]
let () =
  while not true do
    ___bisect_visit___ 1;
    (let ___bisect_result___ = print_endline "foo" in
     ___bisect_visit___ 0; ___bisect_result___)
    done
