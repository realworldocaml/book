[@@@ocaml.text "/*"]
module Bisect_visit___let___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\018\000\000\000\005\000\000\000\017\000\000\000\017\192\160\000FA\160\000R@\160\000}C\160\001\000\159B" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "let.ml" ~point_count:4 ~point_definitions in
      cb
  end
open Bisect_visit___let___ml
[@@@ocaml.text "/*"]
let () =
  let () =
    let ___bisect_result___ = print_endline "foo" in
    ___bisect_visit___ 1; ___bisect_result___ in
  let ___bisect_result___ = print_endline "bar" in
  ___bisect_visit___ 0; ___bisect_result___
let f () =
  ___bisect_visit___ 3;
  (let () =
     let ___bisect_result___ = print_endline "foo" in
     ___bisect_visit___ 2; ___bisect_result___ in
   print_endline "bar")
