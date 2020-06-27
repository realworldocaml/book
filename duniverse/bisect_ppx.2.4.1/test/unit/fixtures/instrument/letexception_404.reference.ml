[@@@ocaml.text "/*"]
module Bisect_visit___letexception_404___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\t\000\000\000\003\000\000\000\t\000\000\000\t\160\160\000E@\160\000pA" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "letexception_404.ml" ~point_count:2 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___letexception_404___ml
[@@@ocaml.text "/*"]
let () = let exception E  in ___bisect_post_visit___ 0 (print_endline "bar")
let f () = ___bisect_visit___ 1; (let exception E  in print_endline "bar")
