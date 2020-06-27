[@@@ocaml.text "/*"]
module Bisect_visit___setfield___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\020\000\000\000\005\000\000\000\017\000\000\000\017\192\160\000m@\160\001\000\164A\160\001\000\186C\160\001\000\211B" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "setfield.ml" ~point_count:4 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___setfield___ml
[@@@ocaml.text "/*"]
type foo = {
  mutable bar: unit }
let baz = { bar = () }
let () = baz.bar <- (___bisect_post_visit___ 0 (print_endline "foo"))
let helper () = ___bisect_visit___ 1; baz
let () =
  (___bisect_post_visit___ 3 (helper ())).bar <-
    (___bisect_post_visit___ 2 (print_endline "foo"))
