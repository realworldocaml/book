[@@@ocaml.text "/*"]
module Bisect_visit___field___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\011\000\000\000\003\000\000\000\t\000\000\000\t\160\160\001\000\133@\160\001\000\155A" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "field.ml" ~point_count:2 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___field___ml
[@@@ocaml.text "/*"]
type foo = {
  bar: unit }
let baz = { bar = () }
let () = baz.bar
let helper () = ___bisect_visit___ 0; baz
let () = (___bisect_post_visit___ 1 (helper ())).bar
