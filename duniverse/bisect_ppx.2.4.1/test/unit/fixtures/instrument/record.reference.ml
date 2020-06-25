[@@@ocaml.text "/*"]
module Bisect_visit___record___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\020\000\000\000\005\000\000\000\017\000\000\000\017\192\160\000]@\160\001\000\163A\160\001\000\191B\160\001\000\222C" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "record.ml" ~point_count:4 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___record___ml
[@@@ocaml.text "/*"]
type foo = {
  bar: unit ;
  baz: unit }
let initial =
  { bar = (___bisect_post_visit___ 0 (print_endline "foo")); baz = () }
let helper () = ___bisect_visit___ 1; initial
let final =
  {
    (___bisect_post_visit___ 2 (helper ())) with
    bar = (___bisect_post_visit___ 3 (print_endline "bar"))
  }
