[@@@ocaml.text "/*"]
module Bisect_visit___letop_408___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0009\000\000\000\r\000\000\0001\000\000\0001\b\000\0000\000\160Q@\160fA\160|B\160\000mD\160\000zC\160\001\000\178G\160\001\000\210F\160\001\000\223E\160\001\001\027K\160\001\001(J\160\001\001>I\160\001\001KH" in
      let `Staged cb =
        Bisect.Runtime.register_file "letop_408.ml" ~point_count:12
          ~point_definitions in
      cb
  end
open Bisect_visit___letop_408___ml
[@@@ocaml.text "/*"]
let ( let* ) x f = ___bisect_visit___ 0; f x
let ( and* ) x y = ___bisect_visit___ 1; (x, y)
let return x = ___bisect_visit___ 2; x
let () =
  let* () =
    let ___bisect_result___ = print_endline "foo" in
    ___bisect_visit___ 4; ___bisect_result___
   in ___bisect_visit___ 3; return ()
let () =
  let* () =
    let ___bisect_result___ = print_endline "foo" in
    ___bisect_visit___ 7; ___bisect_result___
  and* () =
    let ___bisect_result___ = print_endline "bar" in
    ___bisect_visit___ 6; ___bisect_result___ in
  ___bisect_visit___ 5; return ()
let () =
  let* () =
    let ___bisect_result___ = print_endline "foo" in
    ___bisect_visit___ 11; ___bisect_result___
   in
  ___bisect_visit___ 10;
  (let* () =
     let ___bisect_result___ = print_endline "bar" in
     ___bisect_visit___ 9; ___bisect_result___
    in ___bisect_visit___ 8; return ())
