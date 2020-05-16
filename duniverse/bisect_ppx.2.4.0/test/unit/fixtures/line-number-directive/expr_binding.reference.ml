[@@@ocaml.text "/*"]
module Bisect_visit___expr_binding___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\028\000\000\000\007\000\000\000\025\000\000\000\025\224\160}@\160\000vB\160\001\000\151A\160\001\000\183C\160\001\000\211E\160\001\000\230D" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "expr_binding.ml" ~point_count:6 ~point_definitions in
      cb
  end
open Bisect_visit___expr_binding___ml
[@@@ocaml.text "/*"]
let x = 3
let y = [1; 2; 3]
let z = [|1;2;3|]
let f x = ___bisect_visit___ 0; print_endline x
let f' x =
  ___bisect_visit___ 2;
  (let x' =
     let ___bisect_result___ = String.uppercase x in
     ___bisect_visit___ 1; ___bisect_result___ in
   print_endline x')
let g x y z = ___bisect_visit___ 3; (x + y) * z
let g' x y =
  ___bisect_visit___ 5;
  (let ___bisect_result___ = print_endline x in
   ___bisect_visit___ 4; ___bisect_result___);
  print_endline y
