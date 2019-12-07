module Bisect_visit___expr_comment___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\n\000\000\000\004\000\000\000\r\000\000\000\r\176\160H@\160SA\160fB" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_comment.ml" ~point_count:3
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_comment___ml
let x = ___bisect_visit___ 0; 3
let y = ___bisect_visit___ 1; [1; 2; 3]
let z = ___bisect_visit___ 2; [|1;2;3|]
let f x = print_endline x
let f' x = let x' = String.uppercase x in print_endline x'
let g x y z = (x + y) * z
let g' x y = print_endline x; print_endline y
