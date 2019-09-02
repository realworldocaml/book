module Bisect_visit___expr_binding___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000E\000\000\000\016\000\000\000=\000\000\000=\b\000\000<\000\160H@\160SA\160fB\160}C\160\000[F\160\000dD\160\000|E\160\001\000\156G\160\001\000\184I\160\001\000\203H\160\001\000\231N\160\001\000\241L\160\001\000\248M\160\001\001\003J\160\001\001\nK" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_binding.ml" ~point_count:15
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_binding___ml
let x = ___bisect_visit___ 0; 3
let y = ___bisect_visit___ 1; [1; 2; 3]
let z = ___bisect_visit___ 2; [|1;2;3|]
let f x = ___bisect_visit___ 3; print_endline x
let f' x =
  ___bisect_visit___ 6;
  (let x' = ___bisect_visit___ 4; String.uppercase x in
   ___bisect_visit___ 5; print_endline x')
let g x y z = ___bisect_visit___ 7; (x + y) * z
let g' x y =
  ___bisect_visit___ 9;
  print_endline x;
  ___bisect_visit___ 8;
  print_endline y
let () =
  ___bisect_visit___ 14;
  (let f _ = ___bisect_visit___ 12; 0 in
   ___bisect_visit___ 13;
   (let _g _ = ___bisect_visit___ 10; 1 in
    ___bisect_visit___ 11; print_endline (string_of_int (f ()))))
