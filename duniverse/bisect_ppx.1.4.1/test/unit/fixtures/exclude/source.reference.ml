module Bisect_visit___source___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\t\000\000\000\003\000\000\000\t\000\000\000\t\160\160\000EA\160\000[@" in
      let `Staged cb =
        Bisect.Runtime.register_file "source.ml" ~point_count:2
          ~point_definitions in
      cb
  end
open Bisect_visit___source___ml
let f1 x y = if x = y then x + y else x - y
let g s =
  ___bisect_visit___ 1;
  for i = 1 to 5 do (___bisect_visit___ 0; print_endline s) done
let f2 b x = if b then x * x else x
