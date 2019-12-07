module Bisect_visit___expr_lazyop___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\r\000\000\000\005\000\000\000\017\000\000\000\017\192\160LA\160W@\160lC\160wB" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_lazyop.ml" ~point_count:4
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_lazyop___ml
let f x y =
  ___bisect_visit___ 1;
  (___bisect_visit___ 1; x > 0) && ((___bisect_visit___ 0; y > 0))
let g x y =
  ___bisect_visit___ 3;
  (___bisect_visit___ 3; x > 0) || ((___bisect_visit___ 2; y > 0))
