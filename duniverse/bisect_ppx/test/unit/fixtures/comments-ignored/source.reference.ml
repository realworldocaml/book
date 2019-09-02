module Bisect_visit___source___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\011\000\000\000\004\000\000\000\r\000\000\000\r\176\160NB\160`A\160\000@@" in
      let `Staged cb =
        Bisect.Runtime.register_file "source.ml" ~point_count:3
          ~point_definitions in
      cb
  end
open Bisect_visit___source___ml
let f x y =
  ___bisect_visit___ 2;
  if x = y then (___bisect_visit___ 1; x) else (___bisect_visit___ 0; y)
