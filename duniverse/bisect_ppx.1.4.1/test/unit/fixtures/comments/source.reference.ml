module Bisect_visit___source___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000#\000\000\000\b\000\000\000\029\000\000\000\029\240\160\000uB\160\001\000\134A\160\001\000\158@\160\001\001\181C\160\001\001\197D\160\001\001\229E\160\001\002\007F" in
      let `Staged cb =
        Bisect.Runtime.register_file "source.ml" ~point_count:7
          ~point_definitions in
      cb
  end
open Bisect_visit___source___ml
let f1 x y = if x = y then x + y else x - y
let g s =
  ___bisect_visit___ 2;
  if true
  then
    (___bisect_visit___ 1;
     for i = 1 to 5 do (___bisect_visit___ 0; print_endline s) done)
  else assert false
let f2 b x = if b then x * x else x
let s1 = ___bisect_visit___ 3; "\\\\"
let s2 = ___bisect_visit___ 4; '"'
let s3 = ___bisect_visit___ 5; '"'
let s4 = ___bisect_visit___ 6; "a\"a"
