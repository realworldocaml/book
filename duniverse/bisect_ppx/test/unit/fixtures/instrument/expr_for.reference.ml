module Bisect_visit___expr_for___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000/\000\000\000\011\000\000\000)\000\000\000)\b\000\000(\000\160KE\160eD\160|B\160\000UA\160\000n@\160\001\000\140C\160\001\000\174I\160\001\000\200H\160\001\000\223F\160\001\000\253G" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_for.ml" ~point_count:10
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_for___ml
let () =
  ___bisect_visit___ 5;
  print_endline "before";
  ___bisect_visit___ 4;
  for _i = 1 to 3 do
    (___bisect_visit___ 2;
     print_endline "abc";
     ___bisect_visit___ 1;
     print_endline "def";
     ___bisect_visit___ 0;
     print_endline "ghi")
  done;
  ___bisect_visit___ 3;
  print_endline "after"
let () =
  ___bisect_visit___ 9;
  print_endline "before";
  ___bisect_visit___ 8;
  for _i = 1 to 3 do (___bisect_visit___ 6; print_endline "abc") done;
  ___bisect_visit___ 7;
  print_endline "after"
