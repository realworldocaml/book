module Bisect_visit___expr_try___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0000\000\000\000\011\000\000\000)\000\000\000)\b\000\000(\000\160KE\160eD\160\000GA\160\000bB\160\000|@\160\001\000\148C\160\001\000\182I\160\001\000\208H\160\001\000\244F\160\001\001\017G" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_try.ml" ~point_count:10
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_try___ml
let () =
  ___bisect_visit___ 5;
  print_endline "before";
  ___bisect_visit___ 4;
  (try print_endline "abc"; ___bisect_visit___ 1; print_endline "def"
   with
   | _ ->
       (___bisect_visit___ 2;
        print_endline "ABC";
        ___bisect_visit___ 0;
        print_endline "DEF"));
  ___bisect_visit___ 3;
  print_endline "after"
let () =
  ___bisect_visit___ 9;
  print_endline "before";
  ___bisect_visit___ 8;
  (try print_endline "abc"
   with | _ -> (___bisect_visit___ 6; print_endline "ABC"));
  ___bisect_visit___ 7;
  print_endline "after"
