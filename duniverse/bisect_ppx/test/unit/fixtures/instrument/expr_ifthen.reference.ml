module Bisect_visit___expr_ifthen___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0004\000\000\000\012\000\000\000-\000\000\000-\b\000\000,\000\160KB\160\\A\160{@\160\000[G\160\000lF\160\001\000\129D\160\001\000\158E\160\001\000\179C\160\001\000\209J\160\001\000\226I\160\001\000\247H" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_ifthen.ml" ~point_count:11
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_ifthen___ml
let () =
  ___bisect_visit___ 2;
  if true
  then (___bisect_visit___ 1; print_endline "abc")
  else (___bisect_visit___ 0; print_endline "def")
let () =
  ___bisect_visit___ 7;
  if true
  then
    (___bisect_visit___ 6;
     print_string "abc";
     ___bisect_visit___ 4;
     print_newline ())
  else
    (___bisect_visit___ 5;
     print_string "def";
     ___bisect_visit___ 3;
     print_newline ())
let () =
  ___bisect_visit___ 10;
  if true
  then
    (___bisect_visit___ 9;
     print_string "abc";
     ___bisect_visit___ 8;
     print_newline ())
