module Bisect_visit___expr_sequence___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000I\000\000\000\016\000\000\000=\000\000\000=\b\000\000<\000\160K@\160kB\160\000BA\160\000bE\160\000yD\160\001\000\144C\160\001\000\176J\160\001\000\211H\160\001\000\226F\160\001\001\002G\160\001\001&I\160\001\0019N\160\001\001KL\160\001\001]M\160\001\001cK" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_sequence.ml" ~point_count:15
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_sequence___ml
let () = ___bisect_visit___ 0; print_endline "abc"
let () =
  ___bisect_visit___ 2;
  print_endline "abc";
  ___bisect_visit___ 1;
  print_endline "def"
let () =
  ___bisect_visit___ 5;
  print_endline "abc";
  ___bisect_visit___ 4;
  print_endline "def";
  ___bisect_visit___ 3;
  print_endline "ghi"
let () =
  ___bisect_visit___ 10;
  (print_endline "abc";
   ___bisect_visit___ 8;
   (function
    | 0 -> (___bisect_visit___ 6; print_endline "def")
    | _ -> (___bisect_visit___ 7; print_endline "ghi"))) |>
    ((___bisect_visit___ 9; ignore))
let () =
  ___bisect_visit___ 14;
  (let f ?maybe  () = ___bisect_visit___ 12; ignore maybe in
   ___bisect_visit___ 13; () |> ((___bisect_visit___ 11; f)))
