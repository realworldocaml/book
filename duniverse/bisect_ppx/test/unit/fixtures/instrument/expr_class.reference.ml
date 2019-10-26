module Bisect_visit___expr_class___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000X\000\000\000\019\000\000\000I\000\000\000I\b\000\000H\000\160c@\160vA\160\000LB\160\000eC\160\000\127D\160\001\000\164E\160\001\000\207F\160\001\000\226G\160\001\000\248I\160\001\001\018H\160\001\001+K\160\001\0018J\160\001\001WM\160\001\001oL\160\001\001\141N\160\001\001\221O\160\001\002\017P\160\001\0026Q" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_class.ml" ~point_count:18
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_class___ml
class c =
  object
    val mutable x = ___bisect_visit___ 0; 0
    method get_x = ___bisect_visit___ 1; x
    method set_x x' = ___bisect_visit___ 2; x <- x'
    method print = ___bisect_visit___ 3; print_int x
    initializer ___bisect_visit___ 4; print_endline "created"
  end
let i = ___bisect_visit___ 5; new c
class c' =
  object
    val mutable x = ___bisect_visit___ 6; 0
    method get_x = ___bisect_visit___ 7; x
    method set_x x' =
      ___bisect_visit___ 9;
      print_endline "modified";
      ___bisect_visit___ 8;
      x <- x'
    method print =
      ___bisect_visit___ 11;
      print_int x;
      ___bisect_visit___ 10;
      print_newline ()
    initializer
      ___bisect_visit___ 13;
      print_string "created";
      ___bisect_visit___ 12;
      print_newline ()
  end
let i = ___bisect_visit___ 14; new c
class virtual c'' =
  object method virtual  get_x : int method set_x = ___bisect_visit___ 15; ()
  end
class p (v : int) = object method get_v = ___bisect_visit___ 16; v end
class p' = object inherit  ((p) (___bisect_visit___ 17; 0)) end
