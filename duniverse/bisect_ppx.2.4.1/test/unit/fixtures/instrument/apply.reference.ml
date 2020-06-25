[@@@ocaml.text "/*"]
module Bisect_visit___apply___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000g\000\000\000\021\000\000\000Q\000\000\000Q\b\000\000P\000\160\000A@\160\000}A\160\001\000\194B\160\001\000\226C\160\001\001\025D\160\001\001UE\160\001\001sG\160\001\001{F\160\001\001\203H\160\001\001\231I\160\001\002\132J\160\001\002\140K\160\001\002\157L\160\001\002\165M\160\001\002\186N\160\001\002\209O\160\001\003-S\160\001\0032P\160\001\003MR\160\001\003QQ" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "apply.ml" ~point_count:20 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___apply___ml
[@@@ocaml.text "/*"]
let () = ___bisect_post_visit___ 0 (print_endline "foo")
let f () = ___bisect_visit___ 1; print_endline "foo"
let helper () = ___bisect_visit___ 2; print_endline
let () =
  ___bisect_post_visit___ 3 ((___bisect_post_visit___ 3 (helper ())) "foo")
let () = ___bisect_post_visit___ 4 (helper () "foo")
let helper () = ___bisect_visit___ 5; "foo"
let () =
  ___bisect_post_visit___ 7
    (print_endline (___bisect_post_visit___ 6 (helper ())))
let helper ?foo  ~bar  () = ___bisect_visit___ 8; ()
let () = ___bisect_post_visit___ 9 ((helper ~bar:()) @@ ())
let f : unit -> unit = helper ~bar:()
let _ =
  if false
  then (___bisect_visit___ 10; true)
  else if true then (___bisect_visit___ 11; true) else false
let _ =
  if false
  then (___bisect_visit___ 12; true)
  else if true then (___bisect_visit___ 13; true) else false
let _ = true && (___bisect_visit___ 14; true)
let _ = true & (___bisect_visit___ 15; true)
let _ =
  if (___bisect_post_visit___ 19 (print_endline "foo"); false)
  then (___bisect_visit___ 16; true)
  else
    if (___bisect_post_visit___ 18 (print_endline "bar"); true)
    then (___bisect_visit___ 17; true)
    else false
