[@@@ocaml.text "/*"]
module Bisect_visit___send___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000.\000\000\000\n\000\000\000%\000\000\000%\b\000\000$\000\160j@\160\000KA\160\000sB\160\001\000\157C\160\001\000\203D\160\001\001\bE\160\001\001EF\160\001\001[G\160\001\001cH" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "send.ml" ~point_count:9 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___send___ml
[@@@ocaml.text "/*"]
let foo =
  object
    method bar = ___bisect_visit___ 0; ()
    method baz () () = ___bisect_visit___ 1; ()
  end
let () = ___bisect_post_visit___ 2 foo#bar
let () = ___bisect_post_visit___ 3 (foo#baz () ())
let f () = ___bisect_visit___ 4; foo#bar
let f () = ___bisect_visit___ 5; foo#baz () ()
let helper () = ___bisect_visit___ 6; foo
let () =
  ___bisect_post_visit___ 8 (___bisect_post_visit___ 7 (helper ()))#bar
