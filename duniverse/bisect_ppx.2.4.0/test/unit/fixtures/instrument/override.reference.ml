[@@@ocaml.text "/*"]
module Bisect_visit___override___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\t\000\000\000\003\000\000\000\t\000\000\000\t\160\160\000OA\160\000c@" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "override.ml" ~point_count:2 ~point_definitions in
      cb
  end
open Bisect_visit___override___ml
[@@@ocaml.text "/*"]
let _ =
  object
    val mutable foo = ()
    method bar =
      ___bisect_visit___ 1;
      {<foo =
          (let ___bisect_result___ = print_endline "foo" in
           ___bisect_visit___ 0; ___bisect_result___)>}
  end
