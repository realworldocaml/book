[@@@ocaml.text "/*"]
module Bisect_visit___new___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\020\000\000\000\005\000\000\000\017\000\000\000\017\192\160\000b@\160\001\000\139A\160\001\000\185B\160\001\000\246C" in
      let `Staged cb =
        Bisect.Runtime.register_file "new.ml" ~point_count:4
          ~point_definitions in
      cb
  end
open Bisect_visit___new___ml
[@@@ocaml.text "/*"]
class foo = object  end
class bar ()  () = object  end
let _ =
  let ___bisect_result___ = new foo in
  ___bisect_visit___ 0; ___bisect_result___
let _ =
  let ___bisect_result___ = (new bar) () () in
  ___bisect_visit___ 1; ___bisect_result___
let f () = ___bisect_visit___ 2; new foo
let f () = ___bisect_visit___ 3; (new bar) () ()
