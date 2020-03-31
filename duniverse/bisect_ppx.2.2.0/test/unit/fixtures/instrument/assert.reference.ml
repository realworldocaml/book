[@@@ocaml.text "/*"]
module Bisect_visit___assert___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\006\000\000\000\002\000\000\000\005\000\000\000\005\144\160\001\000\163@" in
      let `Staged cb =
        Bisect.Runtime.register_file "assert.ml" ~point_count:1
          ~point_definitions in
      cb
  end
open Bisect_visit___assert___ml
[@@@ocaml.text "/*"]
let () = assert true
let f () = assert true
let () =
  assert
    ((let ___bisect_result___ = print_endline "foo" in
      ___bisect_visit___ 0; ___bisect_result___);
     true)
let () = assert false
let f = function | `A -> assert false
let () = match `A with | `A -> assert false
let () = try () with | Exit -> assert false
let () = if true then assert false else assert false
let () = while false do assert false done
let () = for i = 1 to 0 do assert false done
let _ = lazy (assert false)
let _ = object method foo = assert false end
