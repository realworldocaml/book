[@@@ocaml.text "/*"]
module Bisect_visit___try___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0006\000\000\000\012\000\000\000-\000\000\000-\b\000\000,\000\160lC\160\127A\160\000NB\160\000g@\160\001\000\149G\160\001\000\169F\160\001\000\188D\160\001\000\203E\160\001\001 I\160\001\001'J\160\001\001@H" in
      let `Staged cb =
        Bisect.Runtime.register_file "try.ml" ~point_count:11
          ~point_definitions in
      cb
  end
open Bisect_visit___try___ml
[@@@ocaml.text "/*"]
let () =
  try
    let ___bisect_result___ = print_endline "foo" in
    ___bisect_visit___ 3; ___bisect_result___
  with | Exit -> (___bisect_visit___ 1; ())
  | Not_found ->
      (___bisect_visit___ 2;
       (let ___bisect_result___ = print_endline "bar" in
        ___bisect_visit___ 0; ___bisect_result___))
let f () =
  ___bisect_visit___ 7;
  (try
     let ___bisect_result___ = print_endline "foo" in
     ___bisect_visit___ 6; ___bisect_result___
   with | Exit -> (___bisect_visit___ 4; ())
   | Not_found -> (___bisect_visit___ 5; print_endline "bar"))
let () =
  try ()
  with
  | Exit|Not_found as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | Exit -> (___bisect_visit___ 9; ())
          | Not_found -> (___bisect_visit___ 10; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       (let ___bisect_result___ = print_endline "bar" in
        ___bisect_visit___ 8; ___bisect_result___))
