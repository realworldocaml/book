[@@@ocaml.text "/*"]
module Bisect_visit___match___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000c\000\000\000\021\000\000\000Q\000\000\000Q\b\000\000P\000\160jA\160wB\160\000I@\160\000wE\160\001\000\137C\160\001\000\150D\160\001\000\242F\160\001\001\001G\160\001\001=H\160\001\001BI\160\001\001OJ\160\001\001\135L\160\001\001\148M\160\001\001\178K\160\001\001\244N\160\001\002\012O\160\001\002\019P\160\001\002TS\160\001\002fQ\160\001\002sR" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "match.ml" ~point_count:20 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___match___ml
[@@@ocaml.text "/*"]
let () =
  match `A with
  | `A -> (___bisect_visit___ 1; ())
  | `B ->
      (___bisect_visit___ 2; ___bisect_post_visit___ 0 (print_endline "foo"))
let f () =
  ___bisect_visit___ 5;
  (match `A with
   | `A -> (___bisect_visit___ 3; ())
   | `B -> (___bisect_visit___ 4; print_endline "foo"))
let () =
  match not true with
  | true -> (___bisect_visit___ 6; ())
  | false -> (___bisect_visit___ 7; ())
let () =
  match `A with
  | `A|`B as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A -> (___bisect_visit___ 8; ())
          | `B -> (___bisect_visit___ 9; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
  | `C -> (___bisect_visit___ 10; ())
let () =
  match `A with
  | `A -> (___bisect_visit___ 12; ())
  | exception Exit ->
      (___bisect_visit___ 13;
       ___bisect_post_visit___ 11 (print_endline "foo"))
let () =
  match `A with
  | `A -> (___bisect_visit___ 14; ())
  | exception (Exit|Not_found as ___bisect_matched_value___) ->
      ((((match ___bisect_matched_value___ with
          | Exit -> (___bisect_visit___ 15; ())
          | Not_found -> (___bisect_visit___ 16; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let f () =
  ___bisect_visit___ 19;
  (match `A with
   | `A -> (___bisect_visit___ 17; ())
   | exception Exit -> (___bisect_visit___ 18; print_endline "foo"))
