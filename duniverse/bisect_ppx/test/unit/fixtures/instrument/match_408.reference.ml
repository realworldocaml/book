[@@@ocaml.text "/*"]
module Bisect_visit___match_408___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\147\000\000\000\030\000\000\000u\000\000\000u\b\000\000t\000\160\000K@\160\000XA\160\000iB\160\001\000\197C\160\001\000\210D\160\001\001?E\160\001\001NF\160\001\001_G\160\001\001\188H\160\001\001\201I\160\001\002*J\160\001\002=K\160\001\002NL\160\001\002\169M\160\001\002\174N\160\001\003\005Q\160\001\003\nR\160\001\003)P\160\001\0038O\160\001\003\128S\160\001\003\141T\160\001\003\146U\160\001\003\171V\160\001\003\255W\160\001\004\004X\160\001\004\029Y\160\001\004\"Z\160\001\004t[\160\001\004}\\" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "match_408.ml" ~point_count:29 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___match_408___ml
[@@@ocaml.text "/*"]
let () =
  match `A with
  | `A -> (___bisect_visit___ 0; ())
  | exception (Exit as ___bisect_matched_value___)
    |exception (Not_found as ___bisect_matched_value___) ->
      ((((match ___bisect_matched_value___ with
          | Exit -> (___bisect_visit___ 1; ())
          | Not_found -> (___bisect_visit___ 2; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match () with
  | () -> (___bisect_visit___ 3; ())
  | (exception Exit : unit) -> (___bisect_visit___ 4; ())
let () =
  match () with
  | () -> (___bisect_visit___ 5; ())
  | ((exception (Exit as ___bisect_matched_value___)
      |exception (Not_found as ___bisect_matched_value___)) : unit) ->
      ((((match ___bisect_matched_value___ with
          | Exit -> (___bisect_visit___ 6; ())
          | Not_found -> (___bisect_visit___ 7; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A with
  | `A -> (___bisect_visit___ 8; ())
  | List.(exception Exit)  -> (___bisect_visit___ 9; ())
let () =
  match `A with
  | `A -> (___bisect_visit___ 10; ())
  | List.((exception (Exit as ___bisect_matched_value___)
           |exception (Not_found as ___bisect_matched_value___))) 
      ->
      ((((match ___bisect_matched_value___ with
          | List.(Exit)  -> (___bisect_visit___ 11; ())
          | List.(Not_found)  -> (___bisect_visit___ 12; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  let ___bisect_case_0___ () = () in
  match `A with
  | `A -> (___bisect_visit___ 13; ___bisect_case_0___ ())
  | exception Exit -> (___bisect_visit___ 14; ___bisect_case_0___ ())
let () =
  let ___bisect_case_0___ () =
    if true
    then (___bisect_visit___ 16; ignore `B)
    else (___bisect_visit___ 15; ignore `C) in
  match `A with
  | `A -> (___bisect_visit___ 17; ___bisect_case_0___ ())
  | exception Exit -> (___bisect_visit___ 18; ___bisect_case_0___ ())
let () =
  let ___bisect_case_1___ () = () in
  match `A with
  | `A -> (___bisect_visit___ 19; ())
  | `B -> (___bisect_visit___ 20; ___bisect_case_1___ ())
  | exception Exit -> (___bisect_visit___ 21; ___bisect_case_1___ ())
  | exception Not_found -> (___bisect_visit___ 22; ())
let () =
  let ___bisect_case_1___ () = () in
  let ___bisect_case_0___ () = () in
  match `A with
  | `A -> (___bisect_visit___ 23; ___bisect_case_0___ ())
  | exception Exit -> (___bisect_visit___ 24; ___bisect_case_0___ ())
  | `B -> (___bisect_visit___ 25; ___bisect_case_1___ ())
  | exception Not_found -> (___bisect_visit___ 26; ___bisect_case_1___ ())
let () =
  let ___bisect_case_0___ y x () = () in
  match "foo" with
  | x as y -> (___bisect_visit___ 27; ___bisect_case_0___ y x ())
  | exception Invalid_argument (x as y) ->
      (___bisect_visit___ 28; ___bisect_case_0___ y x ())
