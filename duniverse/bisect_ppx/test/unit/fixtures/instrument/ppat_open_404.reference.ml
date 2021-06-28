[@@@ocaml.text "/*"]
module Bisect_visit___ppat_open_404___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0005\000\000\000\012\000\000\000-\000\000\000-\b\000\000,\000\160p@\160uA\160\000kB\160\000qC\160\000vD\160\001\000\132E\160\001\000\194F\160\001\000\199G\160\001\000\214H\160\001\001(I\160\001\001-J" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "ppat_open_404.ml" ~point_count:11 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___ppat_open_404___ml
[@@@ocaml.text "/*"]
let () =
  match `A with
  | List.((`A|`B))  as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | List.(`A)  -> (___bisect_visit___ 0; ())
          | List.(`B)  -> (___bisect_visit___ 1; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match [`A] with
  | List.((`A|`B)::[])  as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | List.(`A::[])  ->
              (___bisect_visit___ 3; ___bisect_visit___ 2; ())
          | List.(`B::[])  ->
              (___bisect_visit___ 4; ___bisect_visit___ 2; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
  | _ -> (___bisect_visit___ 5; ())
let () =
  match [|`A|] with
  | List.[|(`A|`B)|]  as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | List.[|`A|]  -> (___bisect_visit___ 6; ())
          | List.[|`B|]  -> (___bisect_visit___ 7; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
  | _ -> (___bisect_visit___ 8; ())
let () =
  match { contents = `A } with
  | List.{ contents = (`A|`B) }  as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | List.{ contents = `A }  -> (___bisect_visit___ 9; ())
          | List.{ contents = `B }  -> (___bisect_visit___ 10; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
