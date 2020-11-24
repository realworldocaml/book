[@@@ocaml.text "/*"]
module Bisect_visit___cases___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\001\017\000\000\0007\000\000\000\217\000\000\000\217\b\000\000\216\000\160\000iA\160\000{@\160\001\000\174D\160\001\000\203B\160\001\000\211C\160\001\000\218E\160\001\001XF\160\001\001]G\160\001\001\165H\160\001\001\170I\160\001\002\005J\160\001\002\nK\160\001\002\024L\160\001\002sM\160\001\002xN\160\001\002\199O\160\001\002\204P\160\001\003\017S\160\001\003#Q\160\001\003(R\160\001\003xT\160\001\003}W\160\001\003\131U\160\001\003\136V\160\001\003\239X\160\001\003\244[\160\001\004\000Y\160\001\004\005Z\160\001\004W\\\160\001\004\\_\160\001\004`]\160\001\004e^\160\001\004\172`\160\001\004\177a\160\001\004\183b\160\001\004\252c\160\001\005\003d\160\001\005De\160\001\005Pf\160\001\005\155h\160\001\005\160i\160\001\005\189g\160\001\005\197j\160\001\005\204k\160\001\006\011m\160\001\006\024n\160\001\0066l\160\001\006\128p\160\001\006\141r\160\001\006\182o\160\001\006\190q\160\001\006\251s\160\001\007\019t\160\001\007\026u" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "cases.ml" ~point_count:54 ~point_definitions in
      cb
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index; result
  end
open Bisect_visit___cases___ml
[@@@ocaml.text "/*"]
type ('a, 'b) record = {
  left: 'a ;
  right: 'b }
let () =
  match `A with
  | `A ->
      (___bisect_visit___ 1; ___bisect_post_visit___ 0 (print_endline "foo"))
let () =
  match `A with
  | `A when
      ___bisect_visit___ 4;
      ___bisect_post_visit___ 2 (print_endline "foo");
      true -> (___bisect_visit___ 3; ())
  | _ -> (___bisect_visit___ 5; ())
let () = match `A with | `A -> assert false
let () =
  match `A with
  | `A|`B as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A -> (___bisect_visit___ 6; ())
          | `B -> (___bisect_visit___ 7; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A with
  | `A|`B as x as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A as x -> (___bisect_visit___ 8; ())
          | `B as x -> (___bisect_visit___ 9; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match None with
  | Some (`A|`B) as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | Some `A -> (___bisect_visit___ 10; ())
          | Some `B -> (___bisect_visit___ 11; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
  | _ -> (___bisect_visit___ 12; ())
let () =
  match `A `B with
  | `A (`B|`C) as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A `B -> (___bisect_visit___ 13; ())
          | `A `C -> (___bisect_visit___ 14; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A with
  | ((`A|`B) : _) as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | (`A : _) -> (___bisect_visit___ 15; ())
          | (`B : _) -> (___bisect_visit___ 16; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match lazy (___bisect_visit___ 19; `A) with
  | (lazy (`A|`B)) as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | (lazy `A) -> (___bisect_visit___ 17; ())
          | (lazy `B) -> (___bisect_visit___ 18; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match (`A, `C) with
  | ((`A|`B), (`C|`D)) as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | (`A, `C) -> (___bisect_visit___ 21; ___bisect_visit___ 20; ())
          | (`A, `D) -> (___bisect_visit___ 22; ___bisect_visit___ 20; ())
          | (`B, `C) -> (___bisect_visit___ 21; ___bisect_visit___ 23; ())
          | (`B, `D) -> (___bisect_visit___ 22; ___bisect_visit___ 23; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match { left = `A; right = `C } with
  | { left = (`A|`B); right = (`C|`D) } as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | { left = `A; right = `C } ->
              (___bisect_visit___ 25; ___bisect_visit___ 24; ())
          | { left = `A; right = `D } ->
              (___bisect_visit___ 26; ___bisect_visit___ 24; ())
          | { left = `B; right = `C } ->
              (___bisect_visit___ 25; ___bisect_visit___ 27; ())
          | { left = `B; right = `D } ->
              (___bisect_visit___ 26; ___bisect_visit___ 27; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match [|`A;`C|] with
  | [|(`A|`B);(`C|`D)|] as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | [|`A;`C|] -> (___bisect_visit___ 29; ___bisect_visit___ 28; ())
          | [|`A;`D|] -> (___bisect_visit___ 30; ___bisect_visit___ 28; ())
          | [|`B;`C|] -> (___bisect_visit___ 29; ___bisect_visit___ 31; ())
          | [|`B;`D|] -> (___bisect_visit___ 30; ___bisect_visit___ 31; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A with
  | `A|`B|`C as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A -> (___bisect_visit___ 32; ())
          | `B -> (___bisect_visit___ 33; ())
          | `C -> (___bisect_visit___ 34; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A () with
  | `A x|`B x as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A x -> (___bisect_visit___ 35; ())
          | `B x -> (___bisect_visit___ 36; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       x)
let () =
  match `A with
  | `A as x|`B as x as ___bisect_matched_value___ ->
      ((((match ___bisect_matched_value___ with
          | `A as x -> (___bisect_visit___ 37; ())
          | `B as x -> (___bisect_visit___ 38; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
let () =
  match `A with
  | `A|`B as ___bisect_matched_value___ when
      (((match ___bisect_matched_value___ with
         | `A -> (___bisect_visit___ 40; ())
         | `B -> (___bisect_visit___ 41; ())
         | _ -> ()))
      [@ocaml.warning "-4-8-9-11-26-27-28"]);
      ___bisect_post_visit___ 39 (print_endline "foo");
      true -> (___bisect_visit___ 42; ())
  | _ -> (___bisect_visit___ 43; ())
let () =
  match `A with
  | `A -> (___bisect_visit___ 45; ())
  | exception Exit ->
      (___bisect_visit___ 46;
       ___bisect_post_visit___ 44 (print_endline "foo"))
let () =
  match `A with
  | `A -> (___bisect_visit___ 48; ())
  | exception Exit when
      ___bisect_visit___ 50;
      ___bisect_post_visit___ 47 (print_endline "foo");
      true -> (___bisect_visit___ 49; ())
let () =
  match `A with
  | `A -> (___bisect_visit___ 51; ())
  | exception (Exit|Not_found as ___bisect_matched_value___) ->
      ((((match ___bisect_matched_value___ with
          | Exit -> (___bisect_visit___ 52; ())
          | Not_found -> (___bisect_visit___ 53; ())
          | _ -> ()))
       [@ocaml.warning "-4-8-9-11-26-27-28"]);
       ())
