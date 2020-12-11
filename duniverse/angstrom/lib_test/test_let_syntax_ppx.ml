open Angstrom
open Let_syntax

let (_ : int t) =
  let%bind () = end_of_input in
  return 1

let (_ : int t) =
  let%map (_ : char) = any_char
  and (_ : string) = string "foo"
  in
  2

(* [mapn] support was introduced in ppx_let.v0.14.0, which CI does not reliably
  install. *)
(*
let (_ : int t) =
  let%mapn (_ : char) = any_char
  and (_ : string) = string "foo"
  in
  2
*)
