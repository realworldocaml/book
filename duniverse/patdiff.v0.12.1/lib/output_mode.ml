open! Core
open! Import

type t =
  | Ansi
  | Ascii
  | Html
[@@deriving sexp]

let implies_unrefined t =
  match t with
  | Ansi | Html -> false
  | Ascii -> true
;;
