open! Base
open Import
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type t =
  | Regexp of string
  | Glob of string
  | Literal of string
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : t) -> ()

let sexp_of_t =
  (function
    | Regexp v0 ->
      let v0 = sexp_of_string v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Regexp"; v0 ]
    | Glob v0 ->
      let v0 = sexp_of_string v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Glob"; v0 ]
    | Literal v0 ->
      let v0 = sexp_of_string v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Literal"; v0 ]
      : t -> Ppx_sexp_conv_lib.Sexp.t)
;;

let _ = sexp_of_t

let compare =
  (fun a__001_ b__002_ ->
     if Ppx_compare_lib.phys_equal a__001_ b__002_
     then 0
     else (
       match a__001_, b__002_ with
       | Regexp _a__003_, Regexp _b__004_ -> compare_string _a__003_ _b__004_
       | Regexp _, _ -> -1
       | _, Regexp _ -> 1
       | Glob _a__005_, Glob _b__006_ -> compare_string _a__005_ _b__006_
       | Glob _, _ -> -1
       | _, Glob _ -> 1
       | Literal _a__007_, Literal _b__008_ -> compare_string _a__007_ _b__008_)
       : t -> t -> int)
;;

let _ = compare

let equal =
  (fun a__009_ b__010_ ->
     if Ppx_compare_lib.phys_equal a__009_ b__010_
     then true
     else (
       match a__009_, b__010_ with
       | Regexp _a__011_, Regexp _b__012_ -> equal_string _a__011_ _b__012_
       | Regexp _, _ -> false
       | _, Regexp _ -> false
       | Glob _a__013_, Glob _b__014_ -> equal_string _a__013_ _b__014_
       | Glob _, _ -> false
       | _, Glob _ -> false
       | Literal _a__015_, Literal _b__016_ -> equal_string _a__015_ _b__016_)
       : t -> t -> bool)
;;

let _ = equal

[@@@end]
