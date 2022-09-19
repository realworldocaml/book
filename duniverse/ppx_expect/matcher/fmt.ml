open! Base
open Import
open Ppx_compare_lib.Builtin
open Sexplib0.Sexp_conv

type t =
  | Regexp of string
  | Glob of string
  | Literal of string
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : t) -> ()

let sexp_of_t =
  (function
    | Regexp arg0__001_ ->
      let res0__002_ = sexp_of_string arg0__001_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Regexp"; res0__002_ ]
    | Glob arg0__003_ ->
      let res0__004_ = sexp_of_string arg0__003_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Glob"; res0__004_ ]
    | Literal arg0__005_ ->
      let res0__006_ = sexp_of_string arg0__005_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Literal"; res0__006_ ]
      : t -> Sexplib0.Sexp.t)
;;

let _ = sexp_of_t

let compare =
  (fun a__007_ b__008_ ->
     if Ppx_compare_lib.phys_equal a__007_ b__008_
     then 0
     else (
       match a__007_, b__008_ with
       | Regexp _a__009_, Regexp _b__010_ -> compare_string _a__009_ _b__010_
       | Regexp _, _ -> -1
       | _, Regexp _ -> 1
       | Glob _a__011_, Glob _b__012_ -> compare_string _a__011_ _b__012_
       | Glob _, _ -> -1
       | _, Glob _ -> 1
       | Literal _a__013_, Literal _b__014_ -> compare_string _a__013_ _b__014_)
       : t -> t -> int)
;;

let _ = compare

let equal =
  (fun a__015_ b__016_ ->
     if Ppx_compare_lib.phys_equal a__015_ b__016_
     then true
     else (
       match a__015_, b__016_ with
       | Regexp _a__017_, Regexp _b__018_ -> equal_string _a__017_ _b__018_
       | Regexp _, _ -> false
       | _, Regexp _ -> false
       | Glob _a__019_, Glob _b__020_ -> equal_string _a__019_ _b__020_
       | Glob _, _ -> false
       | _, Glob _ -> false
       | Literal _a__021_, Literal _b__022_ -> equal_string _a__021_ _b__022_)
       : t -> t -> bool)
;;

let _ = equal

[@@@end]
