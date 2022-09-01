open! Core
open! Import
include Cell_intf

type t = Attr.t list * Utf8_text.t list [@@deriving sexp_of]

let attr = fst
let to_tuple = Fn.id
let create attr str = attr, List.map (String.split_lines str) ~f:Utf8_text.of_string
let width (_, lines) = list_max ~f:Utf8_text.width lines

let wrap (_, lines) ~width ~prefer_split_on_spaces =
  List.bind lines ~f:(Utf8_text.chunks_of ~width ~prefer_split_on_spaces)
;;

let height (attrs, lines) ~display_empty_rows ~width ~prefer_split_on_spaces =
  let height = wrap (attrs, lines) ~width ~prefer_split_on_spaces |> List.length in
  if display_empty_rows then max height 1 else height
;;

let is_empty (_, lines) = List.for_all lines ~f:Utf8_text.is_empty
