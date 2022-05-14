open! Base
open Ppxlib
open Ast_builder.Default

type t =
  { loc : location
  ; unique_name : string
  }

let create string ~loc = { loc; unique_name = gen_symbol ~prefix:string () }
let of_string_loc { loc; txt } = create txt ~loc
let to_string_loc { loc; unique_name } = { loc; txt = unique_name }
let expression { loc; unique_name } = evar unique_name ~loc
let pattern { loc; unique_name } = pvar unique_name ~loc
