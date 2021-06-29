open! Base
open! Ppxlib

val grammar_of_tds
  :  loc:Location.t
  -> path:string
  -> rec_flag * type_declaration list
  -> structure_item list

val sexp_grammar : loc:Location.t -> path:string -> core_type -> expression
