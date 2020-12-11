open Base
open Ppxlib

val sexp_of_labelled_exprs
  :  omit_nil:bool
  -> loc:location
  -> (arg_label * expression) list
  -> expression

val expand : omit_nil:bool -> path:'a -> expression -> expression

val expand_opt
  :  omit_nil:bool
  -> loc:location
  -> path:'a
  -> expression option
  -> expression
