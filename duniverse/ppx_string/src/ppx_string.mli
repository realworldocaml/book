open! Base
open Ppxlib

val expand
  :  expr_loc:location
  -> string_loc:location
  -> string:label
  -> delimiter:label option
  -> expression
