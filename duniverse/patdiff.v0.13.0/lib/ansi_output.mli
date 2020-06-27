open! Core
open! Import

val apply_styles
  :  ?drop_leading_resets:bool
  -> Patdiff_format.Style.t list
  -> string
  -> string

include Output_intf.S
