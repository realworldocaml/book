open! Core
open! Import

type t = string Patience_diff.Hunk.t list

val iter'
  :  f_hunk_break:(string Patience_diff.Hunk.t -> unit)
  -> f_line:(string -> unit)
  -> t
  -> unit

val iter
  :  f_hunk_break:(int * int -> int * int -> unit)
  -> f_line:(string -> unit)
  -> t
  -> unit
