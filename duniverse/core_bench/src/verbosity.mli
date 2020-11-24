open! Core

type t =
  | Quiet
  | Low
  | High
[@@deriving sexp]

val set_verbosity : t -> unit

val print_high : ('a, Out_channel.t, unit) format -> 'a
val print_low  : ('a, Out_channel.t, unit) format -> 'a
