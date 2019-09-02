open! Core

type t =
  | Span of Time.Span.t
  | Num_calls of int
[@@deriving sexp, bin_io]

include Stringable.S with type t := t

val max_count : t -> int

val scale_int : t -> int -> t

val fulfilled : t -> start:Time.t -> num_calls:int -> bool

val arg_type : t Command.Arg_type.t
