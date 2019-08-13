(** [Backing_out_channel] generalizes [Out_channel] to a narrow interface that can be used
    to collect strings, etc. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

type output_chars = bigstring -> len:int -> unit

val of_out_channel : Out_channel.t -> t
val of_output_char : (char -> unit) -> t

val create
  :  output_char:(char -> unit)
  -> output_chars:output_chars
  -> flush:(unit -> unit)
  -> sexp:(unit -> Sexp.t)
  -> t

val output_char : t -> char -> unit

val output
  :  t
  -> blit_to_bigstring:('a, Bigstring.t) Blit.blit
  -> src:'a
  -> src_len:int
  -> src_pos:int
  -> unit

val flush : t -> unit
