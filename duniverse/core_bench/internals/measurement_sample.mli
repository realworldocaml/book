(** A module internal to [Core_bench]. Please look at {!Bench}.

    Contains the measurements of one run of a benchmark. *)
open! Core

type t =
  { mutable runs : int
  ; mutable cycles : Int63.t
  ; mutable nanos : Int63.t
  ; mutable compactions : int
  ; mutable minor_allocated : int
  ; mutable major_allocated : int
  ; mutable promoted : int
  ; mutable major_collections : int
  ; mutable minor_collections : int
  }
[@@deriving sexp, fields]

val accessor : Variable.t -> t -> float
val create : unit -> t
val field_names_to_string : unit -> string
val field_values_to_string : t -> string
val max : t array -> len:int -> field:(t -> int) -> int
val of_field_values_string : string -> t
val runs : t -> int
