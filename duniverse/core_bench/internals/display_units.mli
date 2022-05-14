(** A module internal to [Core_bench]. Please look at {!Bench}.

    Abstracts the representation, choice and scaling of units for each variable. *)
open! Core

type t =
  | Words
  | Time
  | Gc
  | Cycles
  | Count
  | Percentage

module Magnitude : sig
  type display_units := t

  type t =
    | One
    | Kilo
    | Mega
    | Giga
    | Milli
    | Micro
    | Nano

  val magnitude : display_units -> float -> t
  val max : t
  val smaller : t -> t -> t
end

val is_displayed : show_all_values:bool -> t -> float -> bool
val is_displayed_opt : show_all_values:bool -> t -> float option -> bool

val to_ci_string
  :  show_all_values:bool
  -> t
  -> Magnitude.t
  -> float * float
  -> Ansi_kernel.Attr.t list * string

val to_string
  :  show_all_values:bool
  -> t
  -> Magnitude.t
  -> float
  -> Ansi_kernel.Attr.t list * string

val to_string_opt
  :  show_all_values:bool
  -> t
  -> Magnitude.t
  -> float option
  -> Ansi_kernel.Attr.t list * string
