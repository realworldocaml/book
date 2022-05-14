(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

type t =
  { don't_display_table : bool
  ; limit_width_to : int
  ; display : Ascii_table_kernel.Display.t
  ; ascii_table : bool
  ; show_output_as_sexp : bool
  ; show_absolute_ci : bool
  ; show_percentage : bool
  ; show_speedup : bool
  ; show_samples : bool
  ; show_all_values : bool
  ; show_overheads : bool
  }
[@@deriving fields]

val create
  :  ?don't_display_table:bool
  -> ?limit_width_to:int
  -> ?display:Ascii_table_kernel.Display.t
  -> ?ascii_table:bool
  -> ?show_output_as_sexp:bool
  -> ?show_absolute_ci:bool
  -> ?show_percentage:bool
  -> ?show_speedup:bool
  -> ?show_samples:bool
  -> ?show_all_values:bool
  -> ?show_overheads:bool
  -> unit
  -> t
