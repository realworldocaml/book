(** A module internal to [Core_bench]. Please look at {!Bench}.

   Tabular display of [Analysis_result]s. *)

open! Core

val display
  :  ?libname:string
  -> display_config:Display_config.t
  -> Analysis_result.t list
  -> unit

