(** A module internal to [Core_bench]. Please look at {!Bench}.

    Tabular display of [Analysis_result]s. *)

open! Core

module Warnings : sig
  val display : unit -> unit
end

val make_columns
  :  Display_config.t
  -> Analysis_result.t list
  -> Analysis_result.t Ascii_table_kernel.Column.t list
