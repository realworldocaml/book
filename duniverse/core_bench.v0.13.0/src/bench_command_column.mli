(** A module internal to [Core_bench]. Please look at {!Bench}.

   Column specifications used by the command line interface. *)

open! Core

type t =
  | Analysis of Analysis_config.t list
  | Display_column of Display_column.t

val column_description_table : string
val of_string                : string -> t
val arg                      : t Command.Param.Arg_type.t


