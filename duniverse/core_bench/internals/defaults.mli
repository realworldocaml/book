(** A module internal to [Core_bench]. Please look at {!Bench}.

    Many of the default settings are here. *)
open! Core

val columns_as_string : string list
val command_columns : Bench_command_column.t list
val display : Ascii_table_kernel.Display.t
val display_as_string : string
val fork_each_benchmark : bool
val geometric_scale : float
val limit_width_to : int
val no_compactions : bool
val quota : Quota.t
val stabilize_gc_between_runs : bool
val string_to_display : string -> Ascii_table_kernel.Display.t
