(** A module internal to [Core_bench]. Please look at {!Bench}.

   Many of the default settings are here. *)
open Core


(* default columns for command *)
let columns_as_string = [
  "time";
  "alloc";
  "percentage";
]
let command_columns = List.map ~f:Bench_command_column.of_string columns_as_string

(* default columns *)
let columns = [ `Name ]

(* how to measure *)
let geometric_scale   = 1.01
let stabilize_gc_between_runs = false
let no_compactions = false

(* how long to measure *)
let quota = Quota.Span (Time.Span.of_int_sec 10)

(* saving generated data *)
let save_sample_data = false

(* width of the output table *)
let limit_width_to = 200

(* Fork each benchmark and run in separate process *)
let fork_each_benchmark = false

(* Bootstrapping iterations *)
let bootstrap_trials = 3000
let boostrap_reduced_trials = 300

(* Predictors to use for mv-regression *)
let predictors = [ `Runs ]
(* The default input-string is empty, since r is assumed and not exposed to user *)
let predictors_string = ""

(* default display *)
let display_as_string = "short"
let string_to_display =
  let module Display = Ascii_table.Display in
  function
  | "short"  -> Display.short_box
  | "tall"   -> Display.tall_box
  | "line"   -> Display.line
  | "blank"  -> Display.blank
  | "column" -> Display.column_titles
  | s -> failwithf "Invalid display name: %s" s ()

let display = string_to_display display_as_string
