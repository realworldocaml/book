open Core

(* default columns for command *)
let columns_as_string = [ "time"; "alloc"; "percentage" ]
let command_columns = List.map ~f:Bench_command_column.of_string columns_as_string

(* how to measure *)
let geometric_scale = 1.01
let stabilize_gc_between_runs = false
let no_compactions = false

(* how long to measure *)
let quota = Quota.Span (Time.Span.of_int_sec 10)

(* width of the output table *)
let limit_width_to = 200

(* Fork each benchmark and run in separate process *)
let fork_each_benchmark = false

(* default display *)
let display_as_string = "short"

let string_to_display =
  let module Display = Ascii_table_kernel.Display in
  function
  | "short" -> Display.short_box
  | "tall" -> Display.tall_box
  | "line" -> Display.line
  | "blank" -> Display.blank
  | "column" -> Display.column_titles
  | s -> failwithf "Invalid display name: %s" s ()
;;

let display = string_to_display display_as_string
