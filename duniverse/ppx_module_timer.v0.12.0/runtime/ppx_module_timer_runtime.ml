open! Base
module Gc = Caml.Gc

external __MODULE__ : string = "%loc_MODULE"

let am_recording_environment_variable = "PPX_MODULE_TIMER"

let get_am_recording_environment_variable () =
  (* avoid Caml.Sys.getenv_opt to preserve 4.04.x compatibility *)
  match Caml.Sys.getenv am_recording_environment_variable with
  | value -> Some value
  | exception _ -> None
;;

let am_recording = Option.is_some (get_am_recording_environment_variable ())

module Startup_time = struct
  module Gc_events = struct
    type t =
      { minor_collections : int
      ; major_collections : int
      ; compactions : int
      }
    [@@deriving sexp_of]
  end

  type t =
    { module_name : string
    ; startup_time_in_nanoseconds : Int63.t
    ; gc_events : Gc_events.t
    }
  [@@deriving sexp_of]
end

let startup_times_in_reverse_chronological_order = ref []
let currently_running_module_name = ref ""
let currently_running_module_start = ref Int63.zero
let currently_running_module_gc_stats = ref (Gc.quick_stat ())

let reset_currently_running_module () =
  currently_running_module_name := "";
  currently_running_module_start := Int63.zero
;;

let record_start module_name =
  if am_recording
  then (
    assert (String.is_empty !currently_running_module_name);
    currently_running_module_name := module_name;
    currently_running_module_gc_stats := Gc.quick_stat ();
    (* call [Time_now] as late as possible before running the module body *)
    currently_running_module_start := Time_now.nanoseconds_since_unix_epoch ())
;;

let record_until module_name =
  if am_recording
  then (
    (* compute [Time_now] as soon as possible after running the module body *)
    let until = Time_now.nanoseconds_since_unix_epoch () in
    let start = !currently_running_module_start in
    let gc_stats_after = Gc.quick_stat () in
    let gc_stats_before = !currently_running_module_gc_stats in
    let startup_time_in_nanoseconds = Int63.( - ) until start in
    assert (String.equal !currently_running_module_name module_name);
    let gc_events : Startup_time.Gc_events.t =
      { minor_collections =
          gc_stats_after.minor_collections - gc_stats_before.minor_collections
      ; major_collections =
          gc_stats_after.major_collections - gc_stats_before.major_collections
      ; compactions = gc_stats_after.compactions - gc_stats_before.compactions
      }
    in
    let startup_time : Startup_time.t =
      { module_name; startup_time_in_nanoseconds; gc_events }
    in
    startup_times_in_reverse_chronological_order :=
      startup_time :: !startup_times_in_reverse_chronological_order;
    reset_currently_running_module ())
;;

let string_of_span_in_ns nanos = Int63.to_string nanos ^ "ns"

let char_is_digit_or_underscore = function
  | '0' .. '9'
  | '_' -> true
  | _ -> false
;;

let span_in_ns_of_string string =
  match String.chop_suffix string ~suffix:"ns" with
  | Some prefix
    when String.for_all prefix ~f:char_is_digit_or_underscore ->
    Some (Int63.of_string prefix)
  | _ -> None
;;

let gc_events_suffix_string
      ({ minor_collections; major_collections; compactions } : Startup_time.Gc_events.t)
  =
  let to_list description count =
    if count = 0 then [] else [ Int.to_string count ^ " " ^ description ]
  in
  let strings =
    to_list "minor collections" minor_collections
    @ to_list "major collections" major_collections
    @ to_list "compactions" compactions
  in
  if List.is_empty strings then "" else "; GC: " ^ String.concat strings ~sep:", "
;;

let print_with_left_column_right_justified list =
  let left_column_width =
    List.fold list ~init:0 ~f:(fun width (left, _, _) ->
      Int.max width (String.length left))
  in
  List.iter list ~f:(fun (left, right, gc_events) ->
    Stdio.printf
      "%*s %s%s\n"
      left_column_width
      left
      right
      (gc_events_suffix_string gc_events))
;;

let default_print_recorded_startup_times startup_times =
  let startup_times =
    match
      get_am_recording_environment_variable () |> Option.bind ~f:span_in_ns_of_string
    with
    | None -> startup_times
    | Some override ->
      Stdio.print_endline "ppx_module_timer: overriding time measurements for testing";
      List.mapi startup_times ~f:(fun index (startup_time : Startup_time.t) ->
        let startup_time_in_nanoseconds =
          Int63.( * ) override (Int63.of_int (index + 1))
        in
        { startup_time with startup_time_in_nanoseconds })
  in
  List.map
    startup_times
    ~f:(fun ({ module_name; startup_time_in_nanoseconds; gc_events } : Startup_time.t) ->
      string_of_span_in_ns startup_time_in_nanoseconds, module_name, gc_events)
  |> print_with_left_column_right_justified
;;

let print_recorded_startup_times = ref default_print_recorded_startup_times

let () =
  Caml.at_exit (fun () ->
    !print_recorded_startup_times
      (List.rev !startup_times_in_reverse_chronological_order))
;;
