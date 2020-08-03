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

let am_recording_value = get_am_recording_environment_variable ()
let am_recording = Option.is_some am_recording_value

module Duration = struct
  type t = Int63.t

  let to_nanoseconds t = t
  let of_nanoseconds t = t

  module type Format = sig
    val of_string : string -> t
    val to_string_with_same_unit : t list -> string list
  end

  module Default_format = struct
    let of_string string = String.chop_suffix_exn string ~suffix:"ns" |> Int63.of_string
    let to_string nanos = Int63.to_string nanos ^ "ns"
    let to_string_with_same_unit list = List.map list ~f:to_string
  end

  let format = ref (module Default_format : Format)

  let of_string string =
    let (module Format) = !format in
    Format.of_string string
  ;;

  let to_string_with_same_unit string =
    let (module Format) = !format in
    Format.to_string_with_same_unit string
  ;;
end

module Gc_events = struct
  type t =
    { minor_collections : int
    ; major_collections : int
    ; compactions : int
    }
end

module Timing_event = struct
  type t =
    { description : string
    ; runtime : Duration.t
    ; gc_events : Gc_events.t
    ; nested_timing_events : t list
    }
end

module Timer = struct
  type t =
    { mutable currently_running_description : string
    ; mutable currently_running_start_time : Duration.t
    ; mutable currently_running_gc_stats : Gc.stat
    ; mutable nested_timer : t option
    ; mutable timing_events_in_reverse_chronological_order : Timing_event.t list
    }

  let create ?nested_timer () =
    { currently_running_description = ""
    ; currently_running_start_time = Int63.zero
    ; currently_running_gc_stats = Gc.quick_stat ()
    ; nested_timer
    ; timing_events_in_reverse_chronological_order = []
    }
  ;;

  let reset t =
    t.currently_running_description <- "";
    t.currently_running_start_time <- Int63.zero;
    match t.nested_timer with
    | None -> ()
    | Some nested -> nested.timing_events_in_reverse_chronological_order <- []
  ;;

  let record_start t description =
    if am_recording
    then (
      assert (String.is_empty t.currently_running_description);
      t.currently_running_description <- description;
      t.currently_running_gc_stats <- Gc.quick_stat ();
      (* call [Time_now] as late as possible before running the module body *)
      t.currently_running_start_time <- Time_now.nanoseconds_since_unix_epoch ())
  ;;

  let record_until t description =
    if am_recording
    then (
      (* compute [Time_now] as soon as possible after running the module body *)
      let until = Time_now.nanoseconds_since_unix_epoch () in
      let start = t.currently_running_start_time in
      let gc_stats_after = Gc.quick_stat () in
      let gc_stats_before = t.currently_running_gc_stats in
      let runtime = Int63.( - ) until start in
      assert (String.equal t.currently_running_description description);
      let gc_events : Gc_events.t =
        { minor_collections =
            gc_stats_after.minor_collections - gc_stats_before.minor_collections
        ; major_collections =
            gc_stats_after.major_collections - gc_stats_before.major_collections
        ; compactions = gc_stats_after.compactions - gc_stats_before.compactions
        }
      in
      let nested_timing_events =
        match t.nested_timer with
        | None -> []
        | Some nested -> List.rev nested.timing_events_in_reverse_chronological_order
      in
      let timing_event : Timing_event.t =
        { description; runtime; gc_events; nested_timing_events }
      in
      t.timing_events_in_reverse_chronological_order
      <- timing_event :: t.timing_events_in_reverse_chronological_order;
      reset t)
  ;;
end

let definition_timer = Timer.create ()
let module_timer = Timer.create ~nested_timer:definition_timer ()
let record_start module_name = Timer.record_start module_timer module_name
let record_until module_name = Timer.record_until module_timer module_name
let record_definition_start loc = Timer.record_start definition_timer loc
let record_definition_until loc = Timer.record_until definition_timer loc

let gc_events_suffix_string
      ({ minor_collections; major_collections; compactions } : Gc_events.t)
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

let with_left_column_right_justified list =
  let left_column_width =
    List.fold list ~init:0 ~f:(fun width (left, _) -> Int.max width (String.length left))
  in
  List.map list ~f:(fun (left, right) ->
    Printf.sprintf "%*s %s" left_column_width left right)
;;

let rec timing_events_to_strings list ~indent =
  let duration_strings =
    List.map list ~f:(fun (timing_event : Timing_event.t) -> timing_event.runtime)
    |> Duration.to_string_with_same_unit
  in
  let prefix = String.make indent ' ' in
  List.map2_exn
    duration_strings
    list
    ~f:(fun duration_string
         { runtime = _; description; gc_events; nested_timing_events }
         ->
           ( duration_string
           , description
             ^ gc_events_suffix_string gc_events
             ^ String.concat
                 (List.map
                    (timing_events_to_strings nested_timing_events ~indent:(indent + 4))
                    ~f:(fun line -> "\n" ^ line)) ))
  |> with_left_column_right_justified
  |> List.map ~f:(fun line -> prefix ^ line)
;;

let fake_timing_events =
  let gc_events i : Gc_events.t =
    { minor_collections = (if i % 2 = 1 then 1 else 0)
    ; major_collections = (if i % 4 = 3 then 1 else 0)
    ; compactions = (if i % 8 = 7 then 1 else 0)
    }
  in
  lazy
    (List.init 12 ~f:(fun i ->
       ({ description = Printf.sprintf "Fake__Dependency_%d" (i + 1)
        ; runtime = Int63.of_int (900 * (i + 1))
        ; gc_events = gc_events i
        ; nested_timing_events =
            (if (i + 1) % 4 = 0
             then
               List.init (i + 1) ~f:(fun j ->
                 ({ description = Printf.sprintf "Line %d" (j + 1)
                  ; runtime = Int63.of_int (900 * (j + 1))
                  ; gc_events = gc_events j
                  ; nested_timing_events = []
                  }
                  : Timing_event.t))
             else [])
        }
        : Timing_event.t)))
;;

let print_recorded_timing_events timing_events =
  let notify_of_overriding () =
    Stdio.print_endline "ppx_module_timer: overriding time measurements for testing"
  in
  let timing_events =
    match Option.value_exn am_recording_value with
    | "FAKE_MODULES" ->
      notify_of_overriding ();
      force fake_timing_events
    | string ->
      (match Duration.of_string string with
       | override ->
         notify_of_overriding ();
         List.mapi timing_events ~f:(fun index (timing_event : Timing_event.t) ->
           let runtime = Int63.( * ) override (Int63.of_int (index + 1)) in
           let nested_timing_events =
             List.mapi
               timing_event.nested_timing_events
               ~f:(fun index nested_timing_event ->
                 let runtime = Int63.( * ) override (Int63.of_int (index + 1)) in
                 { nested_timing_event with runtime })
           in
           { timing_event with runtime; nested_timing_events })
       | exception _ -> timing_events)
  in
  timing_events |> timing_events_to_strings ~indent:0 |> List.iter ~f:Stdio.print_endline
;;

let () =
  if am_recording
  then
    Caml.at_exit (fun () ->
      print_recorded_timing_events
        (List.rev module_timer.timing_events_in_reverse_chronological_order))
;;
