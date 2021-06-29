open! Core_kernel
open! Import

module Time_ns = struct
  include Time_ns

  external format : float -> string -> string = "core_kernel_time_ns_format"

  (* We use a more pleasant format than [Core_kernel.Time_ns.sexp_of_t],
     which has to be messier for round trippability. *)
  let sexp_of_t t =
    [%sexp
      (format (t |> to_span_since_epoch |> Span.to_sec) "%Y-%m-%dT%H:%M:%S%z" : string)]
  ;;
end

module Alarm = struct
  include Timing_wheel.Alarm

  let is_null t = phys_equal t (null ())
end

module Alarm_precision = Timing_wheel.Alarm_precision

let default_timing_wheel_config =
  (* 1/8th of a millisecond alarm_precision seems sufficient to avoid having many alarms
     in the same interval, which avoids quadratic insertion sort when firing alarms.  And
     the level bits give us levels of >1s, >1m, >1h, >1d.  See test in
     [../test/test_synchronous_time_source.ml]. *)
  Timing_wheel.Config.create
    ~alarm_precision:Alarm_precision.(div about_one_millisecond ~pow2:3)
    ~level_bits:(Timing_wheel.Level_bits.create_exn [ 13; 6; 6; 5 ])
    ()
;;

type callback = unit -> unit

module Id = Types.Time_source_id

module T1 = struct
  module Event = struct
    module Status = struct
      type t = Types.Event.Status.t =
        | Aborted
        (* in [fired_events], must not run *)
        | Fired
        (* in [fired_events], ready to run *)
        | Happening
        (* currently running the callback *)
        | Scheduled
        (* in the timing wheel *)
        | Unscheduled (* not in timing wheel or [fired_events] *)
      [@@deriving compare, sexp_of]

      let transition_is_allowed ~from ~to_ =
        match from, to_ with
        | Aborted, Unscheduled (* skipped running callback *)
        | Fired, Happening (* started running callback *)
        | Fired, Aborted (* aborted *)
        | Happening, Scheduled (* for repeating events *)
        | Happening, Unscheduled (* event callback finished *)
        | Scheduled, Fired (* moved from timing wheel to [fired_events] *)
        | Scheduled, Unscheduled (* aborted *)
        | Unscheduled, Fired (* event scheduled in the past *)
        | Unscheduled, Scheduled (* event scheduled in the future *) -> true
        | (Aborted | Fired | Happening | Scheduled | Unscheduled), _ -> false
      ;;
    end

    type t = Types.Event.t =
      { (* [alarm] is non-null iff the event is in the timing wheel. *)
        mutable alarm : Job_or_event.t Alarm.t
      ; mutable at : Time_ns.t
      ; callback : unit -> unit
      ; execution_context : Execution_context.t
      ; (* [interval] is the period for the periodic events *)
        mutable interval : Time_ns.Span.t option
      ; (* [next_fired] is a singly-linked list of fired events, linked via [next_fired].
           An event is added to the list when it fires, either because it is added with a
           time in the past, or because time advances.  [advance_by_alarms] iterates over
           the events in [next_fired] and runs them, emptying the list. *)
        mutable next_fired : t
      ; mutable status : Status.t
      }
    [@@deriving fields]

    (* [none] is used to indicate the end of the singly-linked list of fired events. *)
    let rec none =
      { alarm = Alarm.null ()
      ; at = Time_ns.min_value_for_1us_rounding
      ; callback = (fun () -> assert false)
      ; execution_context = Execution_context.main
      ; interval = None
      ; next_fired = none
      ; status = Unscheduled
      }
    ;;

    let is_none t = phys_equal t none
    let is_some t = not (is_none t)

    let sexp_of_t
          ({ alarm = _
           ; at
           ; callback = _
           ; execution_context = _
           ; interval
           ; next_fired = _
           ; status
           } as t)
      =
      if is_none t
      then [%sexp "none"]
      else
        [%message
          "" (status : Status.t) (at : Time_ns.t) (interval : Time_ns.Span.t option)]
    ;;

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~alarm:
            (check (fun alarm ->
               [%test_result: bool]
                 (Alarm.is_null alarm)
                 ~expect:
                   (match t.status with
                    | Aborted | Fired | Happening | Unscheduled -> true
                    | Scheduled -> false)))
          ~at:ignore
          ~callback:ignore
          ~execution_context:ignore
          ~interval:ignore
          ~next_fired:
            (check (fun next_fired ->
               if is_some next_fired
               then (
                 match t.status with
                 | Aborted | Fired -> ()
                 | Happening | Scheduled | Unscheduled -> assert false)))
          ~status:ignore)
    ;;

    let compare_at t1 t2 = Time_ns.compare t1.at t2.at

    let set_status t to_ =
      let from = t.status in
      if not (Status.transition_is_allowed ~from ~to_)
      then
        raise_s
          [%message
            [%here]
              "bug -- set_status transition not allowed"
              (from : Status.t)
              (to_ : Status.t)
              ~event:(t : t)];
      t.status <- to_
    ;;
  end

  module Job_or_event = struct
    include Job_or_event

    let sexp_of_t t =
      let open Job_or_event.Match in
      let (K k) = kind t in
      match k, project k t with
      | Event, event -> [%sexp (event : Event.t)]
      | Job, _ ->
        (* We don't display the [Job.t]s in [events] because those are
           pool pointers, which are uninformative. *)
        [%message "<Job.t>"]
    ;;
  end

  type -'rw t = 'rw Types.Time_source.t1 =
    { id : Id.t
    ; (* [advance_errors] accumulates errors raised by alarms run by
         [advance_by_alarms]. *)
      mutable advance_errors : Error.t list
    ; (* [am_advancing] is true only during [advance_by_alarms], and is used to cause
         callbacks to raise if they call [advance_by_alarms]. *)
      mutable am_advancing : bool
    ; events : Job_or_event.t Timing_wheel.t
    ; (* [fired_events] is the front of the singly linked list of fired events, which is
         stored in increasing order of [Event.at]. *)
      mutable fired_events : Event.t
    ; (* [most_recently_fired] is the event that was most recently inserted into
         [fired_events].  It is used as an optimization to allow insertion of subsequent
         events to start later in the list rather than at the beginning.  It specifically
         avoids quadratic behavior when inserting multiple events that have exactly the
         same time -- the time source fires such events in the order they were added, and
         we want them to be in that same order in [fired_events]. *)
      mutable most_recently_fired : Event.t
    ; (* We store [handle_fired] in [t] to avoid allocating it every time we call
         [advance_clock]. *)
      handle_fired : Job_or_event.t Alarm.t -> unit
    ; is_wall_clock : bool
    ; scheduler : Scheduler0.t
    }
  [@@deriving fields]

  (* We don't include the [id] in the sexp because the user (rightly) can't control it, so
     it's hard to make it deterministic in tests. *)
  let sexp_of_t
        _
        { id = _
        ; advance_errors = _
        ; am_advancing = _
        ; events
        ; fired_events = _
        ; handle_fired = _
        ; is_wall_clock
        ; most_recently_fired = _
        ; scheduler = _
        }
    =
    let now = Timing_wheel.now events in
    if is_wall_clock
    then [%message "wall_clock" (now : Time_ns.t)]
    else (
      let all_events = ref [] in
      Timing_wheel.iter events ~f:(fun alarm ->
        all_events := (Alarm.at events alarm, Alarm.value events alarm) :: !all_events);
      let events =
        List.sort !all_events ~compare:(fun (at1, _) (at2, _) -> Time_ns.compare at1 at2)
        |> List.map ~f:snd
      in
      [%message "" (now : Time_ns.t) (events : Job_or_event.t list)])
  ;;

  let timing_wheel_now t = Timing_wheel.now t.events

  let is_in_fired_events t event =
    with_return (fun r ->
      let current = ref t.fired_events in
      while Event.is_some !current do
        if phys_equal !current event then r.return true;
        current := !current.next_fired
      done;
      false)
  ;;

  let invariant_with_jobs (type rw) ~job:(job_invariant : Job.t -> unit) (t : rw t) =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~id:ignore
        ~advance_errors:ignore
        ~am_advancing:ignore
        ~events:
          (check (fun events ->
             Timing_wheel.invariant ignore events;
             Timing_wheel.iter events ~f:(fun alarm ->
               let job_or_event = Alarm.value events alarm in
               let open Job_or_event.Match in
               let (K k) = kind job_or_event in
               match k, project k job_or_event with
               | Job, job -> job_invariant job
               | Event, event ->
                 assert (phys_equal alarm event.alarm);
                 [%test_result: Time_ns.t] event.at ~expect:(Alarm.at events alarm);
                 [%test_result: Event.Status.t] event.status ~expect:Scheduled)))
        ~fired_events:
          (check (fun (fired_events : Event.t) ->
             let current = ref fired_events in
             while Event.is_some !current do
               assert (Time_ns.( <= ) !current.at (timing_wheel_now t));
               let next = !current.next_fired in
               if Event.is_some next then assert (Time_ns.( <= ) !current.at next.at);
               current := next
             done))
        ~handle_fired:ignore
        ~is_wall_clock:ignore
        ~most_recently_fired:
          (check (fun most_recently_fired ->
             if Event.is_some t.most_recently_fired
             then assert (is_in_fired_events t most_recently_fired)))
        ~scheduler:ignore)
  ;;

  let invariant t = invariant_with_jobs ~job:(fun _ -> ()) t
end

open T1

type t = read T1.t [@@deriving sexp_of]

let invariant = invariant
let invariant_with_jobs = invariant_with_jobs

module Read_write = struct
  type t = read_write T1.t [@@deriving sexp_of]

  let invariant = invariant
  let invariant_with_jobs = invariant_with_jobs
end

let id t = t.id
let is_wall_clock t = t.is_wall_clock
let length t = Timing_wheel.length t.events
let max_allowed_alarm_time t = Timing_wheel.max_allowed_alarm_time t.events
let read_only (t : [> read ] T1.t) = (t :> t)

(* [fire t event] sets [event.status = Fired] and inserts [event] into [t.fired_events] in
   sorted time order. *)
let fire t (event : Event.t) =
  Event.set_status event Fired;
  event.alarm <- Alarm.null ();
  let prev = ref Event.none in
  let current = ref t.fired_events in
  (* If [event] belongs after [t.most_recently_fired], then we start the insertion there
     rather than at the front of [t.fired_events].  This works nicely if we're getting the
     alarms in non-decreasing time order, which is close to what [Timing_wheel]
     provides (although [Timing_wheel] doesn't guarantee time ordering for times in the
     same interval). *)
  if Event.is_some t.most_recently_fired
  && Time_ns.( >= ) event.at t.most_recently_fired.at
  then (
    prev := t.most_recently_fired;
    current := !prev.next_fired);
  (* We use [Time_ns.( <= )] rather than [<] so that [event] is added after other events
     at the same time.  Since [Timing_wheel] fires alarms in a bucket in the order in
     which they were added, using [<=] keeps events at the same time in the order in which
     they were added. *)
  while Event.is_some !current && Time_ns.( <= ) !current.at event.at do
    prev := !current;
    current := !current.next_fired
  done;
  event.next_fired <- !current;
  t.most_recently_fired <- event;
  if Event.is_none !prev then t.fired_events <- event else !prev.next_fired <- event
;;

let alarm_precision t = Timing_wheel.alarm_precision t.events
let next_alarm_fires_at t = Timing_wheel.next_alarm_fires_at t.events
let now t = if t.is_wall_clock then Time_ns.now () else timing_wheel_now t
let timing_wheel_now = timing_wheel_now

let schedule t (event : Event.t) =
  Event.set_status event Scheduled;
  event.alarm <- Timing_wheel.add t.events ~at:event.at (event |> Job_or_event.of_event)
;;

module Event = struct
  include Event

  let create_internal t ~at ~interval ~callback =
    { alarm = Alarm.null ()
    ; at
    ; callback
    ; execution_context = t.scheduler.current_execution_context
    ; interval
    ; next_fired = none
    ; status = Unscheduled
    }
  ;;

  let add t event =
    if Time_ns.( <= ) event.at (timing_wheel_now t)
    then fire t event
    else schedule t event
  ;;

  let create_and_add t ~at ~interval ~callback =
    let event = create_internal t ~at ~interval ~callback in
    add t event;
    event
  ;;

  let at t at callback = create_and_add t ~at ~interval:None ~callback

  let after t span callback =
    create_and_add t ~at:(Time_ns.after (now t) span) ~interval:None ~callback
  ;;

  let require_span_at_least_alarm_precision t span =
    let alarm_precision = alarm_precision t in
    if Time_ns.Span.( < ) span alarm_precision
    then
      raise_s
        [%message
          "interval span smaller than alarm precision"
            (span : Time_ns.Span.t)
            (alarm_precision : Time_ns.Span.t)]
  ;;

  let at_intervals t span callback =
    require_span_at_least_alarm_precision t span;
    create_and_add t ~at:(now t) ~interval:(Some span) ~callback
  ;;

  module Abort_result = struct
    type t =
      | Ok
      | Currently_happening
      | Previously_unscheduled
    [@@deriving sexp_of]
  end

  let abort t (event : t) : Abort_result.t =
    match event.status with
    | Aborted -> Previously_unscheduled
    | Happening ->
      if Option.is_none event.interval
      then Currently_happening
      else (
        event.interval <- None;
        Ok)
    | Fired ->
      Event.set_status event Aborted;
      Ok
    | Scheduled ->
      Event.set_status event Unscheduled;
      Timing_wheel.remove t.events event.alarm;
      event.alarm <- Alarm.null ();
      Ok
    | Unscheduled -> Previously_unscheduled
  ;;

  let abort_if_possible t event = ignore (abort t event : Abort_result.t)

  let abort_exn t event =
    match abort t event with
    | Ok -> ()
    | reason ->
      raise_s
        [%message
          "[Synchronous_time_source.abort_exn] cannot abort event"
            (reason : Abort_result.t)]
  ;;

  let create t callback = create_internal t ~at:Time_ns.epoch ~interval:None ~callback

  let schedule_at_internal t (event : t) at ~interval =
    (* [Fired] is disallowed to prevent the user from entering into an infinite loop.  The
       user could specify [at] in the past which would constantly add [callback] to the
       back of [t.next_fired] if this function is called from [callback]. *)
    match event.status with
    | (Aborted | Happening | Scheduled | Fired) as status ->
      Or_error.error_s
        [%message "cannot schedule an event with status" ~_:(status : Event.Status.t)]
    | Unscheduled ->
      event.at <- at;
      event.interval <- interval;
      add t event;
      Ok ()
  ;;

  let schedule_at t event at = schedule_at_internal t event at ~interval:None
  let schedule_after t event span = schedule_at t event (Time_ns.after (now t) span)

  let schedule_at_intervals t event span =
    require_span_at_least_alarm_precision t span;
    schedule_at_internal t event (now t) ~interval:(Some span)
  ;;

  module Reschedule_result = struct
    type t =
      | Ok
      | Currently_happening
      | Recently_aborted
      | Recently_fired
    [@@deriving sexp_of]
  end

  let reschedule_at t event at : Reschedule_result.t =
    match event.status with
    | Aborted -> Recently_aborted
    | Fired -> Recently_fired
    | Happening -> Currently_happening
    | Scheduled ->
      event.at <- at;
      if Time_ns.( > ) at (timing_wheel_now t)
      then Timing_wheel.reschedule t.events event.alarm ~at
      else (
        Timing_wheel.remove t.events event.alarm;
        fire t event);
      Ok
    | Unscheduled ->
      event.at <- at;
      event.interval <- None;
      add t event;
      Ok
  ;;

  let reschedule_after t event span = reschedule_at t event (Time_ns.after (now t) span)

  module Option = struct
    type value = t
    type nonrec t = t

    let is_none = is_none
    let is_some = is_some

    let some value =
      (* This assert shouldn't fail because [t] is a [value] and so should never
         be [none]. *)
      assert (is_some value);
      value
    ;;

    (* It should be impossible for [some_is_representable] to return [false]
       because its input is a [value], but since it's only loosely enforced we
       handle the general case. *)
    let some_is_representable value =
      assert (is_some value);
      true
    ;;

    let none = none
    let unchecked_value = Fn.id
    let value t ~default = if is_none t then default else unchecked_value t

    let value_exn t =
      if is_none t
      then raise_s [%message "[Synchronous_time_source.Event.Option.value_exn None]"];
      t
    ;;

    let to_option t = if is_none t then None else Some t

    let of_option = function
      | None -> none
      | Some t -> some t
    ;;

    let sexp_of_t t = to_option t |> [%sexp_of: t option]

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = Fn.id
      end
    end
  end

end

let run_after t span callback = ignore (Event.after t span callback : Event.t)
let run_at t at callback = ignore (Event.at t at callback : Event.t)

let run_at_intervals t span callback =
  ignore (Event.at_intervals t span callback : Event.t)
;;

type send_exn = Monitor0.t -> ?backtrace:[ `Get | `This of Backtrace.t ] -> exn -> unit

let run_fired_events t ~(send_exn : send_exn option) =
  let current_execution_context = t.scheduler.current_execution_context in
  while Event.is_some t.fired_events do
    let event = t.fired_events in
    if phys_equal event t.most_recently_fired then t.most_recently_fired <- Event.none;
    t.fired_events <- event.next_fired;
    event.next_fired <- Event.none;
    match event.status with
    | Aborted -> Event.set_status event Unscheduled
    | Happening | Scheduled | Unscheduled -> assert false
    | Fired ->
      Event.set_status event Happening;
      (* We set the execution context so that [event.callback] runs in the same context
         that was in place when [event] was created. *)
      Scheduler0.set_execution_context t.scheduler event.execution_context;
      (match event.callback () with
       | exception exn ->
         (match send_exn with
          | None -> t.advance_errors <- Error.of_exn exn :: t.advance_errors
          | Some send_exn ->
            let backtrace = Backtrace.get () in
            send_exn event.execution_context.monitor exn ~backtrace:(`This backtrace));
         Event.set_status event Unscheduled
       | () ->
         (match event.interval with
          | None -> Event.set_status event Unscheduled
          | Some interval ->
            event.at
            <- Time_ns.next_multiple
                 ()
                 ~base:event.at
                 ~after:(timing_wheel_now t)
                 ~interval;
            schedule t event))
  done;
  Scheduler0.set_execution_context t.scheduler current_execution_context
;;

let advance_clock t ~to_ ~send_exn =
  Timing_wheel.advance_clock t.events ~to_ ~handle_fired:t.handle_fired;
  run_fired_events t ~send_exn
;;

let fire_past_alarms t ~send_exn =
  Timing_wheel.fire_past_alarms t.events ~handle_fired:t.handle_fired;
  run_fired_events t ~send_exn
;;

let advance_internal t ~to_ ~send_exn =
  advance_clock t ~to_ ~send_exn;
  fire_past_alarms t ~send_exn
;;

let prepare_to_advance t ~send_exn =
  if t.am_advancing
  then raise_s [%message "cannot call [advance_by_alarms] from callback"];
  t.am_advancing <- true;
  (match t.advance_errors with
   | [] -> ()
   | _ -> t.advance_errors <- []);
  run_fired_events t ~send_exn
;;

let finish_advancing t =
  t.am_advancing <- false;
  match t.advance_errors with
  | [] -> Ok ()
  | errors ->
    t.advance_errors <- [];
    Error (Error.of_list errors)
;;

let advance_by_alarms t ~to_ =
  let send_exn = None in
  prepare_to_advance t ~send_exn;
  let continue = ref true in
  while !continue do
    if Timing_wheel.is_empty t.events
    then continue := false
    else (
      let next_alarm_fires_at = Timing_wheel.next_alarm_fires_at_exn t.events in
      if Time_ns.( >= ) next_alarm_fires_at to_
      then continue := false
      else
        (* We use the actual alarm time, rather than [next_alarm_fires_at], so as not to
           expose (or accumulate errors associated with) the precision of
           [Timing_wheel]. *)
        advance_internal
          t
          ~to_:(Timing_wheel.max_alarm_time_in_min_interval_exn t.events)
          ~send_exn)
  done;
  advance_internal t ~to_ ~send_exn;
  finish_advancing t
;;

let advance_directly t ~to_ =
  let send_exn = None in
  prepare_to_advance t ~send_exn;
  advance_internal t ~to_ ~send_exn;
  finish_advancing t
;;

module Expert = struct
  let max_alarm_time_in_min_timing_wheel_interval t =
    Timing_wheel.max_alarm_time_in_min_interval t.events
  ;;

  let has_events_to_run t = Event.is_some t.fired_events
end
