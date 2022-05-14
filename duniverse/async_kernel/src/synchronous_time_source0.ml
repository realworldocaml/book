open! Core
open! Import

module Time_ns = struct
  include Time_ns

  external format : float -> string -> string = "core_time_ns_format"

  (* We use a more pleasant format than [Core.Time_ns.sexp_of_t],
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
        | Fired (* in [fired_events], ready to run *)
        | Happening (* currently running the callback *)
        | Scheduled (* in the timing wheel *)
        | Unscheduled (* not in timing wheel or [fired_events] *)
      [@@deriving compare, equal, sexp_of]

      let transition_is_allowed ~from ~to_ =
        match from, to_ with
        | Fired, Happening (* started running callback *)
        | Fired, Unscheduled (* aborted *)
        (* [reschedule_*] goes through an intermediate [Fired, Unscheduled] state,
           so we never transition from [Fired] directly to [Scheduled]. *)
        | Happening, Scheduled (* for repeating events *)
        | Happening, Unscheduled (* event callback finished *)
        | Scheduled, Fired (* moved from timing wheel to [fired_events] *)
        | Scheduled, Unscheduled (* aborted *)
        | Unscheduled, Fired (* event scheduled in the past *)
        | Unscheduled, Scheduled (* event scheduled in the future *) -> true
        | (Fired | Happening | Scheduled | Unscheduled), _ -> false
      ;;
    end

    type event = Types.Event.t

    let sexp_of_event
          ({ alarm = _
           ; at
           ; callback = _
           ; execution_context = _
           ; interval
           ; next_fired = _
           ; prev_fired = _
           ; status
           } :
             event)
      =
      [%sexp
        { status : Status.t
        ; at : Time_ns.t
        ; interval : (Time_ns.Span.t option[@sexp.option])
        }]
    ;;

    module Option = struct
      (* This redefinition of [Event] is here so the type checks are right next
         to [Obj.magic]s. *)
      module Event_is_block : sig end = struct
        open Types
        open Event

        type _t = t =
          { (* must never be immediate *)
            mutable alarm : Job_or_event.t Timing_wheel.Alarm.t
          ; mutable at : Time_ns.t
          ; callback : unit -> unit
          ; execution_context : Execution_context.t
          ; mutable interval : Time_ns.Span.t option
          ; mutable next_fired : Option.t
          ; mutable prev_fired : Option.t
          ; mutable status : Status.t
          }
      end

      type t = Types.Event.Option.t

      (* Using an immediate rather than a statically-allocated record here seems to
         improve performance noticeably ([../bench/bin/bench_time_source.exe] benchmark
         is faster by ~10ns per alarm), presumably because it avoids the expensive
         parts of caml_modify. *)
      let none = (Obj.magic None : t) (* an arbitrary immediate *)

      let some = (Obj.magic : Types.Event.t -> t)
      let is_none t = phys_equal t none
      let is_some t = not (is_none t)
      let first_some t1 t2 = if is_some t1 then t1 else t2

      module Optional_syntax = struct
        module Optional_syntax = struct
          let is_none = is_none
          let unsafe_value = (Obj.magic : t -> Types.Event.t)
        end
      end

      open Optional_syntax

      let sexp_of_t t =
        match%optional t with
        | None -> [%sexp ()]
        | Some event -> [%sexp (event : event)]
      ;;

      let value t ~default =
        match%optional t with
        | None -> default
        | Some event -> event
      ;;

      let value_exn t =
        match%optional t with
        | None -> raise_s [%sexp "[Synchronous_time_source.Event.Option.value_exn None]"]
        | Some event -> event
      ;;

      let to_option t =
        match%optional t with
        | None -> None
        | Some event -> Some event
      ;;

      let of_option = function
        | None -> none
        | Some event -> some event
      ;;
    end

    type t = Types.Event.t =
      { (* [alarm] is non-null iff the event is in the timing wheel. *)
        mutable alarm : Job_or_event.t Alarm.t
      ; mutable at : Time_ns.t
      ; callback : unit -> unit
      ; execution_context : Execution_context.t
      ; (* [interval] is the period for the periodic events. *)
        mutable interval : Time_ns.Span.t option
      ; (* [next_fired] and [prev_fired] create a doubly-linked (non-circular) list of
           fired events, linked via these fields. An event is added to the list when
           it fires, either because it is added with a time in the past, or
           because time advances. [advance_by_alarms] iterates over the events
           in [next_fired] and runs them, emptying the list. [none] is used to
           indicate the end of the linked list of fired events. *)
        mutable next_fired : Option.t
      ; mutable prev_fired : Option.t
      ; mutable status : Status.t
      }
    [@@deriving fields]

    let sexp_of_t = [%sexp_of: event]

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
                    | Fired | Happening | Unscheduled -> true
                    | Scheduled -> false)))
          ~at:ignore
          ~callback:ignore
          ~execution_context:ignore
          ~interval:ignore
          ~next_fired:
            (check (fun next_fired ->
               match%optional (next_fired : Option.t) with
               | None ->
                 (* [next_fired] can be [None] even if the event status is Fired, assuming
                    it's at the end of the fired events list *)
                 ()
               | Some next_fired ->
                 [%test_result: Status.t] t.status ~expect:Fired;
                 assert (phys_equal (Option.some t) next_fired.prev_fired)))
          ~prev_fired:
            (check (fun prev_fired ->
               match%optional (prev_fired : Option.t) with
               | None ->
                 (* [prev_fired] can be [None] even if the event status is Fired, assuming
                    it's at the beginning of the fired events list *)
                 ()
               | Some prev_fired ->
                 [%test_result: Status.t] t.status ~expect:Fired;
                 assert (phys_equal (Option.some t) prev_fired.next_fired)))
          ~status:ignore)
    ;;

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

    let set_status_if ~is t to_ = if Status.equal is t.status then set_status t to_
    let scheduled_at = at
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
    ; (* [fired_events] is the front of the doubly-linked list of fired events,
         which is stored in increasing order of [Event.at]. *)
      mutable fired_events : Event.Option.t
    ; (* [most_recently_fired] is the event that was most recently inserted into
         [fired_events]. It is used as an optimization to allow insertion of
         subsequent events to start later in the list rather than at the beginning.
         It specifically avoids quadratic behavior when inserting multiple events
         that have exactly the same time -- the time source fires such events in
         the order they were added, and we want them to be in that same order in
         [fired_events]. *)
      mutable most_recently_fired : Event.Option.t
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

  let is_in_fired_events =
    let rec search current ~target_event =
      match%optional (current : Event.Option.t) with
      | None -> false
      | Some current ->
        phys_equal current target_event || search current.next_fired ~target_event
    in
    fun t target_event -> search t.fired_events ~target_event
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
                 [%test_result: Event.Status.t] event.status ~expect:Scheduled;
                 Event.invariant event)))
        ~fired_events:
          (check (fun (fired_events : Event.Option.t) ->
             let rec check_event (current : Event.t) =
               assert (Time_ns.( <= ) current.at (timing_wheel_now t));
               match%optional.Event.Option current.next_fired with
               | None -> ()
               | Some next ->
                 assert (Time_ns.( <= ) current.at next.at);
                 check_event next
             in
             match%optional.Event.Option fired_events with
             | None -> ()
             | Some event -> check_event event))
        ~handle_fired:ignore
        ~is_wall_clock:ignore
        ~most_recently_fired:
          (check (fun most_recently_fired ->
             match%optional (most_recently_fired : Event.Option.t) with
             | None -> ()
             | Some event -> assert (is_in_fired_events t event)))
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

(* [fire t event] sets [event.status = Fired] and inserts [event] into
   [t.fired_events] in sorted time order. *)
let fire t (event : Event.t) =
  Event.set_status event Fired;
  event.alarm <- Alarm.null ();
  let () =
    (* If [event] belongs after [t.most_recently_fired], then we start the
       insertion there rather than at the front of [t.fired_events]. This works
       nicely if we're getting the alarms in non-decreasing time order, which is
       close to what [Timing_wheel] provides (although [Timing_wheel] doesn't
       guarantee time ordering for times in the same interval). *)
    match%optional (t.most_recently_fired : Event.Option.t) with
    | Some most_recently_fired when Time_ns.( <= ) most_recently_fired.at event.at ->
      event.prev_fired <- Event.Option.some most_recently_fired;
      event.next_fired <- most_recently_fired.next_fired
    | _ ->
      event.prev_fired <- Event.Option.none;
      event.next_fired <- t.fired_events
  in
  t.most_recently_fired <- Event.Option.some event;
  (* We use [Time_ns.( <= )] rather than [<] so that [event] is added after other
     events at the same time. Since [Timing_wheel] fires alarms in a bucket in
     the order in which they were added, using [<=] keeps events at the same
     time in the order in which they were added. *)
  while
    match%optional (event.next_fired : Event.Option.t) with
    | None -> false
    | Some next ->
      let continue = Time_ns.( <= ) next.at event.at in
      if continue
      then (
        event.prev_fired <- event.next_fired;
        event.next_fired <- next.next_fired);
      continue
  do
    ()
  done;
  let () =
    match%optional (event.next_fired : Event.Option.t) with
    | None -> ()
    | Some next -> next.prev_fired <- Event.Option.some event
  in
  match%optional (event.prev_fired : Event.Option.t) with
  | None -> t.fired_events <- Event.Option.some event
  | Some prev -> prev.next_fired <- Event.Option.some event
;;

let alarm_precision t = Timing_wheel.alarm_precision t.events
let next_alarm_fires_at t = Timing_wheel.next_alarm_fires_at t.events

let next_alarm_runs_at t =
  if Event.Option.is_some t.fired_events
  then Some (timing_wheel_now t)
  else Timing_wheel.next_alarm_fires_at t.events
;;

let now t = if t.is_wall_clock then Time_ns.now () else timing_wheel_now t
let timing_wheel_now = timing_wheel_now

let schedule t (event : Event.t) =
  Event.set_status event Scheduled;
  event.alarm <- Timing_wheel.add t.events ~at:event.at (event |> Job_or_event.of_event)
;;

let remove_from_fired t (event : Event.t) ~new_status =
  let () =
    match%optional (t.most_recently_fired : Event.Option.t) with
    | None -> ()
    | Some most_recently_fired ->
      if phys_equal event most_recently_fired
      then
        t.most_recently_fired <- Event.Option.first_some event.next_fired event.prev_fired
  in
  let () =
    match%optional (event.prev_fired : Event.Option.t) with
    | None -> t.fired_events <- event.next_fired
    | Some prev -> prev.next_fired <- event.next_fired
  in
  let () =
    match%optional (event.next_fired : Event.Option.t) with
    | None -> ()
    | Some next -> next.prev_fired <- event.prev_fired
  in
  event.next_fired <- Event.Option.none;
  event.prev_fired <- Event.Option.none;
  Event.set_status event new_status
;;

module Event = struct
  include Event

  let create_internal t ~at ~interval ~callback =
    { alarm = Alarm.null ()
    ; at
    ; callback
    ; execution_context = t.scheduler.current_execution_context
    ; interval
    ; next_fired = Event.Option.none
    ; prev_fired = Event.Option.none
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
    | Happening ->
      (match event.interval with
       | None -> Currently_happening
       | Some (_ : Time_ns.Span.t) ->
         event.interval <- None;
         Ok)
    | Fired ->
      remove_from_fired t event ~new_status:Unscheduled;
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
    | (Happening | Scheduled | Fired) as status ->
      Or_error.error_s
        [%sexp "cannot schedule an event with status", (status : Event.Status.t)]
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

  let reschedule_at t event at : unit =
    match event.status with
    | Fired ->
      remove_from_fired t event ~new_status:Unscheduled;
      event.at <- at;
      add t event
    | Happening ->
      (* Happening events have already been removed from [fired]. *)
      event.at <- at;
      add t event
    | Scheduled ->
      event.at <- at;
      if Time_ns.( > ) at (timing_wheel_now t)
      then Timing_wheel.reschedule t.events event.alarm ~at
      else (
        Timing_wheel.remove t.events event.alarm;
        fire t event)
    | Unscheduled ->
      event.at <- at;
      event.interval <- None;
      add t event
  ;;

  let reschedule_after t event span = reschedule_at t event (Time_ns.after (now t) span)

end

let run_after t span callback = ignore (Event.after t span callback : Event.t)
let run_at t at callback = ignore (Event.at t at callback : Event.t)

let run_at_intervals t span callback =
  ignore (Event.at_intervals t span callback : Event.t)
;;

type send_exn = Monitor0.t -> ?backtrace:[ `Get | `This of Backtrace.t ] -> exn -> unit

let run_fired_events t ~(send_exn : send_exn option) =
  let current_execution_context = t.scheduler.current_execution_context in
  while
    match%optional (t.fired_events : Event.Option.t) with
    | None -> false
    | Some event ->
      (match event.status with
       | Happening | Scheduled | Unscheduled -> assert false
       | Fired ->
         remove_from_fired t event ~new_status:Happening;
         (* We set the execution context so that [event.callback] runs in the same context
            that was in place when [event] was created. *)
         Scheduler0.set_execution_context t.scheduler event.execution_context;
         (* Any modification of [status] below needs to first check that the event is
            still [Happening]. If the event status is not [Happening] then the event's
            callback must have rescheduled the event. In that case, do not set the status
            or attempt to reschedule a repeating event.

            This code could be much simpler if we immediately rescheduled the event before
            running the callback (no need for the Happening state then). One reason we
            don't do that is that we don't want to automatically reschedule a periodic
            event if its callback raises. *)
         (match event.callback () with
          | exception exn ->
            (match send_exn with
             | None -> t.advance_errors <- Error.of_exn exn :: t.advance_errors
             | Some send_exn ->
               let backtrace = Backtrace.Exn.most_recent () in
               send_exn event.execution_context.monitor exn ~backtrace:(`This backtrace));
            Event.set_status_if ~is:Happening event Unscheduled
          | () ->
            (match event.interval with
             | None -> Event.set_status_if ~is:Happening event Unscheduled
             | Some interval ->
               if Event.Status.equal Happening event.status
               then (
                 (* The event's callback did not reschedule the event. So reschedule the
                    repeating timer based on the last [at] time. *)
                 event.at
                 <- Time_ns.next_multiple
                      ()
                      ~base:event.at
                      ~after:(timing_wheel_now t)
                      ~interval;
                 schedule t event)));
         true)
  do
    ()
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
  then
    raise_s [%sexp "cannot call [advance_by_alarms] or [advance_directly] from callback"];
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
      let min_alarm_time = Timing_wheel.min_alarm_time_in_min_interval_exn t.events in
      if Time_ns.( >= ) min_alarm_time to_
      then continue := false
      else
        (* We use the actual alarm time, rather than [next_alarm_fires_at], so as not to
           expose (or accumulate errors associated with) the precision of
           [Timing_wheel]. *)
        advance_internal t ~to_:min_alarm_time ~send_exn)
  done;
  advance_internal t ~to_ ~send_exn;
  finish_advancing t
;;

let advance_by_max_alarms_in_each_timing_wheel_interval t ~to_ =
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

let duration_of t f =
  let start = now t in
  let result = f () in
  let duration = Time_ns.diff (now t) start in
  result, duration
;;

let max_alarm_time_in_min_timing_wheel_interval t =
  Timing_wheel.max_alarm_time_in_min_interval t.events
;;

let has_events_to_run t = Event.Option.is_some t.fired_events
