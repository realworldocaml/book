open Core
open Import
open Require_explicit_time_source
module Core_unix = Core.Unix
module Unix = Unix_syscalls
module IOVec = Core.Unix.IOVec
module Id = Unique_id.Int63 ()

let io_stats = Io_stats.create ()
let debug = Debug.writer

module Time_ns_suppress_sexp_in_test = struct
  type t = Time_ns.t

  let sexp_of_t t = if am_running_inline_test then Sexp.List [] else Time_ns.sexp_of_t t
end

module Flush_result = struct
  type t =
    | Error
    | Consumer_left
    | Flushed of Time_ns_suppress_sexp_in_test.t
  [@@deriving sexp_of]
end

module Line_ending = struct
  type t =
    | Dos
    | Unix
  [@@deriving sexp_of]
end

module Check_buffer_age' = struct
  type 'a t =
    { writer : 'a
    ; maximum_age : Time_ns.Span.t
    ; mutable bytes_received_at_now_minus_maximum_age : Int63.t
    ; (* The 2 following queues hold the not-yet-written bytes received by the writer in
         the last [maximum_age] period of time, with the time they were received at.
         [Queue.length bytes_received_queue = Queue.length times_received_queue]. *)
      bytes_received_queue : Int63.t Queue.t
    ; times_received_queue : Time_ns.t Queue.t
    ; (* Number of bytes "seen" by the checker.  [t.writer.bytes_received - t.bytes_seen]
         represents the number of bytes received by the writer since the last time the
         checker ran. *)
      mutable bytes_seen : Int63.t
    ; (* The buffer-age check is responsible for filling in [too_old] if it detects an age
         violation. *)
      mutable too_old : unit Ivar.t
    ; (* The buffer-age checks are stored in one of these data structures per time source,
         and we keep a reference to our parent one in this [t] so we can easily remove
         ourselves from it when closing the writer. *)
      for_this_time_source : 'a per_time_source
    }

  and 'a per_time_source =
    { active_checks : ('a t[@sexp.opaque]) Bag.t
    ; closed : unit Ivar.t
    }
  [@@deriving fields, sexp_of]
end

module Open_flags = Unix.Open_flags

type open_flags = (Open_flags.t, exn) Result.t [@@deriving sexp_of]

module Backing_out_channel = Backing_out_channel

module Destroy_or_keep = struct
  type t =
    | Destroy
    | Keep
  [@@deriving sexp_of]
end

module Scheduled = struct
  type t = (Bigstring.t IOVec.t * Destroy_or_keep.t) Deque.t

  let length (t : t) = Deque.fold t ~init:0 ~f:(fun n (iovec, _) -> n + iovec.len)
end

module Stop_reason = struct
  type t =
    | Error
    | (* [Consumer_left] is only reported when [raise_when_consumer_leaves = false],
         otherwise an [Error] is reported. *)
      Consumer_left
  [@@deriving sexp_of]
end

type t =
  { id : Id.t
  ; mutable fd : Fd.t
  ; (* The writer uses a background job to flush data.  The job runs within
       [inner_monitor], which has a handler that wraps all errors to include [sexp_of_t
       t], and sends them to [monitor]. *)
    monitor : Monitor.t
  ; inner_monitor : Monitor.t
  ; mutable background_writer_state :
      [ `Running | `Not_running | `Stopped_permanently of Stop_reason.t ]
  ; background_writer_stopped : unit Ivar.t
  ; (* [syscall] determines the batching approach that the writer uses to batch data
       together and flush it using the underlying write syscall. *)
    syscall : [ `Per_cycle | `Periodic of Time.Span.t ]
  ; (* Counts since the writer was created. *)
    mutable bytes_received : Int63.t
  ; mutable bytes_written : Int63.t
  ; (* Bytes that we have received but not yet written are stored in two places:
       [scheduled] and [buf].  The bytes that we need to write are the concatenation of
       the sequence of iovecs in [scheduled] followed by the bytes in [buf] from
       [scheduled_back] to [back].  Note that iovecs in [scheduled] can point to regions
       in [buf], even the current [buf] in the writer. *)
    (* [scheduled] holds iovecs that we plan to write. *)
    scheduled : Scheduled.t
  ; (* [scheduled_bytes] is the sum of the lengths of the iovecs in[scheduled] *)
    mutable scheduled_bytes : int
  ; (* [buf] has three regions:
       [0, scheduled_back)             received and scheduled
       [scheduled_back, back)          received but not scheduled
       [back, Bigstring.length buf)    free space*)
    mutable buf : Bigstring.t
  ; mutable scheduled_back : int
  ; mutable back : int
  ; time_source : Time_source.t
  ; flushes : (Flush_result.t Ivar.t * Int63.t) Queue.t
  ; (* [closed_state] tracks the state of the writer as it is being closed.  Initially,
       [closed_state] is [`Open].  When [close] is called, [closed_state] transitions to
       [`Closed_and_flushing].  Once the writer is flushed and we're actually going to
       close [fd], it transitions to[`Closed].

       The distinction between [`Closed] and [`Closed_and_flushing] is necessary because
       we want to allow [write]s to happen while [`Closed_and_flushing], but not when
       [`Closed].  This is necessary to allow upstream producers to flush their data to
       the writer when it is closed. *)
    mutable close_state : [ `Open | `Closed_and_flushing | `Closed ]
  ; (* [close_finished] is filled when the close() system call on [fd] finishes. *)
    close_finished : unit Ivar.t
  ; (* [close_started] is filled when [close] is called. *)
    close_started : unit Ivar.t
  ; (* [producers_to_flush_at_close] holds all upstream producers feeding data to this
       writer, and thus should be flushed when we close this writer, before flushing
       the writer itself. *)
    producers_to_flush_at_close : (unit -> unit Deferred.t) Bag.t
  ; (* [flush_at_shutdown_elt] holds the element in [writers_to_flush_at_shutdown] for
       this writer.  Being in that bag is what causes this writer to be automatically
       closed when [shutdown] is called, and for shutdown to wait for the close to finish.
       [flush_at_shutdown_elt] is [Some] for the lifetime of the writer, until the
       close finishes, at which point it transitions to[None]. *)
    mutable flush_at_shutdown_elt : t Bag.Elt.t option
  ; mutable check_buffer_age : t Check_buffer_age'.t Bag.Elt.t option
  ; (* The "consumer" of a writer is whomever is reading the bytes that the writer
       is writing.  E.g. if the writer's file descriptor is a socket, then it is whomever
       is on the other side of the socket connection.  If the consumer leaves, Unix will
       indicate this by returning EPIPE or ECONNRESET to a write() syscall.  We keep
       track of this with the [consumer_left] ivar, which is exposed in writer.mli.
       We also allow the user to configure what action the writer takes when the
       consumer leaves.  By default, it raises, but that can be disabled. *)
    consumer_left : unit Ivar.t
  ; mutable raise_when_consumer_leaves : bool (* default is [true] *)
  ; (* [open_flags] is the open-file-descriptor bits of [fd].  It is created when [t] is
       created, and starts a deferred computation that calls [Unix.fcntl_getfl].
       [open_flags] is used to report an error when [fd] is not writable.  [Fd] treats the
       call to [fcntl_getfl] as an active system call, which prevents [Unix.close fd] from
       completing until [fcntl_getfl] finishes.  This prevents a file-descriptor or thread
       leak even though client code doesn't explicitly wait on [open_flags]. *)
    open_flags : open_flags Deferred.t
  ; line_ending : Line_ending.t
  ; (* If specified, subsequent writes are synchronously redirected here. *)
    mutable backing_out_channel : Backing_out_channel.t option
  }
[@@deriving fields]

let sexp_of_t t = [%sexp (t.fd : Fd.t_hum)]

type t_internals = t

let sexp_of_t_internals
      { id
      ; fd
      ; monitor
      ; inner_monitor
      ; background_writer_state
      ; background_writer_stopped
      ; syscall
      ; bytes_received
      ; bytes_written
      ; scheduled = _
      ; scheduled_bytes
      ; buf = _
      ; scheduled_back
      ; back
      ; time_source
      ; flushes = _
      ; close_state
      ; close_finished
      ; close_started
      ; producers_to_flush_at_close
      ; flush_at_shutdown_elt
      ; check_buffer_age
      ; consumer_left
      ; raise_when_consumer_leaves
      ; open_flags
      ; line_ending
      ; backing_out_channel
      }
  =
  let suppress_in_test x = if am_running_inline_test then None else Some x in
  let monitor_name_in_test monitor =
    if am_running_inline_test
    then [%sexp (Monitor.name monitor : Info.t)]
    else [%sexp (monitor : Monitor.t)]
  in
  let time_source =
    if phys_equal time_source (Time_source.wall_clock ()) then None else Some time_source
  in
  (* [open_flags] are non-deterministic across CentOS versions and have been suppressed in
     tests.  Linux kernels (CentOS 6) expose O_CLOEXEC via fcntl(fd, F_GETFL), but newer
     (CentOS 7) ones don't *)
  [%sexp
    { id = (suppress_in_test id : (Id.t option[@sexp.option]))
    ; fd = (suppress_in_test fd : (Fd.t option[@sexp.option]))
    ; monitor = (monitor_name_in_test monitor : Sexp.t)
    ; inner_monitor = (monitor_name_in_test inner_monitor : Sexp.t)
    ; background_writer_state : [ `Running
                                | `Not_running
                                | `Stopped_permanently of Stop_reason.t
                                ]
    ; background_writer_stopped : unit Ivar.t
    ; syscall : [ `Per_cycle | `Periodic of Time.Span.t ]
    ; bytes_received : Int63.t
    ; bytes_written : Int63.t
    ; scheduled_bytes : int
    ; scheduled_back : int
    ; back : int
    ; time_source : (Time_source.t option[@sexp.option])
    ; close_state : [ `Open | `Closed_and_flushing | `Closed ]
    ; close_finished : unit Ivar.t
    ; close_started : unit Ivar.t
    ; num_producers_to_flush_at_close = (Bag.length producers_to_flush_at_close : int)
    ; flush_at_shutdown_elt =
        (suppress_in_test flush_at_shutdown_elt : ((t[@sexp.opaque]) Bag.Elt.t option
                                                     option[@sexp.option]))
    ; check_buffer_age =
        (suppress_in_test check_buffer_age : ((t[@sexp.opaque]) Check_buffer_age'.t
                                                Bag.Elt.t
                                                option
                                                option[@sexp.option]))
    ; consumer_left : unit Ivar.t
    ; raise_when_consumer_leaves : bool
    ; open_flags =
        (suppress_in_test open_flags : (open_flags Deferred.t option[@sexp.option]))
    ; line_ending : Line_ending.t
    ; backing_out_channel : (Backing_out_channel.t option[@sexp.option])
    }]
;;

type writer = t [@@deriving sexp_of]

let set_raise_when_consumer_leaves t bool = t.raise_when_consumer_leaves <- bool
let bytes_to_write t = t.scheduled_bytes + t.back - t.scheduled_back

let is_stopped_permanently t =
  match t.background_writer_state with
  | `Stopped_permanently _ -> true
  | `Running | `Not_running -> false
;;

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~id:ignore
      ~fd:ignore
      ~monitor:ignore
      ~inner_monitor:ignore
      ~buf:ignore
      ~background_writer_state:
        (check (function
           | `Stopped_permanently _ ->
             assert (bytes_to_write t = 0);
             assert (Ivar.is_full t.background_writer_stopped)
           | `Running | `Not_running ->
             assert (Bigstring.length t.buf > 0);
             assert (
               Int63.(t.bytes_received - t.bytes_written = of_int (bytes_to_write t)));
             assert (Ivar.is_empty t.background_writer_stopped)))
      ~background_writer_stopped:ignore
      ~syscall:ignore
      ~bytes_written:
        (check (fun bytes_written ->
           assert (Int63.(zero <= bytes_written && bytes_written <= t.bytes_received))))
      ~bytes_received:ignore
      ~scheduled:
        (check (fun (scheduled : Scheduled.t) ->
           Deque.iter scheduled ~f:(fun (iovec, kind) ->
             if phys_equal t.buf iovec.buf
             then
               assert (
                 match kind with
                 | Keep -> true
                 | Destroy -> false))))
      ~scheduled_bytes:
        (check (fun scheduled_bytes ->
           assert (scheduled_bytes = Scheduled.length t.scheduled)))
      ~scheduled_back:
        (check (fun scheduled_back ->
           assert (0 <= scheduled_back && scheduled_back <= t.back)))
      ~back:(check (fun back -> assert (back <= Bigstring.length t.buf)))
      ~time_source:ignore
      ~flushes:ignore
      ~close_state:ignore
      ~close_finished:
        (check (fun close_finished ->
           match t.close_state with
           | `Open | `Closed_and_flushing -> assert (Ivar.is_empty close_finished)
           | `Closed -> ()))
      ~close_started:
        (check (fun close_started ->
           [%test_result: bool]
             (Ivar.is_empty close_started)
             ~expect:
               (match t.close_state with
                | `Open -> true
                | `Closed | `Closed_and_flushing -> false)))
      ~producers_to_flush_at_close:ignore
      ~flush_at_shutdown_elt:
        (check (fun o ->
           assert (Bool.equal (is_none o) (Ivar.is_full t.close_finished));
           Option.iter o ~f:(fun elt -> assert (phys_equal t (Bag.Elt.value elt)))))
      ~check_buffer_age:ignore
      ~consumer_left:
        (check (fun consumer_left ->
           if Ivar.is_full consumer_left then assert (is_stopped_permanently t)))
      ~raise_when_consumer_leaves:ignore
      ~open_flags:ignore
      ~line_ending:ignore
      ~backing_out_channel:(check (Option.invariant Backing_out_channel.invariant))
  with
  | exn ->
    raise_s [%message "writer invariant failed" (exn : exn) ~writer:(t : t_internals)]
;;

module Check_buffer_age : sig
  type t = writer Check_buffer_age'.t Bag.Elt.t option

  val dummy : t
  val create : writer -> maximum_age:[ `At_most of Time.Span.t | `Unlimited ] -> t
  val destroy : t -> unit
  val too_old : t -> unit Deferred.t

  module Internal_for_unit_test : sig
    val check_now : check_invariants:bool -> time_source:Time_source.t -> unit
    val num_active_checks_for : Time_source.t -> int option
  end
end = struct
  open Check_buffer_age'

  type t = writer Check_buffer_age'.t Bag.Elt.t option

  let elt_invariant t : unit =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f field = f (Field.get field t) in
      assert (Queue.length t.bytes_received_queue = Queue.length t.times_received_queue);
      Fields.iter
        ~writer:ignore
        ~maximum_age:ignore
        ~too_old:
          (check (fun ivar ->
             let imply a b = (not a) || b in
             assert (
               imply
                 Int63.O.(
                   t.bytes_received_at_now_minus_maximum_age > t.writer.bytes_written)
                 (Ivar.is_full ivar))))
        ~bytes_received_queue:
          (check (fun q ->
             let n =
               Queue.fold
                 q
                 ~init:t.bytes_received_at_now_minus_maximum_age
                 ~f:(fun prev count ->
                   assert (Int63.( < ) prev count);
                   count)
             in
             assert (Int63.( <= ) n t.writer.bytes_received);
             assert (Int63.( = ) n t.bytes_seen)))
        ~times_received_queue:
          (check (fun q ->
             match Queue.to_list q with
             | [] -> ()
             | times ->
               [%test_result: Time_ns.t list]
                 ~expect:times
                 (List.sort times ~compare:Time_ns.compare);
               assert (
                 Time_ns.Span.( <= )
                   (Time_ns.diff (List.last_exn times) (List.hd_exn times))
                   t.maximum_age)))
        ~bytes_received_at_now_minus_maximum_age:ignore
        ~bytes_seen:ignore
        ~for_this_time_source:ignore)
  ;;

  let dummy = None

  (* [sync] prunes history by removing all the entries from [*_received_queue]s that
     correspond to bytes already written or times older than [now - time_received]. *)
  let rec sync e ~now =
    if not (Queue.is_empty e.bytes_received_queue)
    then (
      let bytes_received = Queue.peek_exn e.bytes_received_queue in
      let time_received = Queue.peek_exn e.times_received_queue in
      let bytes_are_written = Int63.( <= ) bytes_received e.writer.bytes_written in
      let bytes_are_too_old =
        Time_ns.Span.( > ) (Time_ns.diff now time_received) e.maximum_age
      in
      if bytes_are_too_old
      then e.bytes_received_at_now_minus_maximum_age <- bytes_received;
      if bytes_are_written || bytes_are_too_old
      then (
        ignore (Queue.dequeue_exn e.bytes_received_queue : Int63.t);
        ignore (Queue.dequeue_exn e.times_received_queue : Time_ns.t);
        sync e ~now))
  ;;

  module Per_time_source = struct
    type t = writer Check_buffer_age'.per_time_source

    let process_active_check e =
      let now = Time_source.now e.writer.time_source in
      sync e ~now;
      let bytes_received = e.writer.bytes_received in
      let bytes_written = e.writer.bytes_written in
      if Int63.O.(bytes_received > e.bytes_seen)
      then (
        e.bytes_seen <- bytes_received;
        if Int63.O.(bytes_received > bytes_written)
        then (
          Queue.enqueue e.bytes_received_queue e.writer.bytes_received;
          Queue.enqueue e.times_received_queue now));
      let too_old =
        Int63.O.(e.bytes_received_at_now_minus_maximum_age > bytes_written)
      in
      match Ivar.is_full e.too_old, too_old with
      | true, true | false, false -> ()
      | true, false -> e.too_old <- Ivar.create ()
      | false, true ->
        Ivar.fill e.too_old ();
        let writer = e.writer in
        (* [Monitor.send_exn] enqueues jobs but does not run user code, and so cannot
           modify [e]. *)
        Monitor.send_exn
          e.writer.monitor
          (Exn.create_s
             [%message
               "writer buffer has data older than"
                 ~maximum_age:(e.maximum_age : Time_ns.Span.t)
                 ~beginning_of_buffer:
                   (Bigstring.to_string
                      writer.buf
                      ~pos:0
                      ~len:(Int.min 1024 (Bigstring.length writer.buf))
                    : string)
                 (writer : writer)])
    ;;

    let create () = { active_checks = Bag.create (); closed = Ivar.create () }
    let check t = Bag.iter t.active_checks ~f:process_active_check

    let internal_check_now_for_unit_test t ~check_invariants =
      if check_invariants then Bag.iter t.active_checks ~f:elt_invariant;
      check t
    ;;
  end

  module Time_source_key = Hashable.Make_plain (struct
      type t = Time_source.t [@@deriving sexp_of]

      let hash_fold_t state t = Time_source.Id.hash_fold_t state (Time_source.id t)
      let hash t = Time_source.Id.hash (Time_source.id t)
      let compare t1 t2 = Time_source.Id.compare (Time_source.id t1) (Time_source.id t2)
    end)

  (* [by_time_source] holds the set of [Per_time_source.t]'s with nonempty [active_checks]. *)
  let by_time_source : Per_time_source.t Time_source_key.Table.t =
    Time_source_key.Table.create ()
  ;;

  module Internal_for_unit_test = struct
    let num_active_checks_for time_source =
      Option.map (Hashtbl.find by_time_source time_source) ~f:(fun pt ->
        Bag.length pt.active_checks)
    ;;

    let check_now ~check_invariants ~time_source =
      Per_time_source.internal_check_now_for_unit_test
        (Hashtbl.find_exn by_time_source time_source)
        ~check_invariants
    ;;
  end

  let create writer ~maximum_age =
    match maximum_age with
    | `Unlimited -> None
    | `At_most maximum_age ->
      let time_source = writer.time_source in
      let for_this_time_source =
        Hashtbl.find_or_add by_time_source time_source ~default:(fun () ->
          let pt = Per_time_source.create () in
          Time_source.every
            time_source
            Time_ns.Span.second
            ~stop:(Ivar.read pt.closed)
            ~continue_on_error:false
            (fun () -> Per_time_source.check pt);
          pt)
      in
      Some
        (Bag.add
           for_this_time_source.active_checks
           { writer
           ; bytes_received_queue = Queue.create ()
           ; times_received_queue = Queue.create ()
           ; maximum_age = Time_ns.Span.of_span_float_round_nearest maximum_age
           ; bytes_seen = Int63.zero
           ; bytes_received_at_now_minus_maximum_age = Int63.zero
           ; too_old = Ivar.create ()
           ; for_this_time_source
           })
  ;;

  let destroy t =
    match t with
    | None -> ()
    | Some elt ->
      let t = Bag.Elt.value elt in
      let per_time_source = t.for_this_time_source in
      Bag.remove per_time_source.active_checks elt;
      if Bag.is_empty per_time_source.active_checks
      then (
        Hashtbl.remove by_time_source t.writer.time_source;
        Ivar.fill_if_empty per_time_source.closed ())
  ;;

  let too_old t =
    match t with
    | None -> Deferred.never ()
    | Some elt -> Ivar.read (Bag.Elt.value elt).too_old
  ;;
end

let flushed_or_failed_with_result t =
  match t.backing_out_channel with
  | Some backing_out_channel ->
    Backing_out_channel.flush backing_out_channel;
    return (Flush_result.Flushed (Time_source.now t.time_source))
  | None ->
    if Int63.O.(t.bytes_written = t.bytes_received)
    then return (Flush_result.Flushed (Time_source.now t.time_source))
    else (
      match t.background_writer_state with
      | `Stopped_permanently Error -> return Flush_result.Error
      | `Stopped_permanently Consumer_left -> return Flush_result.Consumer_left
      | `Running | `Not_running ->
        if Ivar.is_full t.close_finished
        then
          Deferred.return Flush_result.Error
        else
          Deferred.create (fun ivar -> Queue.enqueue t.flushes (ivar, t.bytes_received)))
;;

let eager_map t ~f =
  if Deferred.is_determined t
  then return (f (Deferred.value_exn t))
  else Deferred.map t ~f
;;

let eager_bind t ~f =
  if Deferred.is_determined t then f (Deferred.value_exn t) else Deferred.bind t ~f
;;

let flushed_or_failed_unit t = eager_map (flushed_or_failed_with_result t) ~f:ignore

let flushed_time_ns t =
  eager_bind (flushed_or_failed_with_result t) ~f:(function
    | Flushed t -> Deferred.return t
    | Error | Consumer_left -> Deferred.never ())
;;

let flushed_time t = eager_map (flushed_time_ns t) ~f:Time_ns.to_time_float_round_nearest

let flushed t =
  (* even though we don't promise any eagerness, there are tests in the tree
     that depend on it *)
  eager_map (flushed_time_ns t) ~f:(ignore : Time_ns.t -> unit)
;;

let set_backing_out_channel t backing_out_channel =
  t.backing_out_channel <- Some backing_out_channel
;;

let set_synchronous_backing_out_channel t backing_out_channel =
  let rec wait_until_no_bytes_to_write () =
    if bytes_to_write t = 0
    then (
      set_backing_out_channel t backing_out_channel;
      return ())
    else (
      let%bind () = flushed t in
      wait_until_no_bytes_to_write ())
  in
  wait_until_no_bytes_to_write ()
;;

let set_synchronous_out_channel t out_channel =
  set_synchronous_backing_out_channel t (Backing_out_channel.of_out_channel out_channel)
;;

let using_synchronous_backing_out_channel t = Option.is_some t.backing_out_channel

let clear_synchronous_out_channel t =
  if is_some t.backing_out_channel
  then (
    assert (bytes_to_write t = 0);
    t.backing_out_channel <- None)
;;

let with_synchronous_backing_out_channel t backing_out_channel ~f =
  let saved_backing_out_channel = t.backing_out_channel in
  (* This code will flush a bit more eagerly than it needs to if
     [with_synchronous_backing_out_channel t oc] is called recursively on the same [t] and
     [oc].  The flush is caused by [set_synchronous_backing_out_channel].  In theory this
     could happen but in practice is exceedingly unlikely. *)
  Monitor.protect
    (fun () ->
       let%bind () = set_synchronous_backing_out_channel t backing_out_channel in
       f ())
    ~finally:(fun () ->
      t.backing_out_channel <- saved_backing_out_channel;
      return ())
;;

let with_synchronous_out_channel t out_channel ~f =
  with_synchronous_backing_out_channel
    t
    ~f
    (Backing_out_channel.of_out_channel out_channel)
;;

let set_fd t fd =
  let%map () = flushed t in
  t.fd <- fd
;;

let consumer_left t = Ivar.read t.consumer_left
let close_finished t = Ivar.read t.close_finished
let close_started t = Ivar.read t.close_started

let is_closed t =
  match t.close_state with
  | `Open -> false
  | `Closed | `Closed_and_flushing -> true
;;

let is_open t = not (is_closed t)


let writers_to_flush_at_shutdown : t Bag.t = Bag.create ()

let final_flush ?force t =
  let producers_flushed =
    (* Note that each element of [producers_to_flush_at_close] checks that the upstream
       producer is flushed, which includes checking that [t] itself is flushed once the
       producer has written everything to [t].  So, there is no need to call [flushed t]
       after the producer is flushed. *)
    Deferred.List.iter
      ~how:`Parallel
      ~f:(fun f -> f ())
      (Bag.to_list t.producers_to_flush_at_close)
  in
  let force =
    match force with
    | Some fc -> fc
    | None ->
      (* We used to use [after (sec 5.)] as the default value for [force] for all kinds
         of underlying fds.  This was problematic, because it silently caused data in
         the writer's buffer to be dropped when it kicked in.  We care about data
         getting out only for the files, when we want to get data to disk.  When we
         close socket writers, we usually just want to drop the connection, so using
         [after (sec 5.)]  makes sense. *)
      (match Fd.kind t.fd with
       | File -> Deferred.never ()
       | Char | Fifo | Socket _ ->
         Time_source.after t.time_source (Time_ns.Span.of_sec 5.))
  in
  Deferred.any_unit
    [ (* If the consumer leaves, there's no more writing we can do. *)
      consumer_left t
    ; Deferred.all_unit [ producers_flushed; flushed t ]
    ; force
      ; (* The buffer-age check might fire while we're waiting. *)
      Check_buffer_age.too_old t.check_buffer_age
    ]
;;

let close ?force_close t =
  if debug then Debug.log "Writer.close" t [%sexp_of: t];
  (match t.close_state with
   | `Closed_and_flushing | `Closed -> ()
   | `Open ->
     t.close_state <- `Closed_and_flushing;
     Ivar.fill t.close_started ();
     final_flush t ?force:force_close
     >>> fun () ->
     t.close_state <- `Closed;
     Check_buffer_age.destroy t.check_buffer_age;
     (match t.flush_at_shutdown_elt with
      | None -> assert false
      | Some elt -> Bag.remove writers_to_flush_at_shutdown elt);
     Unix.close t.fd >>> fun () -> Ivar.fill t.close_finished ());
  close_finished t
;;

let () =
  Shutdown.at_shutdown (fun () ->
    if debug then Debug.log_string "Writer.at_shutdown";
    Deferred.List.iter
      ~how:`Parallel
      (Bag.to_list writers_to_flush_at_shutdown)
      ~f:(fun t -> Deferred.any_unit [ final_flush t; close_finished t ]))
;;

let fill_flushes { bytes_written; flushes; time_source; _ } =
  if not (Queue.is_empty flushes)
  then (
    let now = Time_source.now time_source in
    let rec loop () =
      match Queue.peek flushes with
      | None -> ()
      | Some (ivar, z) ->
        if Int63.(z <= bytes_written)
        then (
          Ivar.fill ivar (Flush_result.Flushed now);
          ignore (Queue.dequeue flushes : (Flush_result.t Ivar.t * Int63.t) option);
          loop ())
    in
    loop ())
;;

let stop_permanently t (outcome : Stop_reason.t) =
  t.background_writer_state <- `Stopped_permanently outcome;
  Deque.clear t.scheduled;
  t.scheduled_bytes <- 0;
  t.buf <- Bigstring.create 0;
  t.scheduled_back <- 0;
  t.back <- 0;
  Ivar.fill_if_empty t.background_writer_stopped ();
  Queue.iter t.flushes ~f:(fun (ivar, _) ->
    Ivar.fill
      ivar
      (match outcome with
       | Error -> Flush_result.Error
       | Consumer_left -> Flush_result.Consumer_left));
  Queue.clear t.flushes
;;

let stopped_permanently t = Ivar.read t.background_writer_stopped

let die t sexp =
  stop_permanently t Error;
  raise_s sexp
;;

type buffer_age_limit =
  [ `At_most of Time.Span.t
  | `Unlimited
  ]
[@@deriving bin_io, sexp]

let create
      ?buf_len
      ?(syscall = `Per_cycle)
      ?buffer_age_limit
      ?(raise_when_consumer_leaves = true)
      ?(line_ending = Line_ending.Unix)
      ?time_source
      fd
  =
  let time_source =
    match time_source with
    | Some x -> Time_source.read_only x
    | None -> Time_source.wall_clock ()
  in
  let buffer_age_limit =
    match buffer_age_limit with
    | Some z -> z
    | None ->
      (match Fd.kind fd with
       | File -> `Unlimited
       | Char | Fifo | Socket _ -> `At_most (Time.Span.of_min 2.))
  in
  let buf_len =
    match buf_len with
    | None -> 65 * 1024 * 2 (* largest observed single write call * 2 *)
    | Some buf_len ->
      if buf_len <= 0 then invalid_arg "Writer.create: buf_len <= 0" else buf_len
  in
  let id = Id.create () in
  let monitor =
    Monitor.create
      ()
      ?name:(if am_running_inline_test then Some "Writer.monitor" else None)
  in
  let inner_monitor =
    Monitor.create
      ()
      ?name:(if am_running_inline_test then Some "Writer.inner_monitor" else None)
  in
  let consumer_left = Ivar.create () in
  let open_flags = try_with (fun () -> Unix.fcntl_getfl fd) in
  let t =
    { id
    ; fd
    ; syscall
    ; monitor
    ; inner_monitor
    ; buf = Bigstring.create buf_len
    ; back = 0
    ; scheduled_back = 0
    ; scheduled = Deque.create ()
    ; scheduled_bytes = 0
    ; bytes_received = Int63.zero
    ; bytes_written = Int63.zero
    ; time_source
    ; flushes = Queue.create ()
    ; background_writer_state = `Not_running
    ; background_writer_stopped = Ivar.create ()
    ; close_state = `Open
    ; close_finished = Ivar.create ()
    ; close_started = Ivar.create ()
    ; producers_to_flush_at_close = Bag.create ()
    ; flush_at_shutdown_elt = None
    ; check_buffer_age = Check_buffer_age.dummy
    ; consumer_left
    ; raise_when_consumer_leaves
    ; open_flags
    ; line_ending
    ; backing_out_channel = None
    }
  in
  Monitor.detach_and_iter_errors inner_monitor ~f:(fun (exn : Exn.t) ->
    Monitor.send_exn
      monitor
      (Exn.create_s
         [%message
           "Writer error from inner_monitor"
             ~_:(Monitor.extract_exn exn : Exn.t)
             ~writer:(t : t_internals)]));
  t.check_buffer_age <- Check_buffer_age.create t ~maximum_age:buffer_age_limit;
  t.flush_at_shutdown_elt <- Some (Bag.add writers_to_flush_at_shutdown t);
  t
;;

let set_buffer_age_limit t maximum_age =
  Check_buffer_age.destroy t.check_buffer_age;
  t.check_buffer_age <- Check_buffer_age.create t ~maximum_age
;;

let of_out_channel oc kind = create (Fd.of_out_channel oc kind)

let can_write t =
  match t.close_state with
  | `Open | `Closed_and_flushing -> true
  | `Closed -> false
;;

let ensure_can_write t =
  if not (can_write t) then raise_s [%message "attempt to use closed writer" ~_:(t : t)]
;;

let open_file
      ?(append = false)
      ?buf_len
      ?syscall
      ?(perm = 0o666)
      ?line_ending
      ?time_source
      file
  =
  (* Writing to NFS needs the [`Trunc] flag to avoid leaving extra junk at the end of
     a file. *)
  let mode = [ `Wronly; `Creat ] in
  let mode = (if append then `Append else `Trunc) :: mode in
  Unix.openfile file ~mode ~perm >>| create ?buf_len ?syscall ?line_ending ?time_source
;;

let with_close t ~f = Monitor.protect f ~finally:(fun () -> close t)

let with_writer_exclusive t f =
  let%bind () = Unix.lockf t.fd Exclusive in
  Monitor.protect f ~finally:(fun () ->
    let%map () = flushed t in
    Unix.unlockf t.fd)
;;

let with_file
      ?perm
      ?append
      ?syscall
      ?(exclusive = false)
      ?line_ending
      ?time_source
      file
      ~f
  =
  let%bind t = open_file ?perm ?append ?syscall ?line_ending ?time_source file in
  with_close t ~f:(fun () ->
    if exclusive then with_writer_exclusive t (fun () -> f t) else f t)
;;

let got_bytes t n = t.bytes_received <- Int63.(t.bytes_received + of_int n)

let add_iovec t kind (iovec : _ IOVec.t) ~count_bytes_as_received =
  assert (t.scheduled_back = t.back);
  if count_bytes_as_received then got_bytes t iovec.len;
  if not (is_stopped_permanently t)
  then (
    t.scheduled_bytes <- t.scheduled_bytes + iovec.len;
    Deque.enqueue_back t.scheduled (iovec, kind));
  assert (t.scheduled_back = t.back)
;;

let schedule_unscheduled t kind =
  let need_to_schedule = t.back - t.scheduled_back in
  assert (need_to_schedule >= 0);
  if need_to_schedule > 0
  then (
    let pos = t.scheduled_back in
    t.scheduled_back <- t.back;
    add_iovec
      t
      kind
      (IOVec.of_bigstring t.buf ~pos ~len:need_to_schedule)
      ~count_bytes_as_received:false
      (* they were already counted *))
;;

let dummy_iovec = IOVec.empty IOVec.bigstring_kind

let mk_iovecs t =
  schedule_unscheduled t Keep;
  let n_iovecs = Int.min (Deque.length t.scheduled) (Lazy.force IOVec.max_iovecs) in
  let iovecs = Array.create ~len:n_iovecs dummy_iovec in
  let contains_mmapped_ref = ref false in
  let iovecs_len = ref 0 in
  with_return (fun r ->
    let i = ref 0 in
    Deque.iter t.scheduled ~f:(fun (iovec, _) ->
      if !i >= n_iovecs then r.return ();
      if (not !contains_mmapped_ref) && Bigstring.is_mmapped iovec.buf
      then contains_mmapped_ref := true;
      iovecs_len := !iovecs_len + iovec.len;
      iovecs.(!i) <- iovec;
      incr i));
  iovecs, !contains_mmapped_ref, !iovecs_len
;;

(* Size of I/O- or blit operation for which a helper thread should be used.  This number
   (a power of two) is somewhat empirically motivated, but there is no reason why it
   should be the best. *)
let thread_io_cutoff = 262_144

let is_running = function
  | `Running -> true
  | _ -> false
;;

(* If the writer was closed, we should be quiet.  But if it wasn't, then someone was
   monkeying around with the fd behind our back, and we should complain. *)
let fd_closed t =
  if not (is_closed t) then die t [%message "writer fd unexpectedly closed "]
;;

let rec start_write t =
  if debug then Debug.log "Writer.start_write" t [%sexp_of: t];
  assert (is_running t.background_writer_state);
  let iovecs, contains_mmapped, iovecs_len = mk_iovecs t in
  let handle_write_result = function
    | `Already_closed -> fd_closed t
    | `Ok n ->
      if n >= 0
      then write_finished t n
      else die t [%message "write system call returned negative result" (n : int)]
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) -> write_when_ready t
    | `Error (Unix.Unix_error (EBADF, _, _)) -> die t [%message "write got EBADF"]
    | `Error
        (Unix.Unix_error
           ( ( EPIPE
             | ECONNRESET
             | EHOSTUNREACH
             | ENETDOWN
             | ENETRESET
             | ENETUNREACH
             | ETIMEDOUT )
           , _
           , _ ) as exn) ->
      (* [t.consumer_left] is empty since once we reach this point, we stop the writer
         permanently, and so will never reach here again. *)
      assert (Ivar.is_empty t.consumer_left);
      Ivar.fill t.consumer_left ();
      if t.raise_when_consumer_leaves
      then (
        stop_permanently t Error;
        raise exn)
      else stop_permanently t Consumer_left
    | `Error exn -> die t [%message "" ~_:(exn : Exn.t)]
  in
  let should_write_in_thread =
    (not (Fd.supports_nonblock t.fd))
    (* Though the write will not block in this case, a memory-mapped bigstring in an
       I/O-vector may cause a page fault, which would cause the async scheduler thread
       to block.  So, we write in a separate thread, and the [Bigstring.writev] releases
       the OCaml lock, allowing the async scheduler thread to continue. *)
    || iovecs_len > thread_io_cutoff
    || contains_mmapped
  in
  if should_write_in_thread
  then
    Fd.syscall_in_thread t.fd ~name:"writev" (fun file_descr ->
      Bigstring_unix.writev file_descr iovecs)
    >>> handle_write_result
  else
    handle_write_result
      (Fd.syscall t.fd ~nonblocking:true (fun file_descr ->
         Bigstring_unix.writev_assume_fd_is_nonblocking file_descr iovecs))

and write_when_ready t =
  if debug then Debug.log "Writer.write_when_ready" t [%sexp_of: t];
  assert (is_running t.background_writer_state);
  Fd.ready_to t.fd `Write
  >>> function
  | `Bad_fd -> die t [%message "writer ready_to got Bad_fd"]
  | `Closed -> fd_closed t
  | `Ready -> start_write t

and write_finished t bytes_written =
  if debug then Debug.log "Writer.write_finished" (bytes_written, t) [%sexp_of: int * t];
  assert (is_running t.background_writer_state);
  let int63_bytes_written = Int63.of_int bytes_written in
  Io_stats.update io_stats ~kind:(Fd.kind t.fd) ~bytes:int63_bytes_written;
  t.bytes_written <- Int63.(int63_bytes_written + t.bytes_written);
  if Int63.(t.bytes_written > t.bytes_received)
  then die t [%message "writer wrote more bytes than it received"];
  fill_flushes t;
  t.scheduled_bytes <- t.scheduled_bytes - bytes_written;
  (* Remove processed iovecs from t.scheduled. *)
  let rec remove_done bytes_written =
    assert (bytes_written >= 0);
    match Deque.dequeue_front t.scheduled with
    | None ->
      if bytes_written > 0
      then die t [%message "writer wrote nonzero amount but IO_queue is empty"]
    | Some ({ buf; pos; len }, kind) ->
      if bytes_written >= len
      then (
        (* Current I/O-vector completely written.  Internally generated buffers get
           destroyed immediately unless they are still in use for buffering.  *)
        (match kind with
         | Destroy -> Bigstring.unsafe_destroy buf
         | Keep -> ());
        remove_done (bytes_written - len))
      else (
        (* Partial I/O: update partially written I/O-vector and retry I/O. *)
        let new_iovec =
          IOVec.of_bigstring buf ~pos:(pos + bytes_written) ~len:(len - bytes_written)
        in
        Deque.enqueue_front t.scheduled (new_iovec, kind))
  in
  remove_done bytes_written;
  (* See if there's anything else to do. *)
  schedule_unscheduled t Keep;
  if Deque.is_empty t.scheduled
  then (
    t.back <- 0;
    t.scheduled_back <- 0;
    t.background_writer_state <- `Not_running)
  else (
    match t.syscall with
    | `Per_cycle -> write_when_ready t
    | `Periodic span ->
      Time_source.after t.time_source (Time_ns.Span.of_span_float_round_nearest span)
      >>> fun _ -> start_write t)
;;

let maybe_start_writer t =
  match t.background_writer_state with
  | `Stopped_permanently _ | `Running -> ()
  | `Not_running ->
    if bytes_to_write t > 0
    then (
      t.background_writer_state <- `Running;
      (* We schedule the background writer thread to run with low priority, so that it
         runs at the end of the cycle and that all of the calls to Writer.write will
         usually be batched into a single system call. *)
      schedule ~monitor:t.inner_monitor ~priority:Priority.low (fun () ->
        t.open_flags
        >>> fun open_flags ->
        let can_write_fd =
          match open_flags with
          | Error _ -> false
          | Ok flags -> Unix.Open_flags.can_write flags
        in
        if not can_write_fd
        then
          (* The reason we produce a custom error message in this case is that
             Linux conflates this case with "not a valid file descriptor" (EBADF), which
             normally indicates a serious bug in file descriptor handling. *)
          die
            t
            [%message
              "not allowed to write due to file-descriptor flags"
                (open_flags : open_flags)];
        start_write t))
;;

let give_buf t desired =
  assert (desired > 0);
  assert (not (is_stopped_permanently t));
  got_bytes t desired;
  let buf_len = Bigstring.length t.buf in
  let available = buf_len - t.back in
  if desired <= available
  then (
    (* Data fits into buffer *)
    let pos = t.back in
    t.back <- t.back + desired;
    t.buf, pos)
  else if (* Preallocated buffer too small; schedule buffered writes.  We create a new buffer of
             exactly the desired size if the desired size is more than half the buffer length.
             If we only created a new buffer when the desired size was greater than the buffer
             length, then multiple consecutive writes of slightly more than half the buffer
             length would each waste slightly less than half of the buffer.  Although, it is
             still the case that multiple consecutive writes of slightly more than one quarter
             of the buffer length will waste slightly less than one quarter of the buffer. *)
    desired > buf_len / 2
  then (
    schedule_unscheduled t Keep;
    (* Preallocation size too small; allocate dedicated buffer *)
    let buf = Bigstring.create desired in
    add_iovec
      t
      Destroy
      (IOVec.of_bigstring ~len:desired buf)
      ~count_bytes_as_received:false;
    (* we already counted them above *)
    buf, 0)
  else (
    schedule_unscheduled t Destroy;
    (* Preallocation size sufficient; preallocate new buffer *)
    let buf = Bigstring.create buf_len in
    t.buf <- buf;
    t.scheduled_back <- 0;
    t.back <- desired;
    buf, 0)
;;

(* If [blit_to_bigstring] raises, [write_gen_unchecked] may leave some unexpected bytes in
   the bigstring.  However it leaves [t.back] and [t.bytes_received] in agreement. *)
let write_gen_internal
      (type a)
      t
      src
      ~src_pos
      ~src_len
      ~allow_partial_write
      ~(blit_to_bigstring : (a, Bigstring.t) Blit.blit)
  =
  if is_stopped_permanently t
  then got_bytes t src_len
  else (
    match t.backing_out_channel with
    | Some backing_out_channel ->
      got_bytes t src_len;
      Backing_out_channel.output
        backing_out_channel
        ~blit_to_bigstring
        ~src
        ~src_len
        ~src_pos;
      t.bytes_written <- Int63.(t.bytes_written + of_int src_len)
    | None ->
      let available = Bigstring.length t.buf - t.back in
      if available >= src_len
      then (
        got_bytes t src_len;
        let dst_pos = t.back in
        t.back <- dst_pos + src_len;
        blit_to_bigstring ~src ~src_pos ~len:src_len ~dst:t.buf ~dst_pos)
      else if allow_partial_write
      then (
        got_bytes t available;
        let dst_pos = t.back in
        t.back <- dst_pos + available;
        blit_to_bigstring ~src ~src_pos ~len:available ~dst:t.buf ~dst_pos;
        let remaining = src_len - available in
        let dst, dst_pos = give_buf t remaining in
        blit_to_bigstring
          ~src
          ~src_pos:(src_pos + available)
          ~len:remaining
          ~dst
          ~dst_pos)
      else (
        let dst, dst_pos = give_buf t src_len in
        blit_to_bigstring ~src ~src_pos ~dst ~dst_pos ~len:src_len);
      maybe_start_writer t)
;;

let write_direct t ~f =
  if is_stopped_permanently t
  then None
  else (
    let pos = t.back in
    let len = Bigstring.length t.buf - pos in
    let x, written = f t.buf ~pos ~len in
    if written < 0 || written > len
    then
      raise_s
        [%message
          "[write_direct]'s [~f] argument returned invalid [written]"
            (written : int)
            (len : int)
            ~writer:(t : t)];
    t.back <- pos + written;
    got_bytes t written;
    maybe_start_writer t;
    Some x)
;;

let write_gen_unchecked ?pos ?len t src ~blit_to_bigstring ~length =
  let src_pos, src_len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length src)
  in
  write_gen_internal t src ~src_pos ~src_len ~allow_partial_write:true ~blit_to_bigstring
;;

let write_gen_whole_unchecked t src ~blit_to_bigstring ~length =
  let src_len = length src in
  write_gen_internal
    t
    src
    ~src_pos:0
    ~src_len
    ~allow_partial_write:false
    ~blit_to_bigstring:(fun ~src ~src_pos ~dst ~dst_pos ~len ->
      assert (src_pos = 0);
      assert (len = src_len);
      blit_to_bigstring src dst ~pos:dst_pos)
;;

let write_bytes ?pos ?len t src =
  write_gen_unchecked
    ?pos
    ?len
    t
    src
    ~blit_to_bigstring:Bigstring.From_bytes.blit
    ~length:Bytes.length
;;

let write ?pos ?len t src =
  write_gen_unchecked
    ?pos
    ?len
    t
    src
    ~blit_to_bigstring:Bigstring.From_string.blit
    ~length:String.length
;;

let write_bigstring ?pos ?len t src =
  write_gen_unchecked
    ?pos
    ?len
    t
    src
    ~blit_to_bigstring:Bigstring.blit
    ~length:Bigstring.length
;;

let write_iobuf ?pos ?len t iobuf =
  let iobuf = Iobuf.read_only (Iobuf.no_seek iobuf) in
  write_gen_unchecked
    ?pos
    ?len
    t
    iobuf
    ~blit_to_bigstring:Iobuf.Peek.To_bigstring.blit
    ~length:Iobuf.length
;;

let write_substring t substring =
  write_bytes
    t
    (Substring.base substring)
    ~pos:(Substring.pos substring)
    ~len:(Substring.length substring)
;;

let write_bigsubstring t bigsubstring =
  write_bigstring
    t
    (Bigsubstring.base bigsubstring)
    ~pos:(Bigsubstring.pos bigsubstring)
    ~len:(Bigsubstring.length bigsubstring)
;;

let writef t = ksprintf (fun s -> write t s)

let write_gen ?pos ?len t src ~blit_to_bigstring ~length =
  try write_gen_unchecked ?pos ?len t src ~blit_to_bigstring ~length with
  | exn -> die t [%message "Writer.write_gen: error writing value" (exn : exn)]
;;

let write_gen_whole t src ~blit_to_bigstring ~length =
  try write_gen_whole_unchecked t src ~blit_to_bigstring ~length with
  | exn -> die t [%message "Writer.write_gen_whole: error writing value" (exn : exn)]
;;

let to_formatter t =
  Format.make_formatter
    (fun str pos len ->
       let str = Bytes.of_string str in
       ensure_can_write t;
       write_substring t (Substring.create str ~pos ~len))
    ignore
;;

let write_char t c =
  if is_stopped_permanently t
  then got_bytes t 1
  else (
    (* Check for the common case that the char can simply be put in the buffer. *)
    match t.backing_out_channel with
    | Some backing_out_channel ->
      got_bytes t 1;
      Backing_out_channel.output_char backing_out_channel c;
      t.bytes_written <- Int63.(t.bytes_written + of_int 1)
    | None ->
      if Bigstring.length t.buf - t.back >= 1
      then (
        got_bytes t 1;
        t.buf.{t.back} <- c;
        t.back <- t.back + 1)
      else (
        let dst, dst_pos = give_buf t 1 in
        dst.{dst_pos} <- c);
      maybe_start_writer t)
;;

let newline ?line_ending t =
  let line_ending =
    match line_ending with
    | Some x -> x
    | None -> t.line_ending
  in
  (match line_ending with
   | Unix -> ()
   | Dos -> write_char t '\r');
  write_char t '\n'
;;

let write_line ?line_ending t s =
  write t s;
  newline t ?line_ending
;;

let write_byte t i = write_char t (char_of_int (i % 256))

module Terminate_with = struct
  type t =
    | Newline
    | Space_if_needed
  [@@deriving sexp_of]
end

let write_sexp_internal =
  let initial_size = 10 * 1024 in
  let buffer = lazy (Buffer.create initial_size) in
  let blit_str = ref (Bytes.create 0) in
  fun ~(terminate_with : Terminate_with.t) ?(hum = false) t sexp ->
    let buffer = Lazy.force buffer in
    Buffer.clear buffer;
    if hum
    then Sexp.to_buffer_hum ~buf:buffer ~indent:!Sexp.default_indent sexp
    else Sexp.to_buffer ~buf:buffer sexp;
    let len = Buffer.length buffer in
    let blit_str_len = Bytes.length !blit_str in
    if len > blit_str_len
    then blit_str := Bytes.create (max len (max initial_size (2 * blit_str_len)));
    Buffer.blit ~src:buffer ~src_pos:0 ~dst:!blit_str ~dst_pos:0 ~len;
    write_bytes t !blit_str ~len;
    match terminate_with with
    | Newline -> newline t
    | Space_if_needed ->
      (* If the string representation doesn't start/end with paren or double quote, we add
         a space after it to ensure that the parser can recognize the end of the sexp. *)
      let c = Bytes.get !blit_str 0 in
      if not Char.O.(c = '(' || c = '"') then write_char t ' '
;;

let write_sexp ?hum ?(terminate_with = Terminate_with.Space_if_needed) t sexp =
  write_sexp_internal t sexp ?hum ~terminate_with
;;

let write_bin_prot t (writer : _ Bin_prot.Type_class.writer) v =
  let len = writer.size v in
  assert (len > 0);
  let tot_len = len + Bin_prot.Utils.size_header_length in
  if is_stopped_permanently t
  then got_bytes t tot_len
  else (
    let buf, start_pos = give_buf t tot_len in
    ignore (Bigstring.write_bin_prot buf ~pos:start_pos writer v : int);
    maybe_start_writer t)
;;

let write_bin_prot_no_size_header t ~size write v =
  if is_stopped_permanently t
  then got_bytes t size
  else (
    let buf, start_pos = give_buf t size in
    let end_pos = write buf ~pos:start_pos v in
    let written = end_pos - start_pos in
    if written <> size
    then
      raise_s
        [%message
          "Writer.write_bin_prot_no_size_header bug!" (written : int) (size : int)];
    maybe_start_writer t)
;;

let send t s =
  write t (string_of_int (String.length s) ^ "\n");
  write t s
;;

let schedule_iovec ?(destroy_or_keep = Destroy_or_keep.Keep) t iovec =
  schedule_unscheduled t Keep;
  add_iovec t destroy_or_keep iovec ~count_bytes_as_received:true;
  maybe_start_writer t
;;

let schedule_iovecs t iovecs =
  schedule_unscheduled t Keep;
  Queue.iter iovecs ~f:(add_iovec t Keep ~count_bytes_as_received:true);
  Queue.clear iovecs;
  maybe_start_writer t
;;

let schedule_bigstring ?destroy_or_keep t ?pos ?len bstr =
  schedule_iovec t (IOVec.of_bigstring ?pos ?len bstr) ?destroy_or_keep
;;

let schedule_bigsubstring t bigsubstring =
  schedule_bigstring
    t
    (Bigsubstring.base bigsubstring)
    ~pos:(Bigsubstring.pos bigsubstring)
    ~len:(Bigsubstring.length bigsubstring)
;;

let schedule_iobuf_peek t ?pos ?len iobuf =
  schedule_iovec t (Iobuf_unix.Expert.to_iovec_shared ?pos ?len iobuf)
;;

let schedule_iobuf_consume t ?len iobuf =
  let iovec = Iobuf_unix.Expert.to_iovec_shared ?len iobuf in
  let len = iovec.len in
  schedule_iovec t iovec;
  let%map _ = flushed_time t in
  Iobuf.advance iobuf len
;;

(* The code below ensures that no calls happen on a closed writer. *)
let fsync t =
  ensure_can_write t;
  let%bind () = flushed t in
  Unix.fsync t.fd
;;

let fdatasync t =
  ensure_can_write t;
  let%bind () = flushed t in
  Unix.fdatasync t.fd
;;

let write_bin_prot t sw_arg v =
  ensure_can_write t;
  write_bin_prot t sw_arg v
;;

let send t s =
  ensure_can_write t;
  send t s
;;

let schedule_iovec ?destroy_or_keep t iovec =
  ensure_can_write t;
  schedule_iovec ?destroy_or_keep t iovec
;;

let schedule_iovecs t iovecs =
  ensure_can_write t;
  schedule_iovecs t iovecs
;;

let schedule_bigstring t ?pos ?len bstr =
  ensure_can_write t;
  schedule_bigstring t ?pos ?len bstr
;;

let schedule_bigsubstring t bigsubstring =
  ensure_can_write t;
  schedule_bigsubstring t bigsubstring
;;

let schedule_iobuf_peek t ?pos ?len iobuf =
  ensure_can_write t;
  schedule_iobuf_peek t ?pos ?len iobuf
;;

let schedule_iobuf_consume t ?len iobuf =
  ensure_can_write t;
  schedule_iobuf_consume t ?len iobuf
;;

let write_gen ?pos ?len t src ~blit_to_bigstring ~length =
  ensure_can_write t;
  write_gen ?pos ?len t src ~blit_to_bigstring ~length
;;

let write_bytes ?pos ?len t s =
  ensure_can_write t;
  write_bytes ?pos ?len t s
;;

let write ?pos ?len t s =
  ensure_can_write t;
  write ?pos ?len t s
;;

let write_line ?line_ending t s =
  ensure_can_write t;
  write_line t s ?line_ending
;;

let writef t =
  ensure_can_write t;
  writef t
;;

let write_sexp ?hum ?terminate_with t s =
  ensure_can_write t;
  write_sexp ?hum ?terminate_with t s
;;

let write_iobuf ?pos ?len t iobuf =
  ensure_can_write t;
  write_iobuf ?pos ?len t iobuf
;;

let write_bigstring ?pos ?len t src =
  ensure_can_write t;
  write_bigstring ?pos ?len t src
;;

let write_bigsubstring t s =
  ensure_can_write t;
  write_bigsubstring t s
;;

let write_substring t s =
  ensure_can_write t;
  write_substring t s
;;

let write_byte t b =
  ensure_can_write t;
  write_byte t b
;;

let write_char t c =
  ensure_can_write t;
  write_char t c
;;

let newline ?line_ending t =
  ensure_can_write t;
  newline ?line_ending t
;;

let stdout_and_stderr =
  lazy
    ((* We [create] the writers inside [Monitor.main] so that it is their monitors'
        parent. *)
      match
        Scheduler.within_v ~monitor:Monitor.main (fun () ->
          let stdout = Fd.stdout () in
          let stderr = Fd.stderr () in
          let t = create stdout in
          let dev_and_ino fd =
            let stats = Core.Unix.fstat (Fd.file_descr_exn fd) in
            stats.st_dev, stats.st_ino
          in
          match am_test_runner with
          | true ->
            (* In tests, we use synchronous output to improve determinism, especially
               when mixing libraries that use Core and Async printing. *)
            set_backing_out_channel
              t
              (Backing_out_channel.of_out_channel Out_channel.stdout);
            t, t
          | false ->
            if [%compare.equal: int * int] (dev_and_ino stdout) (dev_and_ino stderr)
            then
              (* If stdout and stderr point to the same file, we must share a single writer
                 between them.  See the comment in writer.mli for details. *)
              t, t
            else t, create stderr)
      with
      | None -> raise_s [%message [%here] "unable to create stdout/stderr"]
      | Some v -> v)
;;

let stdout = lazy (fst (Lazy.force stdout_and_stderr))
let stderr = lazy (snd (Lazy.force stdout_and_stderr))

let use_synchronous_stdout_and_stderr () =
  let stdout, stderr = Lazy.force stdout_and_stderr in
  let ts_and_channels =
    (stdout, Out_channel.stdout)
    ::
    ((* We only set [stderr] if it is distinct from [stdout]. *)
      match phys_equal stdout stderr with
      | true -> []
      | false -> [ stderr, Out_channel.stderr ])
  in
  List.map ts_and_channels ~f:(fun (t, out_channel) ->
    set_synchronous_out_channel t out_channel)
  |> Deferred.all_unit
;;

(* This test is here rather than in a [test] directory because we want it to run
   immediately after [stdout] and [stderr] are defined, so that they haven't yet been
   forced. *)
let%expect_test "stdout and stderr are always the same in tests" =
  print_s [%message (Lazy.is_val stdout : bool)];
  [%expect {| ("Lazy.is_val stdout" false) |}];
  print_s [%message (Lazy.is_val stderr : bool)];
  [%expect {| ("Lazy.is_val stderr" false) |}];
  let module U = Core.Unix in
  let saved_stderr = U.dup U.stderr in
  (* Make sure fd 1 and 2 have different inodes at the point that we force them. *)
  let pipe_r, pipe_w = U.pipe () in
  U.dup2 ~src:pipe_w ~dst:U.stderr ();
  U.close pipe_r;
  U.close pipe_w;
  let stdout = Lazy.force stdout in
  let stderr = Lazy.force stderr in
  U.dup2 ~src:saved_stderr ~dst:U.stderr ();
  U.close saved_stderr;
  print_s [%message (phys_equal stdout stderr : bool)];
  [%expect {| ("phys_equal stdout stderr" true) |}]
;;

let behave_nicely_in_pipeline ?writers () =
  let writers =
    match writers with
    | Some z -> z
    | None -> List.map [ stdout; stderr ] ~f:force
  in
  List.iter writers ~f:(fun writer ->
    set_buffer_age_limit writer `Unlimited;
    set_raise_when_consumer_leaves writer false;
    don't_wait_for
      (let%map () = consumer_left writer in
       Shutdown.shutdown 0))
;;

let with_file_atomic ?temp_file ?perm ?fsync:(do_fsync = false) ?time_source file ~f =
  let%bind current_file_permissions =
    match%map Monitor.try_with (fun () -> Unix.stat file) with
    | Ok stats -> Some stats.perm
    | Error _ -> None
  in
  let initial_permissions =
    match perm with
    | Some p -> p
    | None ->
      (match current_file_permissions with
       | None -> 0o666
       | Some p -> p)
  in
  let%bind temp_file, fd =
    let temp_file = Option.value temp_file ~default:file in
    let%map temp_file, fd =
      let dir = Filename.dirname temp_file in
      let prefix = Filename.basename temp_file in
      In_thread.run (fun () ->
        Core.Filename.open_temp_file_fd ~perm:initial_permissions ~in_dir:dir prefix "")
    in
    temp_file, Fd.create File fd (Info.of_string temp_file)
  in
  let t = create ?time_source fd in
  let%bind result =
    with_close t ~f:(fun () ->
      let%bind result = f t in
      if is_closed t
      then
        raise_s
          [%message "Writer.with_file_atomic: writer closed by [f]" ~_:(file : string)];
      let%bind () =
        match current_file_permissions with
        | None ->
          (* We don't need to change the permissions here.
             The [initial_permissions] (with umask applied by the OS) should be good. *)
          return ()
        | Some _ ->
          (* We are overwriting permissions here to undo the umask that was applied
             by [openfile]. This is, perhaps, unreasonable, but it preserves the previous
             behavior. *)
          Unix.fchmod fd ~perm:initial_permissions
      in
      let%map () = if do_fsync then fsync t else return () in
      result)
  in
  match%bind Monitor.try_with (fun () -> Unix.rename ~src:temp_file ~dst:file) with
  | Ok () -> return result
  | Error exn ->
    let fail v sexp_of_v =
      raise_s
        [%message
          "Writer.with_file_atomic could not create file" (file : string) ~_:(v : v)]
    in
    (match%map Monitor.try_with (fun () -> Unix.unlink temp_file) with
     | Ok () -> fail exn [%sexp_of: exn]
     | Error exn2 ->
       fail (exn, `Cleanup_failed exn2) [%sexp_of: exn * [ `Cleanup_failed of exn ]])
;;

let save ?temp_file ?perm ?fsync file ~contents =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    write t contents;
    return ())
;;

let save_lines ?temp_file ?perm ?fsync file lines =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    List.iter lines ~f:(fun line ->
      write t line;
      newline t);
    return ())
;;

let save_sexp ?temp_file ?perm ?fsync ?(hum = true) file sexp =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    write_sexp_internal t sexp ~hum ~terminate_with:Newline;
    return ())
;;

let save_sexps ?temp_file ?perm ?fsync ?(hum = true) file sexps =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    List.iter sexps ~f:(fun sexp ->
      write_sexp_internal t sexp ~hum ~terminate_with:Newline);
    return ())
;;

let save_bin_prot ?temp_file ?perm ?fsync file bin_writer a =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    write_bin_prot t bin_writer a;
    return ())
;;

let with_flushed_at_close t ~flushed ~f =
  let producers_to_flush_at_close_elt = Bag.add t.producers_to_flush_at_close flushed in
  Monitor.protect f ~finally:(fun () ->
    Bag.remove t.producers_to_flush_at_close producers_to_flush_at_close_elt;
    return ())
;;

let make_transfer ?(stop = Deferred.never ()) ?max_num_values_per_read t pipe_r write_f =
  let consumer =
    Pipe.add_consumer pipe_r ~downstream_flushed:(fun () ->
      let%map () = flushed t in
      `Ok)
  in
  let end_of_pipe_r = Ivar.create () in
  (* The only reason we can't use [Pipe.iter] is because it doesn't accept
     [?max_num_values_per_read]. *)
  let rec iter () =
    if Ivar.is_full t.consumer_left || (not (can_write t)) || Deferred.is_determined stop
    then
      (* The [choose] in [doit] will become determined and [doit] will do the right
         thing. *)
      ()
    else (
      let read_result =
        match max_num_values_per_read with
        | None -> Pipe.read_now' pipe_r ~consumer
        | Some max_queue_length -> Pipe.read_now' pipe_r ~consumer ~max_queue_length
      in
      match read_result with
      | `Eof -> Ivar.fill end_of_pipe_r ()
      | `Nothing_available -> Pipe.values_available pipe_r >>> fun _ -> iter ()
      | `Ok q ->
        write_f q ~cont:(fun () ->
          Pipe.Consumer.values_sent_downstream consumer;
          flushed t >>> iter))
  in
  let doit () =
    (* Concurrecy between [iter] and [choose] is essential.  Even if [iter] gets blocked,
       for example on [flushed], the result of [doit] can still be determined by [choice]s
       other than [end_of_pipe_r]. *)
    iter ();
    match%map
      choose
        [ choice (Ivar.read end_of_pipe_r) (fun () -> `End_of_pipe_r)
        ; choice stop (fun () -> `Stop)
        ; choice (close_finished t) (fun () -> `Writer_closed)
        ; choice (consumer_left t) (fun () -> `Consumer_left)
        ]
    with
    | `End_of_pipe_r | `Stop -> ()
    | `Writer_closed | `Consumer_left -> Pipe.close_read pipe_r
  in
  with_flushed_at_close t ~f:doit ~flushed:(fun () ->
    Deferred.ignore_m (Pipe.upstream_flushed pipe_r))
;;

let transfer ?stop ?max_num_values_per_read t pipe_r write_f =
  make_transfer ?stop ?max_num_values_per_read t pipe_r (fun q ~cont ->
    Queue.iter q ~f:write_f;
    cont ())
;;

let transfer' ?stop ?max_num_values_per_read t pipe_r write_f =
  make_transfer ?stop ?max_num_values_per_read t pipe_r (fun q ~cont ->
    write_f q >>> cont)
;;

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  don't_wait_for (transfer t pipe_r (fun s -> write t s));
  pipe_w
;;
