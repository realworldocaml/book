open Core
open Import
module Kernel_transport = Rpc_kernel.Transport
module Header = Kernel_transport.Header
module Handler_result = Kernel_transport.Handler_result
module Send_result = Kernel_transport.Send_result

external writev2
  :  Core.Unix.File_descr.t
  -> buf1:Bigstring.t
  -> pos1:int
  -> len1:int
  -> buf2:Bigstring.t
  -> pos2:int
  -> len2:int
  -> Unix.Syscall_result.Int.t
  = "async_extra_rpc_writev2_byte" "async_extra_rpc_writev2"
[@@noalloc]

module Config = struct
  (* Same as the default value of [buffer_age_limit] for [Async_unix.Writer] *)
  let default_write_timeout = Time_ns.Span.of_min 2.

  (* No maximum *)
  let default_max_message_size = Int.max_value
  let default_max_buffer_size = Int.max_value

  (* In general we'll send 1 message per job, if we send 2 there is a good chance we are
     sending a batch.

     Default should actually be 1, but there was a bug that made it 2 in practice, so we
     keep 2 as a default. *)
  let default_start_batching_after_num_messages = 2

  (* Arbitrary choices. *)
  let default_initial_buffer_size = 64 * 1024
  let default_buffering_threshold_in_bytes = 32 * 1024

  type t =
    { max_message_size : int [@default default_max_message_size]
    ; initial_buffer_size : int [@default default_initial_buffer_size]
    ; max_buffer_size : int [@default default_max_buffer_size]
    ; write_timeout : Time_ns.Span.t [@default default_write_timeout]
    ; buffering_threshold_in_bytes : int [@default default_buffering_threshold_in_bytes]
    ; start_batching_after_num_messages : int
                                          [@default default_start_batching_after_num_messages]
    }
  [@@deriving sexp]

  let validate t =
    if t.initial_buffer_size <= 0
    || t.max_message_size <= 0
    || t.initial_buffer_size > t.max_buffer_size
    || t.max_message_size > t.max_buffer_size
    || t.buffering_threshold_in_bytes < 0
    || t.start_batching_after_num_messages < 0
    || Time_ns.Span.( <= ) t.write_timeout Time_ns.Span.zero
    then
      failwiths
        ~here:[%here]
        "Rpc_transport_low_latency.Config.validate: invalid config"
        t
        sexp_of_t;
    t
  ;;

  let t_of_sexp sexp = t_of_sexp sexp |> validate

  let create
        ?(max_message_size = default_max_message_size)
        ?(initial_buffer_size = default_initial_buffer_size)
        ?(max_buffer_size = default_max_buffer_size)
        ?(write_timeout = default_write_timeout)
        ?(buffering_threshold_in_bytes = default_buffering_threshold_in_bytes)
        ?(start_batching_after_num_messages = default_start_batching_after_num_messages)
        ()
    =
    validate
      { max_message_size
      ; initial_buffer_size
      ; max_buffer_size
      ; write_timeout
      ; buffering_threshold_in_bytes
      ; start_batching_after_num_messages
      }
  ;;

  let default = create ()

  let message_size_ok t ~payload_len =
    payload_len >= 0 && payload_len <= t.max_message_size
  ;;

  let check_message_size t ~payload_len =
    if not (message_size_ok t ~payload_len)
    then
      raise_s
        [%sexp
          "Rpc_transport_low_latency: message too small or too big"
        , { message_size = (payload_len : int); config = (t : t) }]
  ;;

  let grow_buffer t buf ~new_size_request =
    assert (new_size_request > Bigstring.length buf);
    if new_size_request > t.max_buffer_size
    then
      raise_s
        [%sexp
          "Rpc_transport_low_latency: cannot grow buffer"
        , { new_size_request : int; config = (t : t) }];
    let len = Int.min t.max_buffer_size (Int.ceil_pow2 new_size_request) in
    Bigstring.unsafe_destroy_and_resize buf ~len
  ;;
end

let set_nonblocking fd = Fd.with_file_descr_exn fd ignore ~nonblocking:true

module Reader_internal = struct
  type t =
    { fd : Fd.t
    ; config : Config.t
    ; mutable reading : bool
    ; mutable closed : bool
    ; close_finished : unit Ivar.t
    ; mutable buf : (Bigstring.t[@sexp.opaque])
    ; mutable pos : int (* Start of unconsumed data. *)
    ; mutable max : int (* End   of unconsumed data. *)
    }
  [@@deriving sexp_of, fields]

  let create fd config =
    set_nonblocking fd;
    { fd
    ; config
    ; reading = false
    ; closed = false
    ; close_finished = Ivar.create ()
    ; buf = Bigstring.create config.initial_buffer_size
    ; pos = 0
    ; max = 0
    }
  ;;

  let is_closed t = t.closed
  let close_finished t = Ivar.read t.close_finished

  (* Shift remaining unconsumed data to the beginning of the buffer *)
  let shift_unconsumed t =
    if t.pos > 0
    then (
      let len = t.max - t.pos in
      if len > 0 then Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos ~dst_pos:0 ~len;
      t.pos <- 0;
      t.max <- len)
  ;;

  let refill t =
    shift_unconsumed t;
    let result =
      Bigstring_unix.read_assume_fd_is_nonblocking
        (Fd.file_descr_exn t.fd)
        t.buf
        ~pos:t.max
        ~len:(Bigstring.length t.buf - t.max)
    in
    if Unix.Syscall_result.Int.is_ok result
    then (
      match Unix.Syscall_result.Int.ok_exn result with
      | 0 -> `Eof
      | n ->
        assert (n > 0);
        t.max <- t.max + n;
        `Read_some)
    else (
      match Unix.Syscall_result.Int.error_exn result with
      | EAGAIN | EWOULDBLOCK | EINTR -> `Nothing_available
      | EPIPE
      | ECONNRESET
      | EHOSTUNREACH
      | ENETDOWN
      | ENETRESET
      | ENETUNREACH
      | ETIMEDOUT -> `Eof
      | error -> raise (Unix.Unix_error (error, "read", "")))
  ;;

  (* To avoid allocating options in a relatively safe way. *)
  module Message_len : sig
    type t = private int

    val none : t
    val is_some : t -> bool
    val create_exn : int -> t

    (* fails on negative ints *)

    val value_exn : t -> int
  end = struct
    type t = int

    let none = -1
    let is_some t = t >= 0

    let create_exn n =
      if n < 0 then failwithf "Message_len.create_exn of negative int: %d" n () else n
    ;;

    let value_exn t = if t < 0 then failwith "Message_len.value_exn of None" else t
  end

  (* If one full message is available, returns its length (not including the
     header). Returns [Message_len.none] otherwise. *)
  let get_payload_length_of_next_available_message t =
    let pos = t.pos in
    let available = t.max - pos in
    if available >= Header.length
    then (
      let payload_len = Header.unsafe_get_payload_length t.buf ~pos in
      let total_len = payload_len + Header.length in
      Config.check_message_size t.config ~payload_len;
      if total_len <= available
      then Message_len.create_exn payload_len
      else (
        if total_len > Bigstring.length t.buf
        then t.buf <- Config.grow_buffer t.config t.buf ~new_size_request:total_len;
        Message_len.none))
    else Message_len.none
  ;;

  module Dispatcher = struct
    (* This module does a [Fd.every_ready_to] and takes care of exiting it when the
       callback returns [Wait _]. *)

    type 'a state =
      | Running
      | Stopped of 'a stop_reason

    and 'a stop_reason =
      | Handler_raised
      | Eof_reached
      (* Last handler call that wasn't determined immediately *)
      | Waiting_for_handler of unit Deferred.t
      | Stopped_by_user of 'a

    type nonrec 'a t =
      { reader : t
      ; on_message : Bigstring.t -> pos:int -> len:int -> 'a Handler_result.t
      ; on_end_of_batch : unit -> unit
      ; interrupt : unit Ivar.t (* To stop watching the file descriptor *)
      ; mutable state : 'a state
      }

    let is_running t =
      match t.state with
      | Running -> true
      | Stopped _ -> false
    ;;

    let interrupt t reason =
      assert (is_running t);
      t.state <- Stopped reason;
      Ivar.fill t.interrupt ()
    ;;

    let can_process_message t = (not t.reader.closed) && is_running t

    let rec process_received_messages t =
      if can_process_message t
      then (
        let len = get_payload_length_of_next_available_message t.reader in
        if Message_len.is_some len
        then (
          let len = Message_len.value_exn len in
          let start = t.reader.pos + Header.length in
          t.reader.pos <- start + len;
          match t.on_message t.reader.buf ~pos:start ~len with
          | Stop x -> interrupt t (Stopped_by_user x)
          | Continue -> process_received_messages t
          | Wait d ->
            if Deferred.is_determined d
            then process_received_messages t
            else interrupt t (Waiting_for_handler d))
        else t.on_end_of_batch ())
    ;;

    let process_incoming t =
      if can_process_message t
      then (
        match refill t.reader with
        | `Eof -> interrupt t Eof_reached
        | `Nothing_available -> ()
        | `Read_some -> process_received_messages t)
    ;;

    (* We want to stop reading/dispatching as soon as we get an error *)
    let stop_watching_on_error t ~monitor =
      let parent = Monitor.current () in
      Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
        if is_running t then interrupt t Handler_raised;
        (* Let the monitor in effect when [dispatch] was called deal with the error. *)
        Monitor.send_exn parent exn)
    ;;

    let rec run reader ~on_message ~on_end_of_batch =
      let t =
        { reader
        ; interrupt = Ivar.create ()
        ; state = Running
        ; on_message
        ; on_end_of_batch
        }
      in
      let monitor =
        Monitor.create
          ~here:[%here]
          ~name:"Rpc_transport_low_latency.Reader_internal.Dispatcher.run"
          ()
      in
      stop_watching_on_error t ~monitor;
      Scheduler.within' ~monitor (fun () ->
        (* Process messages currently in the buffer. *)
        (* This will fill [t.interrupt] if [on_message] returns [Wait _]. However, we
           expect [on_message] to almost never return [Wait _] with this transport, since
           even the "non-copying" writes return [Deferred.unit]. *)
        process_received_messages t;
        let interrupt =
          Deferred.any [ Ivar.read t.interrupt; close_finished t.reader ]
        in
        Fd.interruptible_every_ready_to ~interrupt t.reader.fd `Read process_incoming t)
      >>= function
      | `Bad_fd | `Unsupported ->
        failwith
          "Rpc_transport_low_latency.Reader.read_forever: file descriptor doesn't \
           support watching"
      | `Closed | `Interrupted ->
        (match t.state with
         | Running ->
           assert (Fd.is_closed t.reader.fd || t.reader.closed);
           return (Error `Closed)
         | Stopped (Stopped_by_user x) -> return (Ok x)
         | Stopped Handler_raised ->
           (* The exception has been propagated, we only arrive here because we forced the
              [every_ready_to] to be interrupted. *)
           Deferred.never ()
         | Stopped Eof_reached -> return (Error `Eof)
         | Stopped (Waiting_for_handler d) ->
           d
           >>= fun () ->
           if reader.closed
           then return (Error `Closed)
           else run reader ~on_message ~on_end_of_batch)
    ;;
  end

  let read_forever t ~on_message ~on_end_of_batch =
    if t.closed then failwith "Rpc_transport_low_latency.Reader: reader closed";
    if t.reading then failwith "Rpc_transport_low_latency.Reader: already reading";
    t.reading <- true;
    Monitor.protect
      ~here:[%here]
      ~name:"Rpc_transport_low_latency.Reader_internal.read_forever"
      ~finally:(fun () ->
        t.reading <- false;
        Deferred.unit)
      (fun () -> Dispatcher.run t ~on_message ~on_end_of_batch)
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      Fd.close t.fd >>> fun () -> Ivar.fill t.close_finished ());
    close_finished t
  ;;
end

module Writer_internal = struct
  type flush =
    { pos : Int63.t
    ; ivar : unit Ivar.t
    }
  [@@deriving sexp_of]

  let get_job_number () = Scheduler.num_jobs_run ()

  module Connection_state : sig
    type t [@@deriving sexp_of]

    val create : unit -> t
    val is_currently_accepting_writes : t -> bool
    val is_able_to_send_data : t -> bool
    val start_close : t -> unit
    val finish_close : t -> fd_closed:unit Deferred.t -> unit
    val connection_lost : t -> unit
    val close_finished : t -> unit Deferred.t
    val stopped : t -> unit Deferred.t
  end = struct
    type t =
      { close_started : unit Ivar.t
      ; close_finished : unit Ivar.t
      ; connection_lost : unit Ivar.t
      }
    [@@deriving sexp_of]

    let start_close t = Ivar.fill_if_empty t.close_started ()

    let finish_close t ~fd_closed =
      start_close t;
      Ivar.fill_if_empty t.connection_lost ();
      upon fd_closed (Ivar.fill_if_empty t.close_finished)
    ;;

    let close_finished t = Ivar.read t.close_finished
    let is_currently_accepting_writes t = Ivar.is_empty t.close_started
    let is_able_to_send_data t = Ivar.is_empty t.connection_lost
    let connection_lost t = Ivar.fill_if_empty t.connection_lost ()

    let stopped t =
      Deferred.any [ Ivar.read t.connection_lost; Ivar.read t.close_started ]
    ;;

    let create () =
      { close_started = Ivar.create ()
      ; close_finished = Ivar.create ()
      ; connection_lost = Ivar.create ()
      }
    ;;
  end

  type t =
    { fd : Fd.t
    ; config : Config.t
    ; connection_state : Connection_state.t
    ; mutable writing : bool
    ; mutable buf : (Bigstring.t[@sexp.opaque])
    ; mutable pos : int
    ; mutable bytes_written : Int63.t
    ; monitor : Monitor.t
    ; flushes : flush Queue.t (* the job number of the job when we last sent data *)
    ; mutable last_send_job : int
    ; mutable sends_in_this_job : int
    }
  [@@deriving sexp_of, fields]

  let create fd config =
    set_nonblocking fd;
    { fd
    ; config
    ; writing = false
    ; connection_state = Connection_state.create ()
    ; buf = Bigstring.create config.initial_buffer_size
    ; pos = 0
    ; bytes_written = Int63.zero
    ; monitor = Monitor.create ()
    ; flushes = Queue.create ()
    ; last_send_job = 0
    ; sends_in_this_job = 0
    }
  ;;

  let is_closed t =
    not (Connection_state.is_currently_accepting_writes t.connection_state)
  ;;

  let close_finished t = Connection_state.close_finished t.connection_state
  let bytes_to_write t = t.pos
  let stopped t = Connection_state.stopped t.connection_state

  let flushed t =
    if t.pos = 0
    then Deferred.unit
    else if not (Connection_state.is_able_to_send_data t.connection_state)
    then Deferred.never ()
    else (
      let flush =
        { pos = Int63.( + ) t.bytes_written (Int63.of_int t.pos); ivar = Ivar.create () }
      in
      Queue.enqueue t.flushes flush;
      Ivar.read flush.ivar)
  ;;

  let ready_to_write = flushed

  let dequeue_flushes t =
    while
      (not (Queue.is_empty t.flushes))
      && Int63.( <= ) (Queue.peek_exn t.flushes).pos t.bytes_written
    do
      Ivar.fill (Queue.dequeue_exn t.flushes).ivar ()
    done
  ;;

  (* Discard the [n] first bytes of [t.buf] *)
  let discard t n =
    assert (n >= 0 && n <= t.pos);
    let remaining = t.pos - n in
    if remaining > 0
    then Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:n ~dst_pos:0 ~len:remaining;
    t.pos <- remaining;
    t.bytes_written <- Int63.( + ) t.bytes_written (Int63.of_int n);
    dequeue_flushes t
  ;;

  module Error_kind = struct
    type t =
      | Write_blocked
      | Connection_lost
      | Other_error
  end

  let handle_error t (error : Unix.Error.t) : Error_kind.t =
    match error with
    | EAGAIN | EWOULDBLOCK | EINTR -> Write_blocked
    | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
      ->
      Connection_state.connection_lost t.connection_state;
      Connection_lost
    | _ -> Other_error
  ;;

  module Single_write_result = struct
    type t =
      | Continue
      | Stop
  end

  let single_write t : Single_write_result.t =
    match
      Bigstring_unix.write_assume_fd_is_nonblocking
        (Fd.file_descr_exn t.fd)
        t.buf
        ~pos:0
        ~len:t.pos
    with
    | n ->
      discard t n;
      Continue
    | exception (Unix.Unix_error (error, _, _) as exn) ->
      (match handle_error t error with
       | Write_blocked -> Continue
       | Connection_lost -> Stop
       | Other_error -> raise exn)
  ;;

  let finish_close t =
    let fd_closed = Fd.close t.fd in
    t.writing <- false;
    Connection_state.finish_close t.connection_state ~fd_closed
  ;;

  let rec write_everything t =
    match single_write t with
    | Stop -> finish_close t
    | Continue ->
      if t.pos = 0
      then (
        t.writing <- false;
        if is_closed t then finish_close t)
      else wait_and_write_everything t

  and wait_and_write_everything t =
    Clock_ns.with_timeout t.config.write_timeout (Fd.ready_to t.fd `Write)
    >>> fun result ->
    if not (Connection_state.is_able_to_send_data t.connection_state)
    then finish_close t
    else (
      match result with
      | `Result `Ready -> write_everything t
      | `Timeout ->
        Log.Global.sexp
          ~level:`Error
          [%message
            "Rpc_transport_low_latency.Writer timed out waiting to write on file \
             descriptor. Closing the writer."
              ~timeout:(t.config.write_timeout : Time_ns.Span.t)
              (t : t)];
        finish_close t
      | `Result ((`Bad_fd | `Closed) as result) ->
        raise_s
          [%sexp
            "Rpc_transport_low_latency.Writer: fd changed"
          , { t : t; ready_to_result = (result : [ `Bad_fd | `Closed ]) }])
  ;;

  let flush t =
    if (not t.writing) && t.pos > 0
    then (
      t.writing <- true;
      Scheduler.within ~monitor:t.monitor (fun () -> write_everything t))
  ;;

  let schedule_flush t =
    if (not t.writing) && t.pos > 0
    then (
      t.writing <- true;
      Scheduler.within ~monitor:t.monitor (fun () -> wait_and_write_everything t))
  ;;

  let ensure_at_least t ~needed =
    if Bigstring.length t.buf - t.pos < needed
    then (
      let new_size_request = t.pos + needed in
      t.buf <- Config.grow_buffer t.config t.buf ~new_size_request)
  ;;

  let copy_bytes t ~buf ~pos ~len =
    if len > 0
    then (
      ensure_at_least t ~needed:len;
      Bigstring.blit ~src:buf ~dst:t.buf ~src_pos:pos ~dst_pos:t.pos ~len;
      t.pos <- t.pos + len)
  ;;

  (* Write what's in the internal buffer + bytes denoted by [(buf, pos, len)] *)
  let unsafe_send_bytes t ~buf ~pos ~len =
    let result =
      writev2
        (Fd.file_descr_exn t.fd)
        ~buf1:t.buf
        ~pos1:0
        ~len1:t.pos
        ~buf2:buf
        ~pos2:pos
        ~len2:len
    in
    if Unix.Syscall_result.Int.is_ok result
    then (
      let n = Unix.Syscall_result.Int.ok_exn result in
      if n <= t.pos
      then (
        (* We wrote less than what's in the internal buffer, discard what was written and
           copy in the other buffer. *)
        discard t n;
        copy_bytes t ~buf ~pos ~len)
      else (
        let written_from_other_buf = n - t.pos in
        let remaining_in_other_buf = len - written_from_other_buf in
        discard t t.pos;
        if remaining_in_other_buf > 0
        then
          copy_bytes
            t
            ~buf
            ~pos:(pos + written_from_other_buf)
            ~len:remaining_in_other_buf))
    else (
      let error = Unix.Syscall_result.Int.error_exn result in
      match handle_error t error with
      | Write_blocked -> copy_bytes t ~buf ~pos ~len
      | Connection_lost -> ()
      | Other_error ->
        let syscall = if len = 0 then "write" else "writev" in
        Monitor.send_exn t.monitor (Unix.Unix_error (error, syscall, "")))
  ;;

  let slow_write_bin_prot_and_bigstring
        t
        (writer : _ Bin_prot.Type_class.writer)
        msg
        ~buf
        ~pos
        ~len
    : _ Send_result.t
    =
    let payload_len = writer.size msg + len in
    let total_len = Header.length + payload_len in
    if Config.message_size_ok t.config ~payload_len
    then (
      ensure_at_least t ~needed:total_len;
      Header.unsafe_set_payload_length t.buf ~pos:t.pos payload_len;
      let stop = writer.write t.buf ~pos:(t.pos + Header.length) msg in
      assert (stop + len = t.pos + total_len);
      Bigstring.blit ~src:buf ~dst:t.buf ~src_pos:pos ~dst_pos:stop ~len;
      t.pos <- stop + len;
      Sent ())
    else
      Message_too_big
        { size = payload_len; max_message_size = t.config.max_message_size }
  ;;

  let should_send_now t =
    let current_job = get_job_number () in
    if current_job = t.last_send_job
    then t.sends_in_this_job <- t.sends_in_this_job + 1
    else (
      t.last_send_job <- current_job;
      t.sends_in_this_job <- 1);
    t.pos >= t.config.buffering_threshold_in_bytes
    || t.sends_in_this_job <= t.config.start_batching_after_num_messages
  ;;

  let send_bin_prot_and_bigstring
        t
        (writer : _ Bin_prot.Type_class.writer)
        msg
        ~buf
        ~pos
        ~len
    : _ Send_result.t
    =
    if is_closed t
    then Closed
    else (
      Ordered_collection_common.check_pos_len_exn
        ~pos
        ~len
        ~total_length:(Bigstring.length buf);
      if Connection_state.is_able_to_send_data t.connection_state
      then (
        let send_now = should_send_now t in
        let result =
          if Bigstring.length t.buf - t.pos < Header.length
          then slow_write_bin_prot_and_bigstring t writer msg ~buf ~pos ~len
          else (
            match writer.write t.buf ~pos:(t.pos + Header.length) msg with
            | exception _ ->
              (* It's likely that the exception is due to a buffer overflow, so resize the
                 internal buffer and try again. Technically we could match on
                 [Bin_prot.Common.Buffer_short] only, however we can't easily enforce that
                 custom bin_write_xxx functions raise this particular exception and not
                 [Invalid_argument] or [Failure] for instance. *)
              slow_write_bin_prot_and_bigstring t writer msg ~buf ~pos ~len
            | stop ->
              let payload_len = stop - (t.pos + Header.length) + len in
              if Config.message_size_ok t.config ~payload_len
              then (
                Header.unsafe_set_payload_length t.buf ~pos:t.pos payload_len;
                t.pos <- stop;
                if send_now
                then (
                  let len =
                    if len < 128
                    then (
                      copy_bytes t ~buf ~pos ~len;
                      0)
                    else len
                  in
                  unsafe_send_bytes t ~buf ~pos ~len)
                else copy_bytes t ~buf ~pos ~len;
                Sent ())
              else
                Message_too_big
                  { size = payload_len; max_message_size = t.config.max_message_size })
        in
        if send_now then flush t else schedule_flush t;
        result)
      else Sent ())
  ;;

  let sent_deferred_unit = Send_result.Sent Deferred.unit

  let send_bin_prot_and_bigstring_non_copying t writer msg ~buf ~pos ~len =
    match send_bin_prot_and_bigstring t writer msg ~buf ~pos ~len with
    | Sent () -> sent_deferred_unit
    | (Closed | Message_too_big _) as r -> r
  ;;

  let dummy_buf = Bigstring.create 0

  let send_bin_prot t writer msg =
    send_bin_prot_and_bigstring t writer msg ~buf:dummy_buf ~pos:0 ~len:0
  ;;

  let close t =
    if not (is_closed t)
    then (
      Connection_state.start_close t.connection_state;
      flush t;
      if not t.writing then finish_close t);
    close_finished t
  ;;
end

let make_create f ?(config = Config.default) ~max_message_size fd =
  let max_message_size = min config.max_message_size max_message_size in
  let config = Config.validate { config with max_message_size } in
  f fd config
;;

module Reader = struct
  include Kernel_transport.Reader

  let create_internal fd config =
    pack (module Reader_internal) (Reader_internal.create fd config)
  ;;

  let create = make_create create_internal
end

module Writer = struct
  include Kernel_transport.Writer

  let create_internal fd config =
    pack (module Writer_internal) (Writer_internal.create fd config)
  ;;

  let create = make_create create_internal
end

type t = Kernel_transport.t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]

let close = Kernel_transport.close

let create_internal fd config =
  { reader = Reader.create_internal fd config
  ; writer = Writer.create_internal fd config
  }
;;

let create = make_create create_internal
