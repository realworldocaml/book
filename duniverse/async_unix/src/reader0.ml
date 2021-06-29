open Core
open Import
module Scheduler = Raw_scheduler
module Unix = Unix_syscalls
module Id = Unique_id.Int63 ()

module Read_result = struct
  module Z = struct
    type 'a t =
      [ `Ok of 'a
      | `Eof
      ]
    [@@deriving bin_io, sexp]

    let bind a ~f =
      match a with
      | `Ok a -> f a
      | `Eof -> `Eof
    ;;

    let map a ~f =
      match a with
      | `Ok a -> `Ok (f a)
      | `Eof -> `Eof
    ;;

    let map = `Custom map
    let return a = `Ok a
  end

  include Z
  include Monad.Make (Z)
end

(* We put everything in module [Internal] and then expose just the functions we want
   later.  This reminds us to wrap functions with [do_read], which we do to prevent
   multiple simultaneous active uses of a reader. *)
module Internal = struct
  module State = struct
    type t =
      [ `Not_in_use
      | `In_use
      | `Closed
      ]
    [@@deriving sexp]
  end

  module Open_flags = Unix.Open_flags

  type open_flags =
    [ `Already_closed
    | `Ok of Open_flags.t
    | `Error of exn
    ]
  [@@deriving sexp_of]

  type t =
    { fd : Fd.t
    ; id : Id.t
    ; (* [buf] holds data read by the reader from the OS, but not yet read by user code.
         When [t] is closed, [buf] is set to the empty buffer.  So, we must make sure in
         any code that accesses [buf] that [t] has not been closed.  In particular, after
         any deferred operation, we must check whether [t] has been closed while we were
         waiting. *)
      mutable buf : Bigstring.t
    ; (* [close_may_destroy_buf] indicates whether a call to [close] can immediately
         destroy [buf].  [close_may_destroy_buf] is usually [`Yes], except when we're in
         the middle of a system call in another thread that refers to [buf], in which case
         it is [`Not_now] and [close] can't destroy [buf], and we must wait until that
         system call finishes before doing so.

         [`Not_ever] is used for [read_one_chunk_at_a_time], which exposes[buf]
         to client code, which may in turn hold on to it (e.g. via
         [Bigstring.sub_shared]), and thus it is not safe to ever destroy it. *)
      mutable close_may_destroy_buf : [ `Yes | `Not_now | `Not_ever ]
    ; (* [pos] is the first byte of data in [buf] to b be read by user code. *)
      mutable pos : int
    ; (* [available] is how many bytes in [buf] are available to be read by user code. *)
      mutable available : int
    ; (* [`Closed] means that [close t] has been called.  [`In_use] means there is some
         user call extant that is waiting for data from the reader. *)
      mutable state : State.t
    ; close_finished : unit Ivar.t
    ; mutable last_read_time : Time.t
    ; (* [open_flags] is the open-file-descriptor bits of [fd].  It is created when [t] is
         created, and starts a deferred computation that calls [Unix.fcntl_getfl].
         [open_flags] is used to report an error when [fd] is not readable.  [Fd] treats
         the call to [fcntl_getfl] as an active system call, which prevents [Unix.close
         fd] from completing until [fcntl_getfl] finishes.  This prevents a
         file-descriptor or thread leak even though client code doesn't explicitly wait on
         [open_flags]. *)
      open_flags : open_flags Deferred.t
    }
  [@@deriving fields]

  let sexp_of_t t = [%sexp (t.fd : Fd.t_hum)]

  type t_internals = t

  let sexp_of_t_internals
        { available
        ; buf = _
        ; close_finished
        ; close_may_destroy_buf
        ; id
        ; fd
        ; last_read_time
        ; open_flags
        ; pos
        ; state
        }
    =
    let unless_testing x = Option.some_if (not am_running_inline_test) x in
    [%sexp
      { id = (id |> unless_testing : (Id.t option[@sexp.option]))
      ; state : State.t
      ; available : int
      ; pos : int
      ; open_flags =
          (open_flags |> unless_testing : (open_flags Deferred.t option[@sexp.option]))
      ; last_read_time =
          (last_read_time |> unless_testing : (Time.t option[@sexp.option]))
      ; close_may_destroy_buf : [ `Yes | `Not_now | `Not_ever ]
      ; close_finished : unit Ivar.t
      ; fd = (fd |> unless_testing : (Fd.t option[@sexp.option]))
      }]
  ;;

  let io_stats = Io_stats.create ()

  let invariant t : unit =
    assert (0 <= t.pos);
    assert (0 <= t.available);
    assert (t.pos + t.available <= Bigstring.length t.buf)
  ;;

  let create ?buf_len fd =
    let buf_len =
      match buf_len with
      | None ->
        (match Fd.kind fd with
         | Char | File -> 32 * 1024
         | Fifo | Socket _ -> 128 * 1024)
      | Some buf_len ->
        if buf_len > 0
        then buf_len
        else
          raise_s
            [%message
              "Reader.create got non positive buf_len" (buf_len : int) (fd : Fd.t)]
    in
    let open_flags =
      Fd.syscall_in_thread fd ~name:"fcntl_getfl" (fun file_descr ->
        Core.Unix.fcntl_getfl file_descr)
    in
    { fd
    ; id = Id.create ()
    ; buf = Bigstring.create buf_len
    ; close_may_destroy_buf = `Yes
    ; pos = 0
    ; available = 0
    ; state = `Not_in_use
    ; close_finished = Ivar.create ()
    ; last_read_time = Scheduler.cycle_start ()
    ; open_flags
    }
  ;;

  let of_in_channel ic kind = create (Fd.of_in_channel ic kind)

  let open_file ?buf_len file =
    let%map fd = Unix.openfile file ~mode:[ `Rdonly ] ~perm:0o000 in
    create fd ?buf_len
  ;;

  let stdin = lazy (create (Fd.stdin ()))
  let close_finished t = Ivar.read t.close_finished

  let is_closed t =
    match t.state with
    | `Closed -> true
    | `Not_in_use | `In_use -> false
  ;;

  let empty_buf = Bigstring.create 0

  let destroy t =
    (* Calling [unsafe_destroy] on [t]'s bigstrings rather than waiting for finalizers to
       free them makes their space immediately available for reuse by C's malloc. *)
    Bigstring.unsafe_destroy t.buf;
    t.buf <- empty_buf
  ;;

  let close t =
    (match t.state with
     | `Closed -> ()
     | `Not_in_use | `In_use ->
       t.state <- `Closed;
       upon (Unix.close t.fd) (fun () -> Ivar.fill t.close_finished ());
       t.pos <- 0;
       t.available <- 0;
       (match t.close_may_destroy_buf with
        | `Yes -> destroy t
        | `Not_now | `Not_ever -> ()));
    close_finished t
  ;;

  let with_close t ~f = Monitor.protect f ~finally:(fun () -> close t)

  let with_reader_exclusive t f =
    let%bind () = Unix.lockf t.fd Shared in
    Monitor.protect f ~finally:(fun () ->
      if not (Fd.is_closed t.fd) then Unix.unlockf t.fd;
      return ())
  ;;

  let with_file ?buf_len ?(exclusive = false) file ~f =
    let%bind t = open_file ?buf_len file in
    with_close t ~f:(fun () ->
      if exclusive then with_reader_exclusive t (fun () -> f t) else f t)
  ;;

  (* [get_data t] attempts to read data into [t.buf].  If the read gets data, [get_data]
     returns [`Ok], otherwise it returns [`Eof]. *)
  let get_data t : [ `Ok | `Eof ] Deferred.t =
    Deferred.create (fun result ->
      t.open_flags
      >>> fun open_flags ->
      let eof () = Ivar.fill result `Eof in
      match t.state, open_flags with
      | `Not_in_use, _ -> assert false
      | `Closed, _ | _, `Already_closed -> eof ()
      | `In_use, ((`Error _ | `Ok _) as open_flags) ->
        let can_read_fd =
          match open_flags with
          | `Error _ -> false
          | `Ok open_flags -> Unix.Open_flags.can_read open_flags
        in
        if not can_read_fd
        then
          raise_s
            [%message
              "not allowed to read due to file-descriptor flags"
                (open_flags : open_flags)
                ~reader:(t : t)];
        let ebadf () =
          (* If the file descriptor has been closed, we will get EBADF from a syscall.
             If someone closed the [Fd.t] using [Fd.close], then that is fine.  But if the
             underlying file descriptor got closed in some other way, then something is
             likely wrong, so we raise. *)
          raise_s
            [%message "reader file descriptor was unexpectedly closed" ~reader:(t : t)]
        in
        let finish res handle =
          match res with
          | `Already_closed -> eof ()
          | `Error exn ->
            (match exn with
             | Bigstring_unix.IOError (0, End_of_file)
             | Unix.Unix_error
                 ( ( ECONNRESET
                   | EHOSTUNREACH
                   | ENETDOWN
                   | ENETRESET
                   | ENETUNREACH
                   (* When using OpenOnload, read() can return EPIPE if a TCP connection
                      is established and then immediately closed. *)
                   | EPIPE
                   | ETIMEDOUT )
                 , _
                 , _ ) -> eof ()
             | Unix.Unix_error (EBADF, _, _) -> ebadf ()
             | _ -> handle exn)
          | `Ok (bytes_read, read_time) ->
            Io_stats.update
              io_stats
              ~kind:(Fd.kind t.fd)
              ~bytes:(Int63.of_int bytes_read);
            if bytes_read = 0
            then eof ()
            else (
              t.pos <- 0;
              t.available <- t.available + bytes_read;
              t.last_read_time <- read_time;
              Ivar.fill result `Ok)
        in
        let buf = t.buf in
        if t.available > 0 && t.pos > 0
        then (
          Bigstring.blit ~src:buf ~src_pos:t.pos ~dst:buf ~dst_pos:0 ~len:t.available;
          t.pos <- 0);
        let pos = t.available in
        let len = Bigstring.length buf - pos in
        if not (Fd.supports_nonblock t.fd)
        then (
          (match t.close_may_destroy_buf with
           | `Yes -> t.close_may_destroy_buf <- `Not_now
           | `Not_now | `Not_ever -> ());
          Fd.syscall_in_thread t.fd ~name:"read" (fun file_descr ->
            let res = Bigstring_unix.read file_descr buf ~pos ~len in
            res, Time.now ())
          >>> fun res ->
          (match t.close_may_destroy_buf with
           | `Not_now -> t.close_may_destroy_buf <- `Yes
           | `Yes | `Not_ever -> ());
          match t.state with
          | `Not_in_use -> assert false
          | `In_use -> finish res raise
          | `Closed ->
            (* If we're here, somebody [close]d the reader while we were making the system
               call.  [close] couldn't [destroy], so we need to. *)
            destroy t;
            eof ())
        else (
          let rec loop () =
            (* Force the async cycle to end between reads, allowing others to run. *)
            Fd.ready_to t.fd `Read
            >>> function
            | `Bad_fd -> ebadf ()
            | `Closed -> eof ()
            | `Ready ->
              (* There is a race between the [ready_to] becoming determined and someone
                 [close]ing [t].  It is possible to get [`Ready] and then by the time we
                 get here, [t] is closed. *)
              (match t.state with
               | `Not_in_use -> assert false
               | `Closed -> eof ()
               | `In_use ->
                 finish
                   (Fd.syscall t.fd ~nonblocking:true (fun file_descr ->
                      let res =
                        Unix.Syscall_result.Int.ok_or_unix_error_exn
                          (Bigstring_unix.read_assume_fd_is_nonblocking
                             file_descr
                             buf
                             ~pos
                             ~len)
                          ~syscall_name:"read"
                      in
                      res, Scheduler.cycle_start ()))
                   (function
                     (* Since [t.fd] is ready, we should never see EWOULDBLOCK or EAGAIN.
                        But we don't trust the OS.  So, in case it does, we just try
                        again. *)
                     | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) -> loop ()
                     | exn -> raise exn))
          in
          loop ()))
  ;;

  let ensure_buf_len t ~at_least =
    let buf_len = Bigstring.length t.buf in
    if buf_len < at_least
    then (
      let new_buf = Bigstring.create (Int.max at_least (2 * Bigstring.length t.buf)) in
      if t.available > 0
      then
        Bigstring.blit ~src:t.buf ~src_pos:t.pos ~len:t.available ~dst:new_buf ~dst_pos:0;
      t.buf <- new_buf;
      t.pos <- 0);
    assert (Bigstring.length t.buf >= at_least)
  ;;

  (* [get_data_until] calls [get_data] to read into [t.buf] until [t.available >=
     available_at_least], or until it reaches EOF.  It returns [`Ok] if [t.available >=
     available_at_least], and [`Eof] if not. *)
  let get_data_until t ~available_at_least =
    if t.available >= available_at_least
    then return `Ok
    else (
      ensure_buf_len t ~at_least:available_at_least;
      if t.pos > 0
      then (
        Bigstring.blit ~src:t.buf ~src_pos:t.pos ~dst:t.buf ~dst_pos:0 ~len:t.available;
        t.pos <- 0);
      let rec loop () =
        let%bind result = get_data t in
        if t.available >= available_at_least
        then return `Ok
        else (
          match result with
          | `Eof -> return (`Eof t.available)
          | `Ok -> loop ())
      in
      loop ())
  ;;

  (* [with_nonempty_buffer t f] waits for [t.buf] to have data, and then returns [f `Ok].
     If no data can be read, then [with_nonempty_buffer] returns [f `Eof].
     [with_nonempty_buffer] must be called with [t.state] as [`Closed] or [`In_use].  It
     guarantees that if [f `Ok] is called, that [t.state = `In_use]. *)
  let with_nonempty_buffer (type a) t (f : [ `Ok | `Eof ] -> a) : a Deferred.t =
    match t.state with
    | `Not_in_use -> assert false
    | `Closed -> return (f `Eof)
    | `In_use ->
      if t.available > 0
      then return (f `Ok)
      else (
        let%map ok_or_eof = get_data t in
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> f `Eof
        | `In_use -> f ok_or_eof)
  ;;

  (* [with_nonempty_buffer' t f] is an optimized version of
     [don't_wait_for (with_nonempty_buffer t f)].

     With [force_refill = true], [with_nonempty_buffer'] will do a read, whether or not
     there is already data available in [t.buf]. *)
  let with_nonempty_buffer' ?(force_refill = false) t (f : [ `Ok | `Eof ] -> unit) : unit
    =
    match t.state with
    | `Not_in_use -> assert false
    | `Closed -> f `Eof
    | `In_use ->
      if (not force_refill) && t.available > 0
      then f `Ok
      else
        get_data t
        >>> fun ok_or_eof ->
        (match t.state with
         | `Not_in_use -> assert false
         | `Closed -> f `Eof
         | `In_use -> f ok_or_eof)
  ;;

  let consume t amount =
    assert (0 <= amount && amount <= t.available);
    t.pos <- t.pos + amount;
    t.available <- t.available - amount
  ;;

  type 'a handle_chunk_result =
    [ `Stop of 'a
    | `Stop_consumed of 'a * int
    | `Continue
    | `Consumed of int * [ `Need of int | `Need_unknown ]
    ]
  [@@deriving sexp_of]

  type 'a read_one_chunk_at_a_time_result =
    [ `Eof
    | `Stopped of 'a
    | `Eof_with_unconsumed_data of string
    ]
  [@@deriving sexp_of]

  type consumed = [ `Consumed of int * [ `Need of int | `Need_unknown ] ]
  [@@deriving sexp_of]

  let read_one_chunk_at_a_time t ~handle_chunk =
    t.close_may_destroy_buf <- `Not_ever;
    Deferred.create (fun final_result ->
      let rec loop ~force_refill =
        with_nonempty_buffer' t ~force_refill (function
          | `Eof ->
            let result =
              if t.available > 0
              then
                `Eof_with_unconsumed_data
                  (Bigstring.to_string t.buf ~pos:t.pos ~len:t.available)
              else `Eof
            in
            Ivar.fill final_result result
          | `Ok ->
            let len = t.available in
            let continue z =
              match t.state with
              | `Not_in_use -> assert false
              | `Closed -> Ivar.fill final_result `Eof
              | `In_use ->
                (match z with
                 | `Stop a ->
                   consume t len;
                   Ivar.fill final_result (`Stopped a)
                 | `Stop_consumed (a, consumed) ->
                   consume t consumed;
                   Ivar.fill final_result (`Stopped a)
                 | `Continue ->
                   consume t len;
                   loop ~force_refill:true
                 | `Consumed (consumed, need) as c ->
                   if consumed < 0
                   || consumed > len
                   ||
                   match need with
                   | `Need_unknown -> false
                   | `Need need -> need < 0 || consumed + need <= len
                   then
                     raise_s
                       [%message
                         "handle_chunk returned invalid `Consumed"
                           ~_:(c : consumed)
                           (len : int)
                           ~reader:(t : t)];
                   consume t consumed;
                   let buf_len = Bigstring.length t.buf in
                   let new_len =
                     match need with
                     | `Need_unknown ->
                       if t.available = buf_len
                       (* The buffer is full and the client doesn't know how much to
                          expect: double the buffer size. *)
                       then buf_len * 2
                       else buf_len
                     | `Need need ->
                       if need > buf_len then Int.max need (buf_len * 2) else buf_len
                   in
                   if new_len < 0
                   then
                     raise_s
                       [%message
                         "read_one_chunk_at_a_time got overflow in buffer len"
                           ~reader:(t : t_internals)];
                   (* Grow the internal buffer if needed. *)
                   if new_len > buf_len
                   then (
                     let new_buf = Bigstring.create new_len in
                     if t.available > 0
                     then
                       Bigstring.blit
                         ~src:t.buf
                         ~src_pos:t.pos
                         ~len:t.available
                         ~dst:new_buf
                         ~dst_pos:0;
                     t.buf <- new_buf;
                     t.pos <- 0);
                   loop ~force_refill:true)
            in
            let deferred = handle_chunk t.buf ~pos:t.pos ~len in
            (match Deferred.peek deferred with
             | None -> deferred >>> continue
             | Some result -> continue result))
      in
      loop ~force_refill:false)
  ;;

  type 'a handle_iobuf_result =
    [ `Stop of 'a
    | `Continue
    ]
  [@@deriving sexp_of]

  let read_one_iobuf_at_a_time t ~handle_chunk =
    let iobuf = Iobuf.of_bigstring t.buf in
    read_one_chunk_at_a_time t ~handle_chunk:(fun bstr ~pos ~len ->
      Iobuf.Expert.reinitialize_of_bigstring iobuf bstr ~pos ~len;
      let%map handle_result = handle_chunk iobuf in
      if Iobuf.is_empty iobuf (* [is_empty] implies all data was consumed *)
      then (handle_result :> _ handle_chunk_result)
      else (
        let consumed = len - Iobuf.length iobuf in
        match handle_result with
        | `Continue -> `Consumed (consumed, `Need_unknown)
        | `Stop a -> `Stop_consumed (a, consumed)))
  ;;

  module Read
      (S : Substring_intf.S) (Name : sig
                                val name : string
                              end) =
  struct
    let read_available t s =
      let len = Int.min t.available (S.length s) in
      S.blit_from_bigstring s ~src:t.buf ~src_pos:t.pos ~len;
      consume t len;
      len
    ;;

    let read t s =
      if S.length s = 0 then invalid_argf "Reader.read_%s with empty string" Name.name ();
      with_nonempty_buffer t (function
        | `Ok -> `Ok (read_available t s)
        | `Eof -> `Eof)
    ;;

    let really_read t s =
      Deferred.create (fun result ->
        let rec loop s amount_read =
          if S.length s = 0
          then Ivar.fill result `Ok
          else
            read t s
            >>> function
            | `Eof -> Ivar.fill result (`Eof amount_read)
            | `Ok len -> loop (S.drop_prefix s len) (amount_read + len)
        in
        loop s 0)
    ;;
  end

  module Read_substring =
    Read
      (Substring)
      (struct
        let name = "substring"
      end)

  let read_substring_available = Read_substring.read_available
  let read_substring = Read_substring.read
  let really_read_substring = Read_substring.really_read

  module Read_bigsubstring =
    Read
      (Bigsubstring)
      (struct
        let name = "bigsubstring"
      end)

  let read_bigsubstring = Read_bigsubstring.read
  let really_read_bigsubstring = Read_bigsubstring.really_read

  let really_read_bigstring t bigstring =
    really_read_bigsubstring t (Bigsubstring.create bigstring)
  ;;

  let peek_available t ~len =
    Bigstring.to_string t.buf ~pos:t.pos ~len:(Int.min len t.available)
  ;;

  let peek t ~len =
    match%map get_data_until t ~available_at_least:len with
    | `Eof (_ : int) ->
      assert (t.available < len);
      `Eof
    | `Ok ->
      assert (t.available >= len);
      `Ok (Bigstring.to_string t.buf ~pos:t.pos ~len)
  ;;

  let read_available t ?pos ?len s =
    read_substring_available t (Substring.create s ?pos ?len)
  ;;

  let read t ?pos ?len s = read_substring t (Substring.create s ?pos ?len)
  let really_read t ?pos ?len s = really_read_substring t (Substring.create s ?pos ?len)

  let read_char t =
    with_nonempty_buffer t (function
      | `Eof -> `Eof
      | `Ok ->
        let c = t.buf.{t.pos} in
        consume t 1;
        `Ok c)
  ;;

  let first_char t p =
    let limit = t.pos + t.available in
    let buf = t.buf in
    match p with
    | `Pred p ->
      let rec loop pos =
        if pos = limit then None else if p buf.{pos} then Some pos else loop (pos + 1)
      in
      (* [p] is supplied by the user and may raise, so we wrap [loop] in a [try_with].  We
         put the [try_with] here rather than around the call to [p] to avoid per-character
         try-with overhead. *)
      Or_error.try_with (fun () -> loop t.pos)
    | `Char ch ->
      let rec loop pos =
        if pos = limit
        then None
        else if Char.O.(ch = buf.{pos})
        then Some pos
        else loop (pos + 1)
      in
      Ok (loop t.pos)
  ;;

  let read_until_gen t p ~keep_delim ~max k =
    let rec loop ac total =
      with_nonempty_buffer' t (function
        | `Eof ->
          k
            (Ok
               (if List.is_empty ac
                then `Eof
                else `Eof_without_delim (Bigsubstring.concat_string (List.rev ac))))
        | `Ok ->
          let concat_helper ss lst =
            Bigsubstring.concat_string (List.rev_append lst [ ss ])
          in
          (match first_char t p with
           | Error _ as e -> k e
           | Ok None ->
             let len = t.available in
             let total = total + len in
             let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
             t.buf <- Bigstring.create (Bigstring.length t.buf);
             t.pos <- 0;
             t.available <- 0;
             (match max with
              | Some max when total > max ->
                let s = concat_helper ss ac in
                k (Ok (`Max_exceeded s))
              | Some _ | None -> loop (ss :: ac) total)
           | Ok (Some pos) ->
             let amount_consumed = pos + 1 - t.pos in
             let len = if keep_delim then amount_consumed else amount_consumed - 1 in
             let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
             consume t amount_consumed;
             let res = concat_helper ss ac in
             k (Ok (`Ok res))))
    in
    loop [] 0
  ;;

  let read_until t pred ~keep_delim k =
    read_until_gen t pred ~keep_delim ~max:None (function
      | Error _ as x -> k x
      | Ok (`Max_exceeded _) -> assert false (* impossible - no maximum set *)
      | Ok (`Eof | `Eof_without_delim _ | `Ok _) as x -> k x)
  ;;

  let line_delimiter_pred = `Char '\n'

  let read_line_gen t k =
    read_until t line_delimiter_pred ~keep_delim:false (function
      | Error _ ->
        (* Impossible, since we supplied a [`Char] predicate. *)
        assert false
      | Ok ((`Eof | `Eof_without_delim _) as x) -> k x
      | Ok (`Ok line) ->
        k
          (`Ok
             (let len = String.length line in
              if len >= 1 && Char.O.(line.[len - 1] = '\r')
              then String.sub line ~pos:0 ~len:(len - 1)
              else line)))
  ;;

  let read_line t =
    Deferred.create (fun result ->
      read_line_gen t (fun z ->
        Ivar.fill
          result
          (match z with
           | `Eof_without_delim str -> `Ok str
           | (`Ok _ | `Eof) as x -> x)))
  ;;

  let really_read_line ~wait_time t =
    Deferred.create (fun result ->
      let fill_result = function
        | [] -> Ivar.fill result None
        | ac -> Ivar.fill result (Some (String.concat (List.rev ac)))
      in
      let rec continue ac =
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> fill_result ac
        | `In_use -> Clock.after wait_time >>> fun () -> loop ac
      and loop ac =
        read_line_gen t (function
          | `Eof -> continue ac
          | `Eof_without_delim str -> continue (str :: ac)
          | `Ok line -> fill_result (line :: ac))
      in
      loop [])
  ;;

  let space = Bigstring.of_string " "

  type 'sexp sexp_kind =
    | Plain : Sexp.t sexp_kind
    | Annotated : Sexp.Annotated.t sexp_kind

  let gen_read_sexp (type sexp) ?parse_pos t ~(sexp_kind : sexp sexp_kind) k =
    let rec loop parse_fun =
      with_nonempty_buffer' t (function
        | `Eof ->
          (* The sexp parser doesn't know that a token ends at EOF, so we add a space to
             be sure. *)
          (match Or_error.try_with (fun () -> parse_fun ~pos:0 ~len:1 space) with
           | Error _ as e -> k e
           | Ok (Sexp.Done (sexp, parse_pos)) -> k (Ok (`Ok (sexp, parse_pos)))
           | Ok (Cont (Parsing_toplevel_whitespace, _)) -> k (Ok `Eof)
           | Ok
               (Cont
                  ( ( Parsing_atom
                    | Parsing_list
                    | Parsing_nested_whitespace
                    | Parsing_sexp_comment
                    | Parsing_block_comment )
                  , _ )) ->
             raise_s [%message "Reader.read_sexp got unexpected eof" ~reader:(t : t)])
        | `Ok ->
          (match
             Or_error.try_with (fun () -> parse_fun ~pos:t.pos ~len:t.available t.buf)
           with
           | Error _ as e -> k e
           | Ok (Done (sexp, parse_pos)) ->
             consume t (parse_pos.buf_pos - t.pos);
             k (Ok (`Ok (sexp, parse_pos)))
           | Ok (Cont (_, parse_fun)) ->
             t.available <- 0;
             loop parse_fun))
    in
    let parse ~pos ~len buf : (_, sexp) Sexp.parse_result =
      (* [parse_pos] will be threaded through the entire reading process by the sexplib
         code.  Every occurrence of [parse_pos] above will be identical to the [parse_pos]
         defined here. *)
      let parse_pos =
        match parse_pos with
        | None -> Sexp.Parse_pos.create ~buf_pos:pos ()
        | Some parse_pos -> Sexp.Parse_pos.with_buf_pos parse_pos pos
      in
      match sexp_kind with
      | Plain -> Sexp.parse_bigstring ?parse_pos:(Some parse_pos) ?len:(Some len) buf
      | Annotated ->
        Sexp.Annotated.parse_bigstring ?parse_pos:(Some parse_pos) ?len:(Some len) buf
    in
    loop parse
  ;;

  type 'a read = ?parse_pos:Sexp.Parse_pos.t -> 'a

  let gen_read_sexps ?parse_pos t ~sexp_kind =
    let pipe_r, pipe_w = Pipe.create () in
    let finished =
      Deferred.create (fun result ->
        let rec loop parse_pos =
          gen_read_sexp t ~sexp_kind ?parse_pos (function
            | Error error -> Error.raise error
            | Ok `Eof -> Ivar.fill result ()
            | Ok (`Ok (sexp, parse_pos)) ->
              if Pipe.is_closed pipe_w
              then Ivar.fill result ()
              else Pipe.write pipe_w sexp >>> fun () -> loop (Some parse_pos))
        in
        loop parse_pos)
    in
    upon finished (fun () -> close t >>> fun () -> Pipe.close pipe_w);
    pipe_r
  ;;

  let read_sexps ?parse_pos t = gen_read_sexps t ~sexp_kind:Plain ?parse_pos

  let read_annotated_sexps ?parse_pos t =
    gen_read_sexps t ~sexp_kind:Annotated ?parse_pos
  ;;

  module Peek_or_read = struct
    type t =
      | Peek
      | Read
    [@@deriving sexp_of]

    let to_string = Sexplib.Conv.string_of__of__sexp_of [%sexp_of: t]
  end

  let peek_or_read_bin_prot
        ?(max_len = 100 * 1024 * 1024)
        t
        ~(peek_or_read : Peek_or_read.t)
        (bin_prot_reader : _ Bin_prot.Type_class.reader)
        k
    =
    let error f =
      ksprintf
        (fun msg () ->
           k (Or_error.error "Reader.read_bin_prot" (msg, t) [%sexp_of: string * t]))
        f
    in
    let handle_eof n =
      if n = 0 then k (Ok `Eof) else error "got Eof with %d bytes left over" n ()
    in
    get_data_until t ~available_at_least:Bin_prot.Utils.size_header_length
    >>> function
    | `Eof n -> handle_eof n
    | `Ok ->
      (match t.state with
       | `Not_in_use -> assert false
       | `Closed -> error "Reader.read_bin_prot got closed reader" ()
       | `In_use ->
         let pos = t.pos in
         let pos_ref = ref pos in
         (match
            Or_error.try_with (fun () ->
              Bin_prot.Utils.bin_read_size_header t.buf ~pos_ref)
          with
          | Error _ as e -> k e
          | Ok len ->
            if !pos_ref - pos <> Bin_prot.Utils.size_header_length
            then
              error
                "pos_ref <> len, (%d <> %d)"
                (!pos_ref - pos)
                Bin_prot.Utils.size_header_length
                ();
            if len > max_len then error "max read length exceeded: %d > %d" len max_len ();
            if len < 0 then error "negative length %d" len ();
            let need = Bin_prot.Utils.size_header_length + len in
            get_data_until t ~available_at_least:need
            >>> (function
              | `Eof n -> handle_eof n
              | `Ok ->
                (match t.state with
                 | `Not_in_use -> assert false
                 | `Closed -> error "Reader.read_bin_prot got closed reader" ()
                 | `In_use ->
                   let pos = t.pos + Bin_prot.Utils.size_header_length in
                   pos_ref := pos;
                   (
                     match
                       Or_error.try_with (fun () -> bin_prot_reader.read t.buf ~pos_ref)
                     with
                     | Error _ as e -> k e
                     | Ok v ->
                       if !pos_ref - pos <> len
                       then error "pos_ref <> len, (%d <> %d)" (!pos_ref - pos) len ();
                       (match peek_or_read with
                        | Peek -> ()
                        | Read -> consume t need);
                       k (Ok (`Ok v)))))))
  ;;

  let read_marshal_raw t =
    let eofn n =
      if n = 0
      then `Eof
      else
        raise_s
          [%message "Reader.read_marshal got EOF with bytes remaining" ~_:(n : int)]
    in
    let header = Bytes.create Marshal.header_size in
    match%bind really_read t header with
    | `Eof n -> return (eofn n)
    | `Ok ->
      let len = Marshal.data_size header 0 in
      let buf = Bytes.create (len + Marshal.header_size) in
      Bytes.blit ~src:header ~dst:buf ~src_pos:0 ~dst_pos:0 ~len:Marshal.header_size;
      let sub = Substring.create buf ~pos:Marshal.header_size ~len in
      (match%map really_read_substring t sub with
       | `Eof n -> eofn n
       | `Ok -> `Ok buf)
  ;;

  let read_marshal t =
    match%map read_marshal_raw t with
    | `Eof -> `Eof
    | `Ok buf -> `Ok (Marshal.from_bytes buf 0)
  ;;

  let read_all t read_one =
    let pipe_r, pipe_w = Pipe.create () in
    let finished =
      Deferred.repeat_until_finished () (fun () ->
        match%bind read_one t with
        | `Eof -> return (`Finished ())
        | `Ok one ->
          if Pipe.is_closed pipe_w
          then return (`Finished ())
          else (
            let%map () = Pipe.write pipe_w one in
            `Repeat ()))
    in
    upon finished (fun () -> close t >>> fun () -> Pipe.close pipe_w);
    pipe_r
  ;;

  let lines t = read_all t read_line

  let contents t =
    let buf = Buffer.create 1024 in
    let sbuf = Bytes.create 1024 in
    let%bind () =
      Deferred.repeat_until_finished () (fun () ->
        match%map read t sbuf with
        | `Eof -> `Finished ()
        | `Ok l ->
          Buffer.add_subbytes buf sbuf ~pos:0 ~len:l;
          `Repeat ())
    in
    let%map () = close t in
    Buffer.contents buf
  ;;

  let recv t =
    Deferred.create (fun i ->
      read_line t
      >>> function
      | `Eof -> Ivar.fill i `Eof
      | `Ok length_str ->
        (match
           try Ok (int_of_string length_str) with
           | _ -> Error ()
         with
         | Error () ->
           raise_s
             [%message
               "Reader.recv got strange length" (length_str : string) ~reader:(t : t)]
         | Ok length ->
           let buf = Bytes.create length in
           really_read t buf
           >>> (function
             | `Eof _ -> raise_s [%message "Reader.recv got unexpected EOF"]
             | `Ok -> Ivar.fill i (`Ok buf))))
  ;;

  let transfer t pipe_w =
    Deferred.create (fun finished ->
      don't_wait_for
        (let%map () = Pipe.closed pipe_w in
         Ivar.fill_if_empty finished ());
      let rec loop () =
        with_nonempty_buffer' t (function
          | `Eof -> Ivar.fill_if_empty finished ()
          | `Ok ->
            if not (Pipe.is_closed pipe_w)
            then (
              let pos = t.pos in
              let len = t.available in
              consume t len;
              Pipe.write pipe_w (Bigstring.to_string t.buf ~pos ~len) >>> loop))
      in
      loop ())
  ;;
end

open Internal

(* We now expose all the functions in the mli.  For functions that access a reader in a
   deferred manner, we enclude code to dynamically ensure that there aren't simultaneous
   reads. *)

type nonrec t = t [@@deriving sexp_of]
type nonrec 'a handle_chunk_result = 'a handle_chunk_result [@@deriving sexp_of]
type nonrec 'a handle_iobuf_result = 'a handle_iobuf_result [@@deriving sexp_of]

type nonrec 'a read_one_chunk_at_a_time_result = 'a read_one_chunk_at_a_time_result
[@@deriving sexp_of]

type nonrec 'a read = 'a read

let close = close
let close_finished = close_finished
let create = create
let fd = fd
let id = id
let invariant = invariant
let io_stats = io_stats
let is_closed = is_closed
let last_read_time = last_read_time
let of_in_channel = of_in_channel
let open_file = open_file
let stdin = stdin
let with_close = with_close
let with_file = with_file

let use t =
  let error reason =
    raise_s [%message "can not read from reader" (reason : string) ~reader:(t : t)]
  in
  match t.state with
  | `Closed -> error "closed"
  | `In_use -> error "in use"
  | `Not_in_use -> t.state <- `In_use
;;

let finished_read t =
  match t.state with
  | `Closed -> () (* [f ()] closed it.  Leave it closed. *)
  | `Not_in_use -> assert false (* we're using it *)
  | `In_use -> t.state <- `Not_in_use
;;

let do_read_now t f =
  use t;
  let x = f () in
  finished_read t;
  x
;;

let bytes_available t = do_read_now t (fun () -> t.available)
let peek_available t ~len = do_read_now t (fun () -> peek_available t ~len)
let read_available t ?pos ?len s = do_read_now t (fun () -> read_available t ?pos ?len s)

let do_read t f =
  use t;
  let%map x = f () in
  finished_read t;
  x
;;

let peek t ~len =
  if len < 0 then raise_s [%message "[Reader.peek] got negative len" (len : int)];
  do_read t (fun () -> peek t ~len)
;;

let read t ?pos ?len s = do_read t (fun () -> read t ?pos ?len s)
let read_char t = do_read t (fun () -> read_char t)
let read_substring t s = do_read t (fun () -> read_substring t s)
let read_bigsubstring t s = do_read t (fun () -> read_bigsubstring t s)

let read_one_chunk_at_a_time t ~handle_chunk =
  do_read t (fun () -> read_one_chunk_at_a_time t ~handle_chunk)
;;

let read_one_iobuf_at_a_time t ~handle_chunk =
  do_read t (fun () -> read_one_iobuf_at_a_time t ~handle_chunk)
;;

let really_read t ?pos ?len s = do_read t (fun () -> really_read t ?pos ?len s)
let really_read_substring t s = do_read t (fun () -> really_read_substring t s)
let really_read_bigsubstring t s = do_read t (fun () -> really_read_bigsubstring t s)
let read_line t = do_read t (fun () -> read_line t)
let really_read_line ~wait_time t = do_read t (fun () -> really_read_line ~wait_time t)

(* [do_read_k] takes a [read_k] function that takes a continuation expecting an
   [Or_error.t].  It uses this to do a read returning a deferred.  This allows it to call
   [finished_read] before continuing, in the event that the result is an error. *)
let do_read_k
      (type r r')
      t
      (read_k : (r Or_error.t -> unit) -> unit)
      (make_result : r -> r')
  : r' Deferred.t
  =
  use t;
  Deferred.create (fun result ->
    read_k (fun r ->
      finished_read t;
      Ivar.fill result (make_result (ok_exn r))))
;;

let read_until t p ~keep_delim = do_read_k t (read_until t p ~keep_delim) Fn.id

let read_until_max t p ~keep_delim ~max =
  do_read_k t (read_until_gen t p ~keep_delim ~max:(Some max)) Fn.id
;;

let read_sexp ?parse_pos t =
  do_read_k t (gen_read_sexp t ~sexp_kind:Plain ?parse_pos) (function
    | `Eof -> `Eof
    | `Ok (sexp, _) -> `Ok sexp)
;;

let read_sexps ?parse_pos t =
  use t;
  read_sexps ?parse_pos t
;;

let read_annotated_sexps ?parse_pos t =
  use t;
  read_annotated_sexps ?parse_pos t
;;

let peek_or_read_bin_prot ?max_len t reader ~peek_or_read =
  do_read_k t (peek_or_read_bin_prot ?max_len t reader ~peek_or_read) Fn.id
;;

let peek_bin_prot ?max_len t reader =
  peek_or_read_bin_prot ?max_len t reader ~peek_or_read:Peek
;;

let read_bin_prot ?max_len t reader =
  peek_or_read_bin_prot ?max_len t reader ~peek_or_read:Read
;;

let read_marshal_raw t = do_read t (fun () -> read_marshal_raw t)
let read_marshal t = do_read t (fun () -> read_marshal t)
let recv t = do_read t (fun () -> recv t)

(* [read_all] does not call [use t], because [read_one] will do so each time it is using
   [t]. *)
let read_all t read_one = read_all t read_one

let lines t =
  use t;
  lines t
;;

let contents t = do_read t (fun () -> contents t)
let file_contents file = with_file file ~f:contents

let file_lines file =
  let%bind t = open_file file in
  Pipe.to_list (lines t)
;;

let transfer t =
  use t;
  transfer t
;;

let lseek t offset ~mode =
  do_read t (fun () ->
    t.pos <- 0;
    t.available <- 0;
    Unix_syscalls.lseek t.fd offset ~mode)
;;

let ltell t =
  do_read t (fun () ->
    let%map fd_offset = Unix_syscalls.lseek t.fd Int64.zero ~mode:`Cur in
    Int64.( - ) fd_offset (Int64.of_int t.available))
;;

let get_error
      (type a sexp)
      ~file
      ~(sexp_kind : sexp sexp_kind)
      ~(a_of_sexp : sexp -> a)
      (annotated_sexp : Sexp.Annotated.t)
  =
  try
    ignore
      (a_of_sexp
         (match sexp_kind with
          | Plain -> (Sexp.Annotated.get_sexp annotated_sexp : sexp)
          | Annotated -> (annotated_sexp : sexp))
       : a);
    Ok ()
  with
  | exn ->
    let unexpected_error () =
      error "Reader.load_sexp error" (file, exn) [%sexp_of: string * exn]
    in
    (match exn with
     | Of_sexp_error (exc, bad_sexp) ->
       (match Sexp.Annotated.find_sexp annotated_sexp bad_sexp with
        | None -> unexpected_error ()
        | Some bad_annotated_sexp ->
          (match Sexp.Annotated.get_conv_exn ~file ~exc bad_annotated_sexp with
           | Of_sexp_error (Sexp.Annotated.Conv_exn (pos, exn), sexp) ->
             (* The error produced by [get_conv_exn] already has the file position, so
                we don't wrap with a redundant error message. *)
             Or_error.error
               "invalid sexp"
               (pos, exn, "in", sexp)
               [%sexp_of: string * exn * string * Sexp.t]
           | _ -> unexpected_error ()))
     | _ -> unexpected_error ())
;;

let gen_load_exn
      (type sexp a)
      ?exclusive
      ~(sexp_kind : sexp sexp_kind)
      ~file
      (convert : sexp list -> a)
      (get_error : Sexp.Annotated.t list -> Error.t)
  : a Deferred.t
  =
  let may_load_file_multiple_times = ref false in
  let load ~sexp_kind =
    match%map
      Monitor.try_with ~extract_exn:true (fun () ->
        with_file ?exclusive file ~f:(fun t ->
          (may_load_file_multiple_times
           := (* Although [file] typically is of kind [Fd.Kind.File], it may also have other
                 kinds.  We can only load it multiple times if it has kind [File]. *)
             match Fd.kind (fd t) with
             | File -> true
             | Char | Fifo | Socket _ -> false);
          use t;
          Pipe.to_list (gen_read_sexps t ~sexp_kind)))
    with
    | Ok sexps -> sexps
    | Error exn ->
      (match exn with
       | Sexp.Parse_error { err_msg; parse_state; _ } ->
         (* This code reformats the [Parse_error] produced by sexplib to be more
            readable. *)
         let parse_pos =
           match parse_state with
           | `Sexp { parse_pos; _ } | `Annot { parse_pos; _ } -> parse_pos
         in
         Error.raise
           (Error.create
              "syntax error when parsing sexp"
              (sprintf "%s:%d:%d" file parse_pos.text_line parse_pos.text_char, err_msg)
              [%sexp_of: string * string])
       | _ -> raise exn)
  in
  let%bind sexps = load ~sexp_kind in
  try return (convert sexps) with
  | Of_sexp_error (exn, _bad_subsexp) ->
    if !may_load_file_multiple_times
    then (
      let%bind sexps = load ~sexp_kind:Annotated in
      Error.raise (get_error sexps))
    else
      raise_s
        [%message
          "invalid sexp (failed to determine location information)"
            (file : string)
            (exn : exn)]
  | exn -> raise_s [%message "Reader.load_sexp(s) error" (file : string) (exn : exn)]
;;

type ('sexp, 'a, 'b) load = ?exclusive:bool -> string -> ('sexp -> 'a) -> 'b Deferred.t

let get_load_result_exn = function
  | `Result x -> x
  | `Error (exn, _sexp) -> raise exn
;;

let gen_load_sexp_exn
      (type a sexp)
      ?exclusive
      ~(sexp_kind : sexp sexp_kind)
      ~file
      ~(a_of_sexp : sexp -> a)
  =
  let multiple sexps =
    Error.create
      "Reader.load_sexp requires one sexp but got"
      (List.length sexps, file)
      [%sexp_of: int * string]
  in
  gen_load_exn
    ?exclusive
    ~file
    ~sexp_kind
    (fun sexps ->
       match sexps with
       | [ sexp ] -> a_of_sexp sexp
       | _ -> Error.raise (multiple sexps))
    (fun annot_sexps ->
       match annot_sexps with
       | [ annot_sexp ] ->
         (match get_error ~file ~sexp_kind ~a_of_sexp annot_sexp with
          | Error e -> e
          | Ok () ->
            Error.create
              "conversion of annotated sexp unexpectedly succeeded"
              (Sexp.Annotated.get_sexp annot_sexp)
              [%sexp_of: Sexp.t])
       | _ -> multiple annot_sexps)
;;

let load_sexp_exn ?exclusive file a_of_sexp =
  gen_load_sexp_exn ?exclusive ~sexp_kind:Plain ~file ~a_of_sexp
;;

let load_annotated_sexp_exn ?exclusive file a_of_sexp =
  gen_load_sexp_exn ?exclusive ~sexp_kind:Annotated ~file ~a_of_sexp
;;

let gen_load_sexp ?exclusive ~sexp_kind ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    gen_load_sexp_exn ?exclusive ~sexp_kind ~file ~a_of_sexp)
;;

let load_sexp ?exclusive file a_of_sexp =
  gen_load_sexp ?exclusive ~sexp_kind:Plain ~file ~a_of_sexp
;;

let load_annotated_sexp ?exclusive file a_of_sexp =
  gen_load_sexp ?exclusive ~sexp_kind:Annotated ~file ~a_of_sexp
;;

let gen_load_sexps_exn
      (type a sexp)
      ?exclusive
      ~(sexp_kind : sexp sexp_kind)
      ~file
      ~(a_of_sexp : sexp -> a)
  =
  gen_load_exn
    ?exclusive
    ~file
    ~sexp_kind
    (fun sexps -> List.map sexps ~f:a_of_sexp)
    (fun annot_sexps ->
       Error.of_list
         (List.filter_map annot_sexps ~f:(fun annot_sexp ->
            match get_error ~file ~sexp_kind ~a_of_sexp annot_sexp with
            | Ok _ -> None
            | Error error -> Some error)))
;;

let load_sexps_exn ?exclusive file a_of_sexp =
  gen_load_sexps_exn ?exclusive ~sexp_kind:Plain ~file ~a_of_sexp
;;

let load_annotated_sexps_exn ?exclusive file a_of_sexp =
  gen_load_sexps_exn ?exclusive ~sexp_kind:Annotated ~file ~a_of_sexp
;;

let gen_load_sexps ?exclusive ~sexp_kind ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    gen_load_sexps_exn ?exclusive ~sexp_kind ~file ~a_of_sexp)
;;

let load_sexps ?exclusive file a_of_sexp =
  gen_load_sexps ?exclusive ~sexp_kind:Plain ~file ~a_of_sexp
;;

let load_annotated_sexps ?exclusive file a_of_sexp =
  gen_load_sexps ?exclusive ~sexp_kind:Annotated ~file ~a_of_sexp
;;

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  upon (transfer t pipe_w) (fun () -> close t >>> fun () -> Pipe.close pipe_w);
  pipe_r
;;

let drain t =
  match%bind
    read_one_chunk_at_a_time t ~handle_chunk:(fun _bigstring ~pos:_ ~len:_ ->
      return `Continue)
  with
  | `Stopped _ | `Eof_with_unconsumed_data _ -> assert false
  | `Eof -> close t
;;

type ('a, 'b) load_bin_prot =
  ?exclusive:bool
  -> ?max_len:int
  -> string
  -> 'a Bin_prot.Type_class.reader
  -> 'b Deferred.t

let load_bin_prot ?exclusive ?max_len file bin_reader =
  match%map
    Monitor.try_with_or_error ~here:[%here] ~name:"Reader.load_bin_prot" (fun () ->
      with_file ?exclusive file ~f:(fun t -> read_bin_prot ?max_len t bin_reader))
  with
  | Ok (`Ok v) -> Ok v
  | Ok `Eof -> Or_error.error_string "Reader.load_bin_prot got unexpected eof"
  | Error _ as result -> result
;;

let load_bin_prot_exn ?exclusive ?max_len file bin_reader =
  load_bin_prot ?exclusive ?max_len file bin_reader >>| ok_exn
;;
