open Core
open Import

let debug = Debug.fd

module File_descr = Unix.File_descr

module Kind = struct
  type t =
    | Char
    | Fifo
    | File
    | Socket of [ `Unconnected | `Bound | `Passive | `Active ]
  [@@deriving sexp_of]
end

module State = struct
  (* [State] is is used to keep track of when the file descriptor is in use or being
     closed.  Here are the allowed transitions.

     Open --> Close_requested --> Closed *)
  type t =
    (* [Close_requested (execution_context, do_close_syscall)] indicates that [Fd.close t]
       has been called, but that we haven't yet started the close() syscall, because there
       are still active syscalls using the file descriptor.  Once there are no active
       syscalls, we enqueue a job to [do_close_syscall] in [execution_context]. *)
    | Close_requested of Execution_context.t * (unit -> unit)
    (* [Closed] indicates that there are no more active syscalls and we have started the
       close() syscall. *)
    | Closed
    (* [Open] is the initial state of a file descriptor, and the normal state when it is
       in use.  It indicates that it has not not yet been closed.  The argument is an ivar
       to be filled when [close] is called. *)
    | Open of unit Ivar.t
  [@@deriving sexp_of]

  let transition_is_allowed t t' =
    match t, t' with
    | Open _, Close_requested _ | Close_requested _, Closed -> true
    | _ -> false
  ;;

  let is_open = function
    | Open _ -> true
    | Close_requested _ | Closed -> false
  ;;
end

type ready_to_result =
  [ `Ready
  | `Bad_fd
  | `Closed
  | `Interrupted
  | `Unsupported
  ]
[@@deriving sexp_of]

module Watching = struct
  (* Every fd can be monitored by a file_descr_watcher for read, for write, for both, or
     for neither.  Each fd also has its own notion, independent of the file_descr_watcher,
     of a [Watching.t], for both read and write that indicates the desired state of the
     file_descr_watcher for this fd.  That desired state is maintained only in the fd
     while async jobs are running, and is then synchronized with the file_descr_watcher's
     notion, via calls to [File_descr_watcher.set], just prior to asking the
     file_descr_watcher to check fds for ready I/O.

     Initially, watching state starts as [Not_watching].  When one initially requests that
     the fd be monitored via [request_start_watching], the state transitions to
     [Watch_once] or [Watch_repeatedly].  After the file_descr_watcher detects I/O is
     available, the job in [Watch_repeatedly] is enqueued, or the ivar in [Watch_once] is
     filled and the state transitions to [Stop_requested].  Or, if one calls
     [request_stop_watching], the state transitions to [Stop_requested].  Finally,
     [Stop_requested] will transition to [Not_watching] when the desired state is
     synchronized with the file_descr_watcher. *)
  type t =
    | Not_watching
    | Watch_once of ready_to_result Ivar.t
    | Watch_repeatedly of
        Job.t * [ `Bad_fd | `Closed | `Interrupted | `Unsupported ] Ivar.t
    | Stop_requested
  [@@deriving sexp_of]

  let invariant t : unit =
    try
      match t with
      | Not_watching | Stop_requested -> ()
      | Watch_once ivar -> assert (Ivar.is_empty ivar)
      | Watch_repeatedly (_, ivar) -> assert (Ivar.is_empty ivar)
    with
    | exn -> raise_s [%message "Watching.invariant failed" (exn : exn) ~watching:(t : t)]
  ;;
end

module Nonblock_status = struct
  (* Encodes the knowledge of the O_NONBLOCK flag of an fd. *)
  type t =
    | Blocking
    | Nonblocking
    | Unknown
  [@@deriving sexp_of]
end

module T = struct
  type t =
    { file_descr : File_descr.t
    ; (* [info] is for debugging info. It is mutable because it changes after [bind],
         [listen], or[connect]. *)
      mutable info : Info.t
    ; (* [kind] is mutable because it changes after [bind], [listen], or [connect]. *)
      mutable kind : Kind.t
    ; (* if [can_set_nonblock] is true, async will switch the underlying file
         descriptor into nonblocking mode any time a non-blocking operation is attempted.
         It can be [false] if the user explicitly tells async to avoid modifying that
         flag on the underlying fd, or if Async detects that the file descriptor
         doesn't support nonblocking I/O. *)
      mutable can_set_nonblock : bool
    ; mutable nonblock_status : Nonblock_status.t
    ; mutable state : State.t
    ; watching : Watching.t Read_write_pair.Mutable.t
    ; (* [watching_has_changed] is true if [watching] has changed since the last time
         [watching] was synchronized with the file_descr_watcher.  In this case, the
         fd appears in the scheduler's [fds_whose_watching_has_changed] list so that
         it can be synchronized later. *)
      mutable watching_has_changed : bool
    ; (* [num_active_syscalls] is used to ensure that we don't call [close] on a file
         descriptor until there are no active system calls involving that file descriptor.
         This prevents races in which the OS assigns that file descriptor to a new
         open file, and thus a system call deals with the wrong open file.   If the
         state of an fd is [Close_requested], then once [num_active_syscalls] drops to
         zero, the close() syscall will start and the state will transition to [Closed],
         thus preventing further system calls from using the file descriptor.

         [num_active_syscalls] is abused slightly to include the syscall to the
         file_descr_watcher to check for ready I/O.  Watching for read and for write
         each potentially count for one active syscall. *)
      mutable num_active_syscalls : int
    ; (* [close_finished] becomes determined after the file descriptor has been closed
         and the underlying close() system call has finished. *)
      close_finished : unit Ivar.t
    }
  [@@deriving fields, sexp_of]

  type t_hum = t

  let sexp_of_t_hum { file_descr; info; kind; _ } =
    let file_descr =
      if am_running_inline_test then [%sexp "_"] else [%sexp (file_descr : File_descr.t)]
    in
    [%sexp { file_descr : Sexp.t; info : Info.t; kind : Kind.t }]
  ;;
end

include T

let equal (t : t) t' = phys_equal t t'

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~info:ignore
      ~file_descr:ignore
      ~kind:ignore
      ~can_set_nonblock:ignore
      ~nonblock_status:ignore
      ~state:ignore
      ~watching:
        (check (fun watching -> Read_write_pair.iter watching ~f:Watching.invariant))
      ~watching_has_changed:ignore
      ~num_active_syscalls:
        (check (fun num_active_syscalls ->
           assert (t.num_active_syscalls >= 0);
           let watching read_or_write =
             match Read_write_pair.get t.watching read_or_write with
             | Not_watching -> 0
             | Stop_requested | Watch_once _ | Watch_repeatedly _ -> 1
           in
           assert (t.num_active_syscalls >= watching `Read + watching `Write);
           match t.state with
           | Closed -> assert (num_active_syscalls = 0)
           | Close_requested _ | Open _ -> ()))
      ~close_finished:
        (check (fun close_finished ->
           match t.state with
           | Closed -> ()
           | Close_requested _ -> assert (Ivar.is_empty close_finished)
           | Open close_started ->
             assert (Ivar.is_empty close_finished);
             assert (Ivar.is_empty close_started)))
  with
  | exn -> raise_s [%message "Fd.invariant failed" (exn : exn) ~fd:(t : t)]
;;

let to_int t = File_descr.to_int t.file_descr

let create ?(avoid_setting_nonblock = false) (kind : Kind.t) file_descr info =
  let can_set_nonblock =
    if avoid_setting_nonblock
    then false
    else (
      match kind with
      (* No point in setting nonblocking for files.  Unix doesn't care. *)
      | File -> false
      (* We don't use nonblocking I/O for char devices because we don't want to change the
         blocking status of TTYs, which would affect all processes currently attached to
         that TTY and even persist after this process terminates.

         Also, /dev/null is a char device not supported by epoll.

         We don't really care about doing nonblocking I/O on other character devices,
         e.g. /dev/random. *)
      | Char -> false
      | Fifo -> true
      | Socket _ ->
        (* All one can do on a `Bound socket is listen() to it, and we don't use listen()
           in a nonblocking way.
           `Unconnected sockets support nonblocking so we can connect() them.
           `Passive     sockets support nonblocking so we can accept() them.
           `Active      sockets support nonblocking so we can read() and write() them.

           We need the sockets to be in nonblocking mode for [`Unconnected] and [`Passive]
           sockets, because [accept_interruptible] and [connect_interruptible] in
           unix_syscalls.ml assume that such sockets are nonblocking.  On the other hand,
           there is no such assumption about [`Active] sockets.

           In any case, returning [true] here makes sense. Not only is it harmless
           in case we don't end up using it, but this field is also not updated by
           socket state transitions ([Fd.Private.replace]), so its value needs to be the
           same across the different socket states.
        *)
        true)
  in
  let t =
    { info
    ; file_descr
    ; kind
    ; can_set_nonblock
    ; nonblock_status = Nonblock_status.Unknown
    ; state = State.Open (Ivar.create ())
    ; watching = Read_write_pair.create_both Watching.Not_watching
    ; watching_has_changed = false
    ; num_active_syscalls = 0
    ; close_finished = Ivar.create ()
    }
  in
  if debug then Debug.log "Fd.create" t [%sexp_of: t];
  t
;;

let inc_num_active_syscalls t =
  match t.state with
  | Close_requested _ | Closed -> `Already_closed
  | Open _ ->
    t.num_active_syscalls <- t.num_active_syscalls + 1;
    `Ok
;;

let set_state t new_state =
  if debug then Debug.log "Fd.set_state" (new_state, t) [%sexp_of: State.t * t];
  if State.transition_is_allowed t.state new_state
  then t.state <- new_state
  else
    raise_s
      [%message
        "Fd.set_state attempted disallowed state transition"
          ~fd:(t : t)
          (new_state : State.t)]
;;

let is_open t = State.is_open t.state
let is_closed t = not (is_open t)

let determine_nonblock_status t =
  match t.nonblock_status with
  | Unknown ->
    let flags = Core_unix.fcntl_getfl t.file_descr in
    let nonblock =
      Core_unix.Open_flags.is_subset Core_unix.Open_flags.nonblock ~of_:flags
    in
    t.nonblock_status
    <- (match nonblock with
      | false -> Blocking
      | true -> Nonblocking);
    nonblock
  | Nonblocking -> true
  | Blocking -> false
;;

let supports_nonblock t = if t.can_set_nonblock then true else determine_nonblock_status t

let set_nonblock_if_necessary ?(nonblocking = false) t =
  if nonblocking
  then (
    match determine_nonblock_status t with
    | true -> ()
    | false ->
      if t.can_set_nonblock
      then (
        Unix.set_nonblock t.file_descr;
        t.nonblock_status <- Nonblocking)
      else
        raise_s
          [%message
            "Fd.set_nonblock_if_necessary called on fd that does not support nonblock"
              ~fd:(t : t)])
;;

let with_file_descr_exn ?nonblocking t f =
  if is_closed t
  then raise_s [%message "Fd.with_file_descr_exn got closed fd" ~_:(t : t)]
  else (
    set_nonblock_if_necessary t ?nonblocking;
    f t.file_descr)
;;

let with_file_descr ?nonblocking t f =
  if is_closed t
  then `Already_closed
  else (
    try
      set_nonblock_if_necessary t ?nonblocking;
      `Ok (f t.file_descr)
    with
    | exn -> `Error exn)
;;

let syscall ?nonblocking t f =
  with_file_descr t ?nonblocking (fun file_descr ->
    Result.ok_exn (Syscall.syscall (fun () -> f file_descr)))
;;

let syscall_exn ?nonblocking t f =
  match syscall t f ?nonblocking with
  | `Ok a -> a
  | `Already_closed -> raise_s [%message "Fd.syscall_exn got closed fd" ~_:(t : t)]
  | `Error exn -> raise exn
;;

let syscall_result_exn ?nonblocking t a f =
  if is_closed t
  then raise_s [%message "Fd.syscall_result_exn got closed fd" ~_:(t : t)]
  else (
    set_nonblock_if_necessary t ?nonblocking;
    Syscall.syscall_result2 t.file_descr a f)
;;
