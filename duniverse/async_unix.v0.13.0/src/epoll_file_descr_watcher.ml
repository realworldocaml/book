open Core
open Import
open File_descr_watcher_intf
open Read_write.Export
module Epoll = Linux_ext.Epoll
module Timerfd = Linux_ext.Timerfd

module Flags = struct
  include Epoll.Flags

  let in_out = in_ + out

  (* Use the edge-triggered behavior so we don't have to reset the timerfd when it
     expires. *)
  let for_timerfd = in_ + et

  let of_rw = function
    | `Read -> in_
    | `Write -> out
  ;;
end

type t =
  { timerfd : Timerfd.t
  ; epoll : Epoll.t
  ; handle_fd_read_ready : File_descr.t -> Flags.t -> unit
  ; handle_fd_write_ready : File_descr.t -> Flags.t -> unit
  }
[@@deriving sexp_of, fields]

let backend = Config.File_descr_watcher.Epoll
let is_timerfd t fd = File_descr.equal fd (Timerfd.to_file_descr t.timerfd)

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~timerfd:
        (check (fun timerfd ->
           [%test_result: Flags.t option]
             (Epoll.find t.epoll (Timerfd.to_file_descr timerfd))
             ~expect:(Some Flags.for_timerfd)))
      ~epoll:
        (check (fun epoll ->
           Epoll.iter epoll ~f:(fun _ flags ->
             assert (
               List.exists
                 Flags.[ in_; out; in_out; for_timerfd ]
                 ~f:(fun flags' -> Flags.equal flags flags')))))
      ~handle_fd_read_ready:ignore
      ~handle_fd_write_ready:ignore
  with
  | exn ->
    raise_s
      [%message
        "Epoll_file_descr_watcher.invariant failed"
          (exn : exn)
          ~epoll_file_descr_watcher:(t : t)]
;;

type 'a additional_create_args = timerfd:Linux_ext.Timerfd.t -> 'a

let create ~timerfd ~num_file_descrs ~handle_fd_read_ready ~handle_fd_write_ready =
  let epoll =
    Or_error.ok_exn
      Epoll.create
      ~num_file_descrs
      ~max_ready_events:(Epoll_max_ready_events.raw Config.epoll_max_ready_events)
  in
  let err_or_hup = Flags.(hup + err) in
  let handle_fd read_or_write handle_fd =
    let bit = Flags.of_rw read_or_write in
    fun file_descr flags ->
      (* A difference between select and epoll crops up here: epoll has implicit event
         flags for hangup (HUP) and error (ERR), whereas select will just return that fd
         as "ready" in its appropriate fd_set.  Since we don't know if it's ready for IN
         or OUT, we have to go lookup the entry if the HUP or ERR flag is set. *)
      if Flags.do_intersect flags bit
      || (Flags.do_intersect flags err_or_hup
          && Flags.do_intersect (Epoll.find_exn epoll file_descr) bit)
      then handle_fd file_descr
  in
  Epoll.set epoll (Timerfd.to_file_descr timerfd) Flags.for_timerfd;
  { timerfd
  ; epoll
  ; handle_fd_read_ready = handle_fd `Read handle_fd_read_ready
  ; handle_fd_write_ready = handle_fd `Write handle_fd_write_ready
  }
;;

let reset_in_forked_process t = Epoll.close t.epoll

let iter t ~f =
  Epoll.iter t.epoll ~f:(fun file_descr flags ->
    if not (is_timerfd t file_descr)
    then (
      if Flags.do_intersect flags Flags.in_ then f file_descr `Read;
      if Flags.do_intersect flags Flags.out then f file_descr `Write))
;;

let set t file_descr desired =
  let actual_flags = Epoll.find t.epoll file_descr in
  let desired_flags =
    match desired.read, desired.write with
    | false, false -> None
    | true, false -> Some Flags.in_
    | false, true -> Some Flags.out
    | true, true -> Some Flags.in_out
  in
  match actual_flags, desired_flags with
  | None, None -> ()
  | None, Some d -> Epoll.set t.epoll file_descr d
  | Some _, None -> Epoll.remove t.epoll file_descr
  | Some a, Some d -> if not (Flags.equal a d) then Epoll.set t.epoll file_descr d
;;

module Pre = struct
  type t = unit [@@deriving sexp_of]
end

let pre_check _t = ()

module Check_result = struct
  type t = ([ `Ok | `Timeout ], exn * Backtrace.t) Result.t [@@deriving sexp_of]

  let ok = Ok `Ok
  let timeout = Ok `Timeout
end

let epoll_wait (type a) (epoll : Epoll.t) (timeout : a Timeout.t) (span_or_unit : a) =
  match timeout with
  | Never -> Epoll.wait epoll ~timeout:`Never
  | Immediately -> Epoll.wait epoll ~timeout:`Immediately
  | After -> Epoll.wait_timeout_after epoll span_or_unit
;;

let thread_safe_check t () timeout span_or_unit =
  match epoll_wait t.epoll timeout span_or_unit with
  | `Ok -> Check_result.ok
  | `Timeout -> Check_result.timeout
  | exception e -> Error (e, Backtrace.Exn.most_recent ())
;;

let post_check t check_result =
  try
    match check_result with
    (* We think 514 should be treated like EINTR. *)
    | Error (Unix.Unix_error ((EINTR | EUNKNOWNERR 514), _, _), _) -> ()
    | Error (exn, backtrace) ->
      raise_s
        [%message "epoll raised unexpected exn" (exn : exn) (backtrace : Backtrace.t)]
    | Ok `Timeout -> ()
    | Ok `Ok ->
      Epoll.iter_ready t.epoll ~f:t.handle_fd_write_ready;
      Epoll.iter_ready t.epoll ~f:t.handle_fd_read_ready;
      Epoll.Expert.clear_ready t.epoll
  with
  | exn ->
    let backtrace = Backtrace.Exn.most_recent () in
    raise_s
      [%message
        "Epoll.post_check bug"
          (exn : exn)
          (backtrace : Backtrace.t)
          (check_result : Check_result.t)
          ~epoll_file_descr_watcher:(t : t)]
;;
