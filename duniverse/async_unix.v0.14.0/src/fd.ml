open Core
open Import
module File_descr = Unix.File_descr
module Scheduler = Raw_scheduler
module Fd = Raw_fd
include Fd.T
open Fd

let debug = debug
let is_closed = is_closed
let is_open = is_open
let syscall = syscall
let syscall_exn = syscall_exn
let syscall_result_exn = syscall_result_exn
let with_file_descr = with_file_descr
let with_file_descr_exn = with_file_descr_exn

module Kind = struct
  include Fd.Kind

  let blocking_infer_using_stat file_descr =
    let st = Unix.fstat file_descr in
    match st.st_kind with
    | S_REG | S_DIR | S_BLK | S_LNK -> File
    | S_CHR -> Char
    | S_FIFO -> Fifo
    | S_SOCK ->
      Socket (if Unix.getsockopt file_descr SO_ACCEPTCONN then `Passive else `Active)
  ;;

  let infer_using_stat file_descr =
    In_thread.syscall_exn ~name:"fstat" (fun () -> blocking_infer_using_stat file_descr)
  ;;
end

let to_string t = Sexp.to_string_hum (sexp_of_t t)
let the_one_and_only () = Scheduler.the_one_and_only ~should_lock:true

let create ?avoid_nonblock_if_possible kind file_descr info =
  Scheduler.create_fd
    ?avoid_nonblock_if_possible
    (the_one_and_only ())
    kind
    file_descr
    info
;;

(* If possible, we try not to treat [stdin], [stdout], or [stderr] as nonblocking so that
   one can use Core I/O libraries simultaneously with async without them failing due to
   [Sys_blocked_io]. *)
let create_std_descr file_descr info =
  create
    (Kind.blocking_infer_using_stat file_descr)
    file_descr
    info
    ~avoid_nonblock_if_possible:true
;;

let stdin = Memo.unit (fun () -> create_std_descr Unix.stdin (Info.of_string "<stdin>"))

let stdout =
  Memo.unit (fun () -> create_std_descr Unix.stdout (Info.of_string "<stdout>"))
;;

let stderr =
  Memo.unit (fun () -> create_std_descr Unix.stderr (Info.of_string "<stderr>"))
;;

let clear_nonblock t =
  if t.supports_nonblock
  then (
    t.supports_nonblock <- false;
    if t.have_set_nonblock
    then (
      t.have_set_nonblock <- false;
      Unix.clear_nonblock t.file_descr))
;;

module Close = struct
  type socket_handling =
    | Shutdown_socket
    | Do_not_shutdown_socket

  type file_descriptor_handling =
    | Close_file_descriptor of socket_handling
    | Do_not_close_file_descriptor

  let close ?(file_descriptor_handling = Close_file_descriptor Shutdown_socket) t =
    if debug then Debug.log "Fd.close" t [%sexp_of: t];
    (match t.state with
     | Close_requested _ | Closed -> ()
     | Open close_started ->
       Ivar.fill close_started ();
       let do_close_syscall () =
         don't_wait_for
           (let%map () =
              match file_descriptor_handling with
              | Do_not_close_file_descriptor -> return ()
              | Close_file_descriptor socket_handling ->
                Monitor.protect
                  ~finally:(fun () ->
                    In_thread.syscall_exn ~name:"close" (fun () ->
                      Unix.close t.file_descr))
                  (fun () ->
                     match t.kind, socket_handling with
                     | Socket `Active, Shutdown_socket ->
                       In_thread.syscall_exn ~name:"shutdown" (fun () ->
                         Unix.shutdown t.file_descr ~mode:SHUTDOWN_ALL)
                     | _ -> return ())
            in
            Ivar.fill t.close_finished ())
       in
       let scheduler = the_one_and_only () in
       let kernel_scheduler = scheduler.kernel_scheduler in
       set_state
         t
         (Close_requested
            (Kernel_scheduler.current_execution_context kernel_scheduler, do_close_syscall));
       (* Notify other users of this fd that it is going to be closed. *)
       Scheduler.request_stop_watching scheduler t `Read `Closed;
       Scheduler.request_stop_watching scheduler t `Write `Closed;
       (* If there are no syscalls in progress, then start closing the fd. *)
       Scheduler.maybe_start_closing_fd scheduler t);
    Ivar.read t.close_finished
  ;;
end

include Close

let close_finished t = Ivar.read t.close_finished

let close_started t =
  match t.state with
  | Open close_started -> Ivar.read close_started
  | Close_requested _ | Closed -> return ()
;;

let with_close t ~f = Monitor.protect (fun () -> f t) ~finally:(fun () -> close t)

let with_file_descr_deferred t f =
  match inc_num_active_syscalls t with
  | `Already_closed -> return `Already_closed
  | `Ok ->
    let%map result = Monitor.try_with (fun () -> f t.file_descr) in
    Scheduler.dec_num_active_syscalls_fd (the_one_and_only ()) t;
    (match result with
     | Ok x -> `Ok x
     | Error e -> `Error e)
;;

let with_file_descr_deferred_exn t f =
  match%map with_file_descr_deferred t f with
  | `Ok x -> x
  | `Error exn -> raise exn
  | `Already_closed ->
    raise_s [%message "Fd.with_file_descr_deferred_exn got closed fd" ~_:(t : t_hum)]
;;

let start_watching t read_or_write watching =
  if debug
  then Debug.log "Fd.start_watching" (t, read_or_write) [%sexp_of: t * Read_write.Key.t];
  let r = the_one_and_only () in
  match Scheduler.request_start_watching r t read_or_write watching with
  | (`Unsupported | `Already_closed | `Watching) as x -> x
  | `Already_watching ->
    raise_s
      [%message
        "cannot watch an fd already being watched"
          ~fd:(t : t)
          ~scheduler:(r : Scheduler.t)]
;;

let stop_watching_upon_interrupt t read_or_write ivar ~interrupt =
  upon
    (choose
       [ choice interrupt (fun () -> `Interrupted)
       ; choice (Ivar.read ivar) (fun _ -> `Not_interrupted)
       ])
    (function
      | `Not_interrupted -> ()
      | `Interrupted ->
        if Ivar.is_empty ivar
        then
          Scheduler.request_stop_watching
            (the_one_and_only ())
            t
            read_or_write
            `Interrupted)
;;

let interruptible_ready_to t read_or_write ~interrupt =
  if debug
  then
    Debug.log
      "Fd.interruptible_ready_to"
      (t, read_or_write)
      [%sexp_of: t * Read_write.Key.t];
  let ready = Ivar.create () in
  match start_watching t read_or_write (Watch_once ready) with
  | `Already_closed -> return `Closed
  | `Unsupported ->
    return (if Deferred.is_determined interrupt then `Interrupted else `Ready)
  | `Watching ->
    stop_watching_upon_interrupt t read_or_write ready ~interrupt;
    Ivar.read ready
;;

let ready_to t read_or_write =
  if debug
  then Debug.log "Fd.ready_to" (t, read_or_write) [%sexp_of: t * Read_write.Key.t];
  let ready = Ivar.create () in
  match start_watching t read_or_write (Watch_once ready) with
  | `Already_closed -> return `Closed
  | `Unsupported -> return `Ready
  | `Watching ->
    (match%map Ivar.read ready with
     | (`Bad_fd | `Closed | `Ready) as x -> x
     | `Interrupted -> (* impossible *) assert false)
;;

let interruptible_every_ready_to t read_or_write ~interrupt f x =
  if debug
  then
    Debug.log
      "Fd.interruptible_every_ready_to"
      (t, read_or_write)
      [%sexp_of: t * Read_write.Key.t];
  let job = Scheduler.(create_job (t ())) f x in
  let finished = Ivar.create () in
  match start_watching t read_or_write (Watch_repeatedly (job, finished)) with
  | `Already_closed -> return `Closed
  | `Unsupported -> return `Unsupported
  | `Watching ->
    stop_watching_upon_interrupt t read_or_write finished ~interrupt;
    (Ivar.read finished
     :> [ `Bad_fd | `Closed | `Unsupported | `Interrupted ] Deferred.t)
;;

let every_ready_to t read_or_write f x =
  if debug
  then Debug.log "Fd.every_ready_to" (t, read_or_write) [%sexp_of: t * Read_write.Key.t];
  let job = Scheduler.(create_job (t ())) f x in
  let finished = Ivar.create () in
  match start_watching t read_or_write (Watch_repeatedly (job, finished)) with
  | `Already_closed -> return `Closed
  | `Unsupported -> return `Unsupported
  | `Watching ->
    (match%map Ivar.read finished with
     | (`Bad_fd | `Closed) as x -> x
     | `Interrupted -> (* impossible *) assert false)
;;

let syscall_in_thread t ~name f =
  match%map
    with_file_descr_deferred t (fun file_descr ->
      In_thread.syscall ~name (fun () -> f file_descr))
  with
  | `Error e ->
    (* [In_thread.syscall] catches any exceptions [f] can raise, so this can only be
       reached when there's something wrong with the [In_thread] machinery itself. *)
    raise_s
      [%message "Fd.syscall_in_thread problem -- please report this" name ~_:(e : exn)]
  | `Already_closed -> `Already_closed
  | `Ok x ->
    (match x with
     | Ok x -> `Ok x
     | Error exn -> `Error exn)
;;

let syscall_in_thread_exn t ~name f =
  match%map syscall_in_thread t ~name f with
  | `Ok x -> x
  | `Error exn -> raise exn
  | `Already_closed ->
    raise_s [%message "Fd.syscall_in_thread_exn of a closed fd" name ~_:(t : t_hum)]
;;

let of_in_channel ic kind =
  create kind (Unix.descr_of_in_channel ic) (Info.of_string "<of_in_channel>")
;;

let of_out_channel oc kind =
  create kind (Unix.descr_of_out_channel oc) (Info.of_string "<of_out_channel>")
;;

let of_in_channel_auto ic =
  Kind.infer_using_stat (Unix.descr_of_in_channel ic) >>| of_in_channel ic
;;

let of_out_channel_auto oc =
  Kind.infer_using_stat (Unix.descr_of_out_channel oc) >>| of_out_channel oc
;;

let file_descr_exn t =
  if is_closed t
  then raise_s [%message "Fd.file_descr_exn on already closed fd" ~_:(t : t)]
  else t.file_descr
;;

let to_int_exn t = File_descr.to_int (file_descr_exn t)

module Private = struct
  let replace t kind info =
    if is_closed t
    then
      raise_s
        [%message
          "Fd.replace got closed fd"
            ~fd:(t : t)
            (kind : Kind.t)
            ~scheduler:(the_one_and_only () : Scheduler.t)]
    else (
      t.kind <- kind;
      t.info
      <- (match info with
        | `Set i -> i
        | `Extend i ->
          Info.create
            "replaced"
            (i, `previously_was t.info)
            [%sexp_of: Info.t * [ `previously_was of Info.t ]]))
  ;;
end
