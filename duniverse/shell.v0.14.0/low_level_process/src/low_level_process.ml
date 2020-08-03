open Core
open Poly
module Sys = Caml.Sys

let rec temp_failure_retry f =
  try
    f ()
  with Unix.Unix_error (EINTR, _, _) -> temp_failure_retry f

let close_non_intr fd =
  temp_failure_retry (fun () -> Unix.close fd)

(* Creates a unix pipe with both sides set close on exec *)
let cloexec_pipe () =
  let (fd1,fd2) as res = Unix.pipe () in
  Unix.set_close_on_exec fd1;
  Unix.set_close_on_exec fd2;
  res

module Process_info = struct
  type t = {
    pid : Pid.t;
    stdin : Unix.File_descr.t;
    stdout : Unix.File_descr.t;
    stderr : Unix.File_descr.t;
  }
end
(* We use a slightly more powerful version of create process than the one in
   core. This version is not quite as carefuly code reviewed but allows us to
   have more control over the forked side of the process (e.g.: chdir).
*)
let internal_create_process ?working_dir ?setuid ?setgid ~env ~prog ~args () =
  let close_on_err = ref [] in
  try
    let (in_read, in_write) = cloexec_pipe () in
    close_on_err := in_read :: in_write :: !close_on_err;
    let (out_read, out_write) = cloexec_pipe () in
    close_on_err := out_read :: out_write :: !close_on_err;
    let (err_read, err_write) = cloexec_pipe () in
    close_on_err := err_read :: err_write :: !close_on_err;
    let pid = Unix_extended.fork_exec
                prog
                args
                ?working_dir
                ?setuid
                ?setgid
                ~env
                ~stdin:in_read
                ~stdout:out_write
                ~stderr:err_write
    in
    close_non_intr in_read;
    close_non_intr out_write;
    close_non_intr err_write;
    {
      Process_info.pid = pid;
      stdin = in_write;
      stdout = out_read;
      stderr = err_read
    }
  with e ->
    List.iter
      ~f:(fun fd -> try close_non_intr fd with _ -> ())
      !close_on_err;
    raise e


(**
   Remembers the last n-characters appended to it....
*)
module Tail_buffer = struct
  (** remembers the output in a circular buffer.
      looped is used to tell whether we loop around the
      boundary of the buffer.
  *)
  type t = {
    buffer : Bytes.t;
    length : int;
    mutable looped : bool;
    mutable position : int;
  }

  let contents b =
    if not b.looped then
      Bytes.To_string.sub b.buffer ~pos:0 ~len:b.position
    else
      let dst = Bytes.create (b.length + 3) in
      Bytes.set dst 0 '.';
      Bytes.set dst 1 '.';
      Bytes.set dst 2 '.';
      Bytes.blit
        ~src:b.buffer
        ~dst ~dst_pos:3
        ~src_pos:b.position
        ~len:(b.length - b.position);
      Bytes.blit ~src:b.buffer
        ~dst
        ~dst_pos:(b.length - b.position + 3)
        ~src_pos:0
        ~len:(b.position);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst

  let create len = {
    buffer = Bytes.create len;
    length = len;
    looped = false;
    position = 0
  }

  let add b src len =
    if b.length <= len then begin
      Bytes.blit
        ~src
        ~dst:b.buffer
        ~dst_pos:0
        ~src_pos:(len - b.length)
        ~len:(b.length);
      b.looped <- true;
      b.position <- 0
    end else
      let leftover =  b.length - b.position in
      if (len < leftover) then begin
        Bytes.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0 ~len;
        b.position <- b.position + len;
      end else begin
        Bytes.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0
          ~len:leftover;
        b.looped <- true;
        let len = (len-leftover) in
        Bytes.blit ~src ~dst:b.buffer ~dst_pos:0 ~src_pos:leftover ~len;
        b.position <- len
      end
end

module Status = struct
  type t =
    [ `Timeout of Time.Span.t
    | `Exited of int
    | `Signaled of Signal.t
      (* WStopped is impossible*)
    ]
  [@@deriving sexp_of]

  let to_string = function
    | `Exited i -> sprintf "exited with code %d" i
    | `Signaled s ->
      sprintf !"died after receiving %{Signal} (signal number %d)"
        s (Signal.to_system_int s)
    | `Timeout s -> sprintf !"Timed out (ran for %{Time.Span})" s

end
module Command_result = struct
  type t= {
    status: Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

let waitpid_nohang pid =
  match Unix.wait_nohang (`Pid pid) with
  | None -> None
  | Some (v, res) -> assert Pid.(v = pid); Some res

(** wait for a given pid to exit;
    returns true when the process exits and false if the process is still runing
    after waiting for [span]
*)
let wait_for_exit ~is_child span pid =
  let end_time = Time.add (Time.now ()) span in
  let exited () =
    if is_child then begin
      match waitpid_nohang pid with
      | None -> true
      | Some _ -> false
    end else
      (* This is the equivalent of calling the C kill with 0 (test whether a process
         exists) *)
      match Signal.send (Signal.of_system_int 0) (`Pid pid) with
      | `Ok -> true
      | `No_such_process -> false
  in
  let rec loop () =
    if Time.(>) (Time.now ()) end_time then
      false
      (*We need to explicitely waitpid the child otherwise we are sending
        signals to a zombie*)
    else if not (exited ()) then true
    else begin
      Time.pause (sec 0.1);
      loop ()
    end
  in
  loop ()

let kill
      ?(is_child=false)
      ?(wait_for=sec 2.0)
      ?(signal = Signal.term)
      pid
  =
  Signal.send_exn signal (`Pid pid);
  if not (wait_for_exit ~is_child wait_for pid) then begin
    begin
      match
        Signal.send Signal.kill (`Pid pid)
      with
      | `No_such_process ->
        if is_child then
          failwith "Process.kill got `No_such_process even though the process was a \
                    child we never waited for"
      | `Ok -> ()
    end;
    if not (wait_for_exit ~is_child wait_for pid) then begin
      failwithf "Process.kill failed to kill %i%s"
        (Pid.to_int pid)
        (if is_child then "" else
           " (or the process wasn't collected by its parent)")
        ()
    end
  end

type t = {
  mutable open_fds : Unix.File_descr.t list;
  mutable in_fds   : Unix.File_descr.t list;
  mutable out_fds  : Unix.File_descr.t list;
  keep_open        : bool;
  buf              : Bytes.t;
  in_cnt           : String.t;
  in_len           : int;
  out_callbacks    : (Unix.File_descr.t*(Bytes.t -> int -> unit)) list;
  pid              : Pid.t;
  mutable in_pos   : int;
}

let close_pooled state fd =
  if List.mem state.open_fds fd ~equal:Unix.File_descr.equal then
    close_non_intr fd;
  state.open_fds <- List.filter ~f:((<>) fd) state.open_fds;
  state.out_fds  <- List.filter ~f:((<>) fd) state.out_fds;
  state.in_fds   <- List.filter ~f:((<>) fd) state.in_fds

let process_io ~read ~write state =
  List.iter write
    ~f:(fun fd ->
      (try
         let len =
           temp_failure_retry (fun () ->
             Unix.single_write_substring fd
               ~buf:state.in_cnt
               ~pos:state.in_pos
               ~len:(state.in_len - state.in_pos))
         in
         state.in_pos <- state.in_pos + len;
         (* Close the process's in_channel iff we are done writing to it*)
         if len = 0 then
           if state.keep_open then
             state.in_fds <- List.filter ~f:((<>) fd) state.in_fds
           else
             close_pooled state fd
       with Unix.Unix_error (EPIPE, _, _) -> close_pooled state fd));
  List.iter read
    ~f:(fun fd ->
      let len =
        temp_failure_retry
          (fun () -> Unix.read fd
                       ~buf:state.buf
                       ~pos:0
                       ~len:(Bytes.length state.buf))
      in
      if len = 0 then
        close_pooled state fd
      else
        let callback =
          List.Assoc.find_exn ~equal:Unix.File_descr.equal state.out_callbacks fd in
        callback state.buf len)

let available_fds =
  let use_select state ~timeout =
    let { Unix.Select_fds. read; write; _; } =
      temp_failure_retry (fun () ->
        Unix.select
          ~read:state.out_fds
          ~write:state.in_fds
          ~except:[]
          ~timeout ())
    in
    read,write
  in
  let use_epoll epoll_create = fun state ~timeout ->
    let module Epoll = Linux_ext.Epoll in
    let timeout =
      match timeout with
      | (`Immediately | `Never) as timeout -> timeout
      | `After span -> `After span
    in
    let epoll_t =
      let fds = List.map ~f:Unix.File_descr.to_int (state.in_fds @ state.out_fds) in
      let max_ready_events = List.length fds in
      let num_file_descrs = 1 + List.fold ~init:max_ready_events ~f:Int.max fds in
      epoll_create ~num_file_descrs ~max_ready_events
    in
    List.iter state.in_fds  ~f:(fun fd -> Epoll.set epoll_t fd Epoll.Flags.out);
    List.iter state.out_fds ~f:(fun fd -> Epoll.set epoll_t fd Epoll.Flags.in_);
    let read, write =
      match temp_failure_retry (fun () -> Epoll.wait epoll_t ~timeout) with
      | `Timeout -> ([], [])
      | `Ok -> Epoll.fold_ready epoll_t ~init:([], []) ~f:(fun (read, write) fd flags ->
        let take_matching_flags acc fd flags ~wanted =
          if Epoll.Flags.do_intersect wanted flags
          then fd :: acc
          else acc
        in
        let read = take_matching_flags read fd flags ~wanted:Epoll.Flags.in_ in
        let write = take_matching_flags write fd flags ~wanted:Epoll.Flags.out in
        (read, write))
    in
    Epoll.close epoll_t;
    (read, write)
  in
  match Linux_ext.Epoll.create with
  | Error _ -> use_select
  | Ok epoll_create -> use_epoll epoll_create
;;

let create
      ~keep_open
      ~use_extra_path
      ~working_dir
      ~setuid
      ~setgid
      ~prog
      ~args
      ~stdoutf
      ~stderrf
      ~input_string
      ~env =
  let full_prog = Shell_internal.path_expand ?use_extra_path prog in
  let process_info =
    internal_create_process
      ?working_dir ?setuid ?setgid
      ~env ~prog:full_prog ~args ()
  in

  let out_fd = process_info.Process_info.stdout
  and in_fd = process_info.Process_info.stdin
  and err_fd = process_info.Process_info.stderr
  and pid = process_info.Process_info.pid in

  {
    keep_open;
    open_fds = [in_fd;out_fd;err_fd];
    in_fds   = [in_fd];
    out_fds  = [err_fd;out_fd];
    buf      = Bytes.create 4096;
    in_cnt   = input_string;
    in_pos   = 0;
    in_len   = String.length input_string;
    out_callbacks = [out_fd,stdoutf;
                     err_fd,stderrf];
    pid
  }

let rec finish_reading state =
  match available_fds state ~timeout:`Immediately with
  | []  ,_ -> ()
  | read,_ ->
    process_io state ~read ~write:[];
    finish_reading state

let rec run_loop ~start_time ~timeout state =
  let read,write = available_fds state ~timeout:(`After (Time_ns.Span.of_sec 0.1)) in
  begin
    try
      process_io state ~read ~write
    with e ->
      kill ~is_child:true state.pid;
      raise e
  end;
  let elapsed = Time.diff (Time.now ()) start_time in
  match timeout with
  | Some timeout when Time.Span.(elapsed > timeout) ->
    kill ~is_child:true state.pid;
    finish_reading state;
    `Timeout elapsed
  | None | Some _ ->
    match waitpid_nohang state.pid with
    | None -> run_loop ~start_time ~timeout state
    | Some status ->
      finish_reading state;
      match status with
      | Ok () -> `Exited 0
      | Error (`Exit_non_zero i) -> `Exited i
      | Error (`Signal s) -> `Signaled s

let run
      ?timeout
      ?use_extra_path
      ?working_dir
      ?setuid
      ?setgid
      ?(env=`Extend [])
      ?input:(input_string="")
      ?(keep_open=false)
      ?(stdoutf=(fun _string _len -> ()))
      ?(stderrf=(fun _string _len -> ()))
      ?(tail_len = 2048) ~prog ~args
      () =
  let stdout_tail = Tail_buffer.create tail_len
  and stderr_tail = Tail_buffer.create tail_len in
  let stdoutf sbuf len =
    stdoutf sbuf len;
    Tail_buffer.add stdout_tail sbuf len
  and stderrf sbuf len =
    stderrf sbuf len;
    Tail_buffer.add stderr_tail sbuf len
  in
  let status =
    protectx (Sys.signal Sys.sigpipe Sys.Signal_ignore,
              create
                ~keep_open
                ~use_extra_path
                ~working_dir
                ~setuid
                ~setgid
                ~stderrf
                ~stdoutf
                ~prog
                ~args
                ~env
                ~input_string)
      ~f:(fun (_old_sigpipe,state) ->
        run_loop state
          ~start_time:(Time.now ())
          ~timeout;)
      ~finally:(fun (old_sigpipe,state) ->
        List.iter state.open_fds
          ~f:close_non_intr;
        ignore (Sys.signal Sys.sigpipe old_sigpipe : Sys.signal_behavior))
  in
  {Command_result.
    status      = status;
    stdout_tail = Tail_buffer.contents stdout_tail;
    stderr_tail = Tail_buffer.contents stderr_tail }

(* Externally export this *)
let kill ?is_child ?wait_for ?(signal=Signal.term) pid =
  kill
    ?is_child
    ?wait_for
    ~signal
    pid

let%test_module _ = (module struct
  let with_fds n ~f =
    let restore_max_fds =
      let module RLimit = Core.Unix.RLimit in
      let max_fds = RLimit.get RLimit.num_file_descriptors in
      match max_fds.RLimit.cur with
      | RLimit.Infinity -> None
      | RLimit.Limit limit when Int64.(of_int Int.(2 * n) < limit) -> None
      | RLimit.Limit _ ->
        RLimit.set RLimit.num_file_descriptors
          { max_fds with RLimit.cur = RLimit.Limit (Int64.of_int (2 * n)) };
        Some max_fds
    in
    let fds = List.init n ~f:(fun _ -> Unix.openfile ~mode:[ Unix.O_RDONLY ] "/dev/null") in
    let retval = Or_error.try_with f in
    List.iter fds ~f:(fun fd -> Unix.close fd);
    Option.iter restore_max_fds ~f:(fun max_fds ->
      let module RLimit = Core.Unix.RLimit in
      RLimit.set RLimit.num_file_descriptors max_fds);
    Or_error.ok_exn retval

  let run_process () = ignore (run ~prog:"true" ~args:[] ())

  let%test_unit _ = with_fds 10 ~f:run_process
  let%test_unit _ = with_fds 1055 ~f:(fun () ->
    [%test_eq: bool]
      (Result.is_ok Linux_ext.Epoll.create)
      (Result.is_ok (Result.try_with run_process)))
end)
