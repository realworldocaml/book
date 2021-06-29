module Async_signal = Signal
open Core
open Import
module File_descr = Unix.File_descr
module Exit = Unix.Exit
module Exit_or_signal = Unix.Exit_or_signal
module Exit_or_signal_or_stop = Unix.Exit_or_signal_or_stop
module Syscall_result = Unix.Syscall_result
module Error = Unix.Error

exception Unix_error = Unix.Unix_error

include Fd.Close
module Open_flags = Unix.Open_flags

let system s = In_thread.syscall_exn ~name:"system" (fun () -> Unix.system s)

let system_exn s =
  let%map status = system s in
  if not (Result.is_ok status)
  then raise_s [%message "system failed" ~_:(s : string) (status : Exit_or_signal.t)]
;;

let getpid () = Unix.getpid ()
let getppid () = Unix.getppid ()
let getppid_exn () = Unix.getppid_exn ()

let this_process_became_child_of_init ?(poll_delay = sec 1.) () =
  Deferred.create (fun i ->
    Clock.every poll_delay ~stop:(Ivar.read i) (fun () ->
      if Pid.equal (getppid_exn ()) Pid.init then Ivar.fill i ()))
;;

let nice i = Unix.nice i

let cores =
  Or_error.map Linux_ext.cores ~f:(fun cores () ->
    In_thread.syscall_exn ~name:"cores" cores)
;;

(* basic input/output *)

let convert_open_flag : _ -> Unix.open_flag = function
  | `Rdonly -> O_RDONLY
  | `Wronly -> O_WRONLY
  | `Rdwr -> O_RDWR
  | `Nonblock -> O_NONBLOCK
  | `Append -> O_APPEND
  | `Creat -> O_CREAT
  | `Trunc -> O_TRUNC
  | `Excl -> O_EXCL
  | `Noctty -> O_NOCTTY
  | `Dsync -> O_DSYNC
  | `Sync -> O_SYNC
  | `Rsync -> O_RSYNC
;;

type open_flag =
  [ `Rdonly
  | `Wronly
  | `Rdwr
  | `Nonblock
  | `Append
  | `Creat
  | `Trunc
  | `Excl
  | `Noctty
  | `Dsync
  | `Sync
  | `Rsync
  ]

type file_perm = int [@@deriving sexp, bin_io, compare]

let openfile ?perm file ~mode =
  let mode = List.map mode ~f:convert_open_flag @ [ O_CLOEXEC ] in
  let%bind file_descr =
    In_thread.syscall_exn ~name:"openfile" (fun () -> Unix.openfile ?perm file ~mode)
  in
  let%map kind = Fd.Kind.infer_using_stat file_descr in
  Fd.create kind file_descr (Info.of_string file)
;;

let fcntl_getfl fd =
  Fd.syscall_in_thread_exn fd ~name:"fcntl_getfl" (fun file_descr ->
    Unix.fcntl_getfl file_descr)
;;

let fcntl_setfl fd flags =
  Fd.syscall_in_thread_exn fd ~name:"fcntl_setfl" (fun file_descr ->
    Unix.fcntl_setfl file_descr flags)
;;

let lseek fd pos ~mode =
  let mode : Unix.seek_command =
    match mode with
    | `Set -> SEEK_SET
    | `Cur -> SEEK_CUR
    | `End -> SEEK_END
  in
  Fd.syscall_in_thread_exn fd ~name:"lseek" (fun file_descr ->
    Unix.lseek file_descr pos ~mode)
;;

let truncate filename ~len =
  In_thread.syscall_exn ~name:"truncate" (fun () -> Unix.truncate filename ~len)
;;

let ftruncate fd ~len =
  Fd.syscall_in_thread_exn fd ~name:"ftruncate" (fun file_descr ->
    Unix.ftruncate file_descr ~len)
;;

let fsync fd = Fd.syscall_in_thread_exn fd ~name:"fsync" Unix.fsync
let fdatasync fd = Fd.syscall_in_thread_exn fd ~name:"fdatasync" Unix.fdatasync
let sync () = In_thread.syscall_exn ~name:"sync" Unix.sync

module Lock_mode = struct
  type t =
    | Shared
    | Exclusive
  [@@deriving sexp_of]

  let flock_command : t -> _ = function
    | Shared -> Unix.Flock_command.lock_shared
    | Exclusive -> Unix.Flock_command.lock_exclusive
  ;;
end

let lockf ?(len = 0L) fd (lock_mode : Lock_mode.t) =
  let mode : Unix.lock_command =
    match lock_mode with
    | Shared -> F_RLOCK
    | Exclusive -> F_LOCK
  in
  Fd.syscall_in_thread_exn fd ~name:"lockf" (fun file_descr ->
    Unix.lockf file_descr ~mode ~len)
;;

let try_lockf ?(len = 0L) fd (lock_mode : Lock_mode.t) =
  let mode : Unix.lock_command =
    match lock_mode with
    | Shared -> F_TRLOCK
    | Exclusive -> F_TLOCK
  in
  Fd.syscall_exn fd (fun file_descr ->
    try
      Unix.lockf file_descr ~mode ~len;
      true
    with
    | Unix_error ((EACCES | EAGAIN), _, _) -> false)
;;

let test_lockf ?(len = 0L) fd =
  Fd.syscall_exn fd (fun file_descr ->
    try
      Unix.lockf file_descr ~mode:F_TEST ~len;
      true
    with
    | Unix_error ((EACCES | EAGAIN), _, _) -> false)
;;

let unlockf ?(len = 0L) fd =
  Fd.syscall_exn fd (fun file_descr -> Unix.lockf file_descr ~mode:F_ULOCK ~len)
;;

let flock fd lock_mode =
  let mode = Lock_mode.flock_command lock_mode in
  Fd.syscall_in_thread_exn fd ~name:"flock" (fun file_descr ->
    Unix.flock_blocking file_descr mode)
;;

let try_flock fd lock_mode =
  let mode = Lock_mode.flock_command lock_mode in
  Fd.syscall_exn fd (fun file_descr -> Unix.flock file_descr mode)
;;

let funlock fd =
  Fd.syscall_exn fd (fun file_descr ->
    ignore (Unix.flock file_descr Unix.Flock_command.unlock : bool))
;;

module Lock_mechanism = struct
  module T = struct
    type t =
      | Lockf
      | Flock
    [@@deriving compare, enumerate, sexp, variants]
  end

  let lock (t : T.t) =
    match t with
    | Lockf -> lockf ?len:None
    | Flock -> flock
  ;;

  include T
  include Sexpable.To_stringable (T)

  let arg_type = Command.Param.Arg_type.Export.sexp_conv T.t_of_sexp
end

module Lock = struct
  type t =
    { mode : Lock_mode.t
    ; mechanism : Lock_mechanism.t
    }
  [@@deriving sexp_of]
end

let with_file ?lock ?perm file ~mode ~f =
  (* Here we rely on closing the fd to release the lock if appropriate. In the case of
     flock, this will release the lock only if no other fds are pointing to the same file
     description in the kernel. This will only happen if [f] dups or otherwise copies the
     fd.

     The question of what happens when this function is called with a file such as
     [/dev/fd*] is left as an exercise to the caller. *)
  let doit f =
    let%bind fd = openfile file ~mode ?perm in
    Fd.with_close fd ~f
  in
  match lock with
  | None -> doit f
  | Some { Lock.mode; mechanism } ->
    doit (fun fd ->
      let%bind () = Lock_mechanism.lock mechanism fd mode in
      f fd)
;;

(* file status *)

module File_kind = struct
  module T = struct
    type t =
      [ `File
      | `Directory
      | `Char
      | `Block
      | `Link
      | `Fifo
      | `Socket
      ]
    [@@deriving compare, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)

  let of_unix : Unix.file_kind -> _ = function
    | S_REG -> `File
    | S_DIR -> `Directory
    | S_CHR -> `Char
    | S_BLK -> `Block
    | S_LNK -> `Link
    | S_FIFO -> `Fifo
    | S_SOCK -> `Socket
  ;;
end

module Stats = struct
  type t =
    { dev : int
    ; ino : int
    ; kind : File_kind.t
    ; perm : file_perm
    ; nlink : int
    ; uid : int
    ; gid : int
    ; rdev : int
    ; size : int64
    ; atime : Time.t
    ; mtime : Time.t
    ; ctime : Time.t
    }
  [@@deriving fields, sexp, bin_io, compare]

  let of_unix (u : Unix.stats) =
    let of_float_sec f = Time.of_span_since_epoch (Time.Span.of_sec f) in
    { dev = u.st_dev
    ; ino = u.st_ino
    ; kind = File_kind.of_unix u.st_kind
    ; perm = u.st_perm
    ; nlink = u.st_nlink
    ; uid = u.st_uid
    ; gid = u.st_gid
    ; rdev = u.st_rdev
    ; size = u.st_size
    ; atime = of_float_sec u.st_atime
    ; mtime = of_float_sec u.st_mtime
    ; ctime = of_float_sec u.st_ctime
    }
  ;;

  let to_string t = Sexp.to_string (sexp_of_t t)
end

let fstat fd = Fd.syscall_in_thread_exn fd ~name:"fstat" Unix.fstat >>| Stats.of_unix

let stat filename =
  In_thread.syscall_exn ~name:"stat" (fun () -> Unix.stat filename) >>| Stats.of_unix
;;

let lstat filename =
  In_thread.syscall_exn ~name:"lstat" (fun () -> Unix.lstat filename) >>| Stats.of_unix
;;

(* We treat [isatty] as a blocking operation, because it acts on a file. *)
let isatty fd = Fd.syscall_in_thread_exn fd ~name:"isatty" Unix.isatty

(* operations on filenames *)

let unlink filename =
  In_thread.syscall_exn ~name:"unlink" (fun () -> Unix.unlink filename)
;;

let remove filename =
  In_thread.syscall_exn ~name:"remove" (fun () -> Unix.remove filename)
;;

let rename ~src ~dst =
  In_thread.syscall_exn ~name:"rename" (fun () -> Unix.rename ~src ~dst)
;;

let link ?force ~target ~link_name () =
  In_thread.syscall_exn ~name:"link" (fun () -> Unix.link ?force ~target ~link_name ())
;;

(* file permission and ownership *)

let chmod filename ~perm =
  In_thread.syscall_exn ~name:"chmod" (fun () -> Unix.chmod filename ~perm)
;;

let fchmod fd ~perm =
  Fd.syscall_in_thread_exn fd ~name:"fchmod" (fun file_descr ->
    Unix.fchmod file_descr ~perm)
;;

let chown filename ~uid ~gid =
  In_thread.syscall_exn ~name:"chown" (fun () -> Unix.chown filename ~uid ~gid)
;;

let fchown fd ~uid ~gid =
  Fd.syscall_in_thread_exn fd ~name:"fchown" (fun file_descr ->
    Unix.fchown file_descr ~uid ~gid)
;;

let access filename perm =
  match%map
    Monitor.try_with (fun () ->
      In_thread.syscall_exn ~name:"access" (fun () -> Unix.access filename perm))
  with
  | Ok res -> res
  | Error exn -> Error (Monitor.extract_exn exn)
;;

let access_exn filename perm =
  In_thread.syscall_exn ~name:"access" (fun () -> Unix.access_exn filename perm)
;;

(* operations on file descriptors *)

let set_close_on_exec fd = Fd.with_file_descr_exn fd Unix.set_close_on_exec
let clear_close_on_exec fd = Fd.with_file_descr_exn fd Unix.clear_close_on_exec

let mkdir ?p ?(perm = 0o777) dirname =
  match p with
  | None -> In_thread.syscall_exn ~name:"mkdir" (fun () -> Unix.mkdir dirname ~perm)
  | Some () -> In_thread.syscall_exn ~name:"mkdir" (fun () -> Unix.mkdir_p dirname ~perm)
;;

let rmdir dirname = In_thread.syscall_exn ~name:"rmdir" (fun () -> Unix.rmdir dirname)
let chdir dirname = In_thread.syscall_exn ~name:"chdir" (fun () -> Unix.chdir dirname)
let chroot dirname = In_thread.syscall_exn ~name:"chroot" (fun () -> Unix.chroot dirname)
let getcwd () = In_thread.syscall_exn ~name:"getcwd" (fun () -> Unix.getcwd ())

type dir_handle = Unix.dir_handle

let opendir dirname =
  In_thread.syscall_exn ~name:"opendir" (fun () -> Unix.opendir dirname)
;;

let readdir_opt handle =
  In_thread.syscall_exn ~name:"readdir" (fun () -> Unix.readdir_opt handle)
;;

let rewinddir handle =
  In_thread.syscall_exn ~name:"rewinddir" (fun () -> Unix.rewinddir handle)
;;

let closedir handle =
  In_thread.syscall_exn ~name:"closedir" (fun () -> Unix.closedir handle)
;;

let pipe info =
  let%map reader, writer =
    In_thread.syscall_exn ~name:"pipe" (fun () -> Unix.pipe ~close_on_exec:true ())
  in
  let create file_descr kind = Fd.create Fifo file_descr (Info.tag info ~tag:kind) in
  `Reader (create reader "reader"), `Writer (create writer "writer")
;;

let mkfifo ?(perm = 0o666) name =
  In_thread.syscall_exn ~name:"mkfifo" (fun () -> Unix.mkfifo name ~perm)
;;

(* symlinks *)
let symlink ~target ~link_name =
  In_thread.syscall_exn ~name:"symlink" (fun () -> Unix.symlink ~target ~link_name)
;;

let readlink filename =
  In_thread.syscall_exn ~name:"readlink" (fun () -> Unix.readlink filename)
;;

let mkdtemp filename =
  In_thread.syscall_exn ~name:"mkdtemp" (fun () -> Unix.mkdtemp filename)
;;

let mkstemp filename =
  let%map name, file_descr =
    In_thread.syscall_exn ~name:"mkstemp" (fun () -> Unix.mkstemp filename)
  in
  name, Fd.create File file_descr (Info.of_string name)
;;

let getgrouplist username gid =
  In_thread.syscall_exn ~name:"getgrouplist" (fun () -> Unix.getgrouplist username gid)
;;

type process_times = Unix.process_times =
  { tms_utime : float
  ; tms_stime : float
  ; tms_cutime : float
  ; tms_cstime : float
  }

let times = Unix.times

type tm = Unix.tm =
  { tm_sec : int
  ; tm_min : int
  ; tm_hour : int
  ; tm_mday : int
  ; tm_mon : int
  ; tm_year : int
  ; tm_wday : int
  ; tm_yday : int
  ; tm_isdst : bool
  }

let time = Unix.time
let gettimeofday = Unix.gettimeofday
let gmtime = Unix.gmtime
let localtime = Unix.localtime
let mktime = Unix.mktime

let utimes name ~access ~modif =
  In_thread.syscall_exn ~name:"utimes" (fun () -> Unix.utimes name ~access ~modif)
;;

(* environment *)

type env = Unix.env [@@deriving sexp]

let environment = Unix.environment
let getenv = Sys.getenv
let getenv_exn = Sys.getenv_exn
let unsafe_getenv = Sys.unsafe_getenv
let unsafe_getenv_exn = Sys.unsafe_getenv_exn
let putenv = Unix.putenv
let unsetenv = Unix.unsetenv

(* processes *)

let fork_exec ~prog ~argv ?use_path ?env () =
  In_thread.run (fun () -> Unix.fork_exec ~prog ~argv ?use_path ?env ())
;;

type wait_on = Unix.wait_on [@@deriving sexp_poly]

let wait_nohang = Unix.wait_nohang
let wait_nohang_untraced = Unix.wait_nohang_untraced

module Wait : sig
  val check_all : unit -> unit
  val do_not_handle_sigchld : unit -> unit
  val waitpid : Pid.t -> Exit_or_signal.t Deferred.t
  val wait : wait_on -> (Pid.t * Exit_or_signal.t) Deferred.t
  val wait_untraced : wait_on -> (Pid.t * Exit_or_signal_or_stop.t) Deferred.t
end = struct
  module Kind = struct
    type (_, _) t =
      | Normal : (wait_on, Pid.t * Exit_or_signal.t) t
      | Untraced : (wait_on, Pid.t * Exit_or_signal_or_stop.t) t
      | Waitpid : (Pid.t, Exit_or_signal.t) t
    [@@deriving sexp_of]

    let waitpid_nohang pid =
      let res = wait_nohang (`Pid pid) in
      match res with
      | None -> None
      | Some (pid2, exit_or_signal) ->
        assert (Pid.( = ) pid2 pid);
        Some exit_or_signal
    ;;

    let wait_nohang : type q r. (q, r) t -> q -> r option =
      fun t wait_on ->
        match t with
        | Normal -> wait_nohang wait_on
        | Untraced -> wait_nohang_untraced wait_on
        | Waitpid -> waitpid_nohang wait_on
    ;;
  end

  module Wait = struct
    type t =
      | T :
          { kind : ('q, 'r) Kind.t
          ; result : ('r, exn) Result.t Ivar.t
          ; wait_on : 'q
          }
          -> t
    [@@deriving sexp_of]

    let check (T t) =
      match Kind.wait_nohang t.kind t.wait_on with
      | None -> false
      | Some x ->
        Ivar.fill t.result (Ok x);
        true
      | exception exn ->
        Ivar.fill t.result (Error exn);
        true
    ;;
  end

  let waits : Wait.t list ref = ref []
  let add ~kind ~result ~wait_on = waits := T { kind; result; wait_on } :: !waits
  let check_all () = waits := List.filter !waits ~f:(Fn.non Wait.check)
  let should_handle_sigchld = ref true
  let am_handling_sigchld = ref false

  let do_not_handle_sigchld () =
    if !am_handling_sigchld then raise_s [%message "already handling SIGCHLD" [%here]];
    should_handle_sigchld := false
  ;;

  let install_sigchld_handler_the_first_time =
    lazy
      (if !should_handle_sigchld
       then (
         am_handling_sigchld := true;
         Async_signal.handle [ Signal.chld ] ~f:(fun _ -> check_all ())))
  ;;

  let deferred_wait (type q r) (wait_on : q) ~(kind : (q, r) Kind.t) =
    (* We are going to install a handler for SIGCHLD that will call [wait_nohang wait_on]
       in the future.  However, we must also call [wait_nohang wait_on] right now, in case
       the child already exited, and will thus never cause a SIGCHLD in the future.  We
       must install the SIGCHLD handler first and then call [wait_nohang].  If we did
       [wait_nohang] first, we could miss a SIGCHLD that was delivered after calling
       [wait_nohang] and before installing the handler. *)
    Lazy.force install_sigchld_handler_the_first_time;
    match Kind.wait_nohang kind wait_on with
    | Some result -> return result
    | None ->
      Deferred.create (fun result -> add ~kind ~result ~wait_on) >>| Result.ok_exn
  ;;

  let wait wait_on = deferred_wait wait_on ~kind:Normal
  let wait_untraced wait_on = deferred_wait wait_on ~kind:Untraced
  let waitpid pid = deferred_wait pid ~kind:Waitpid
end

let wait = Wait.wait
let wait_untraced = Wait.wait_untraced
let waitpid_prompt = Wait.waitpid
let waitpid = Wait.waitpid

let waitpid_exn pid =
  let%map exit_or_signal = waitpid pid in
  if Result.is_error exit_or_signal
  then
    raise_s
      [%message
        "child process didn't exit with status 0"
          ~child_pid:(pid : Pid.t)
          (exit_or_signal : Exit_or_signal.t)]
;;

module Inet_addr = struct
  include Unix.Inet_addr

  let of_string_or_getbyname s =
    match of_string s with
    | t -> Deferred.return t
    | exception _ -> In_thread.run (fun () -> of_string_or_getbyname s)
  ;;
end

module Cidr = Unix.Cidr

let bind_to_interface_exn =
  Or_error.map Linux_ext.bind_to_interface ~f:(fun bind_to_interface fd spec ->
    Fd.with_file_descr_exn fd (fun file_descr -> bind_to_interface file_descr spec))
;;

module Socket = struct
  module Address = struct
    module Inet = struct
      type t = [ `Inet of Inet_addr.t * int ] [@@deriving bin_io, compare, hash]

      let to_string_internal ~show_port_in_test (`Inet (a, p)) =
        sprintf
          "%s:%s"
          (Inet_addr.to_string a)
          (if am_running_inline_test && not show_port_in_test
           then "PORT"
           else p |> Int.to_string)
      ;;

      let to_string = to_string_internal ~show_port_in_test:false
      let sexp_of_t t : Sexp.t = Atom (to_string t)

      module Blocking_sexp = struct
        type t = [ `Inet of Inet_addr.Blocking_sexp.t * int ]
        [@@deriving bin_io, compare, hash, sexp]
      end

      module Show_port_in_test = struct
        type t = [ `Inet of Inet_addr.t * int ] [@@deriving sexp_of]

        let to_string = to_string_internal ~show_port_in_test:true
      end

      let addr (`Inet (a, _)) = a
      let port (`Inet (_, p)) = p

      let to_host_and_port (`Inet (addr, port)) =
        Host_and_port.create ~host:(Inet_addr.to_string addr) ~port
      ;;

      let create a ~port = `Inet (a, port)
      let create_bind_any ~port = `Inet (Inet_addr.of_string "0.0.0.0", port)

      let of_sockaddr_exn : Unix.sockaddr -> _ = function
        | ADDR_INET (a, i) -> `Inet (a, i)
        | u -> raise_s [%message "Socket.Address.inet" ~_:(u : Unix.sockaddr)]
      ;;

      let to_sockaddr (`Inet (a, i)) = Unix.ADDR_INET (a, i)
    end

    module Unix = struct
      type t = [ `Unix of string ] [@@deriving bin_io, compare, hash, sexp]

      let create s = `Unix s
      let to_string (`Unix s) = s

      let of_sockaddr_exn : Unix.sockaddr -> t = function
        | ADDR_UNIX s -> `Unix s
        | u -> raise_s [%message "Socket.Address.unix" ~_:(u : Unix.sockaddr)]
      ;;

      let to_sockaddr (`Unix s) = Unix.ADDR_UNIX s
    end

    type t =
      [ Inet.t
      | Unix.t
      ]
    [@@deriving bin_io, sexp_of]

    module Blocking_sexp = struct
      type t =
        [ Inet.Blocking_sexp.t
        | Unix.t
        ]
      [@@deriving bin_io, hash, sexp]
    end

    let to_sockaddr = function
      | #Inet.t as t -> Inet.to_sockaddr t
      | #Unix.t as t -> Unix.to_sockaddr t
    ;;

    let to_string = function
      | `Inet _ as t -> t |> Inet.to_string
      | `Unix _ as t -> t |> Unix.to_string
    ;;
  end

  module Family = struct
    type 'address t =
      { family : Unix.socket_domain
      ; address_of_sockaddr_exn : Unix.sockaddr -> 'address
      ; sexp_of_address : 'address -> Sexp.t
      }
      constraint 'address = [< Address.t ]
    [@@deriving fields]

    let sexp_of_t _ { address_of_sockaddr_exn = _; family; sexp_of_address = _ } =
      [%sexp (family : Unix.socket_domain)]
    ;;

    let to_string t =
      match t.family with
      | PF_INET -> "inet"
      | PF_INET6 -> "inet6"
      | PF_UNIX -> "unix"
    ;;

    let inet =
      { family = PF_INET
      ; address_of_sockaddr_exn = Address.Inet.of_sockaddr_exn
      ; sexp_of_address = Address.Inet.sexp_of_t
      }
    ;;

    let unix =
      { family = PF_UNIX
      ; address_of_sockaddr_exn = Address.Unix.of_sockaddr_exn
      ; sexp_of_address = Address.Unix.sexp_of_t
      }
    ;;
  end

  module Type = struct
    type 'a t =
      { family : 'a Family.t
      ; socket_type : Unix.socket_type
      }
    [@@deriving sexp_of]

    let sexp_of_address t = t.family.sexp_of_address
    let tcp = { family = Family.inet; socket_type = SOCK_STREAM }
    let udp = { family = Family.inet; socket_type = SOCK_DGRAM }
    let unix = { family = Family.unix; socket_type = SOCK_STREAM }
    let unix_dgram = { family = Family.unix; socket_type = SOCK_DGRAM }
    let phys_same (t1 : _ t) (t2 : _ t) = phys_same t1 t2
  end

  module For_info = struct
    type 'addr t =
      { mutable connected_to : 'addr option
      ; mutable bound_on : 'addr option
      ; mutable listening : bool
      ; type_ : 'addr Type.t
      }

    let create type_ = { connected_to = None; bound_on = None; listening = false; type_ }

    let info { connected_to; bound_on; listening; type_ } =
      let type_ =
        if Type.phys_same type_ Type.tcp
        then [%sexp "tcp"]
        else if Type.phys_same type_ Type.udp
        then [%sexp "udp"]
        else [%sexp (type_ : _ Type.t)]
      in
      let bound_on, listening_on =
        if listening then None, bound_on else bound_on, None
      in
      Info.create_s
        [%sexp
          { connected_to : ([< Address.t ] option[@sexp.option])
          ; type_ : Sexp.t
          ; bound_on : ([< Address.t ] option[@sexp.option])
          ; listening_on : ([< Address.t ] option[@sexp.option])
          }]
    ;;
  end

  type 'addr t_ =
    { type_ : 'addr Type.t
    ; fd : Fd.t
    ; for_info : 'addr For_info.t option
    }

  type (+'state, 'addr) t = 'addr t_
    constraint 'state = [< `Unconnected | `Bound | `Passive | `Active ]

  let sexp_of_t _ _ t = Fd.sexp_of_t t.fd
  let fd t = t.fd
  let of_fd fd type_ = { type_; fd; for_info = None }
  let sexp_of_address t = Type.sexp_of_address t.type_

  let create (type_ : _ Type.t) =
    let file_descr =
      Unix.socket
        ~domain:type_.family.family
        ~kind:type_.socket_type
        ~protocol:0
        ~close_on_exec:true
        ()
    in
    let fd =
      Fd.create
        (Socket `Unconnected)
        file_descr
        (Info.create "socket" type_ [%sexp_of: _ Type.t])
    in
    { type_; fd; for_info = Some (For_info.create type_) }
  ;;

  module Opt = struct
    type 'a t =
      { name : string
      ; get : File_descr.t -> 'a
      ; set : File_descr.t -> 'a -> unit
      }

    let to_string t = t.name

    let make getsockopt setsockopt name opt =
      { name
      ; get = (fun fd -> getsockopt fd opt)
      ; set = (fun fd a -> setsockopt fd opt a)
      }
    ;;

    let bool = make Unix.getsockopt Unix.setsockopt
    let int = make Unix.getsockopt_int Unix.setsockopt_int
    let optint = make Unix.getsockopt_optint Unix.setsockopt_optint
    let float = make Unix.getsockopt_float Unix.setsockopt_float
    let debug = bool "debug" SO_DEBUG
    let broadcast = bool "broadcast" SO_BROADCAST
    let reuseaddr = bool "reuseaddr" SO_REUSEADDR
    let keepalive = bool "keepalive" SO_KEEPALIVE
    let dontroute = bool "dontroute" SO_DONTROUTE
    let oobinline = bool "oobinline" SO_OOBINLINE
    let acceptconn = bool "acceptconn" SO_ACCEPTCONN
    let nodelay = bool "nodelay" TCP_NODELAY
    let sndbuf = int "sndbuf" SO_SNDBUF
    let rcvbuf = int "rcvbuf" SO_RCVBUF
    let error = int "error" SO_ERROR
    let typ = int "typ" SO_TYPE
    let rcvlowat = int "rcvlowat" SO_RCVLOWAT
    let sndlowat = int "sndlowat" SO_SNDLOWAT
    let linger = optint "linger" SO_LINGER
    let rcvtimeo = float "rcvtimeo" SO_RCVTIMEO
    let sndtimeo = float "sndtimeo" SO_SNDTIMEO

    (* Since there aren't socket options like SO_MCASTLOOP or SO_MCASTTTL, we wrap
       [Core.Unix] functions to match async's socket-options interface. *)
    let mcast_loop =
      { name = "mcast_loop"; get = Unix.get_mcast_loop; set = Unix.set_mcast_loop }
    ;;

    let mcast_ttl =
      { name = "mcast_ttl"; get = Unix.get_mcast_ttl; set = Unix.set_mcast_ttl }
    ;;
  end

  let getopt t (opt : _ Opt.t) = Fd.with_file_descr_exn t.fd opt.get

  let setopt t (opt : _ Opt.t) a =
    Fd.with_file_descr_exn t.fd (fun file_descr -> opt.set file_descr a)
  ;;

  let mcast_join ?ifname ?source t address =
    Fd.with_file_descr_exn t.fd (fun file_descr ->
      Unix.mcast_join ?ifname ?source file_descr (Address.to_sockaddr address))
  ;;

  let mcast_leave ?ifname ?source t address =
    Fd.with_file_descr_exn t.fd (fun file_descr ->
      Unix.mcast_leave ?ifname ?source file_descr (Address.to_sockaddr address))
  ;;

  let mark_bound t address =
    let info =
      match t.for_info with
      | Some i ->
        i.bound_on <- Some address;
        `Set (For_info.info i)
      | None ->
        `Extend
          (Info.create
             "socket"
             (`bound_on address)
             (let sexp_of_address = sexp_of_address t in
              [%sexp_of: [ `bound_on of address ]]))
    in
    Fd.Private.replace t.fd (Socket `Bound) info
  ;;

  let bind ?(reuseaddr = true) t address =
    setopt t Opt.reuseaddr reuseaddr;
    set_close_on_exec t.fd;
    let sockaddr = Address.to_sockaddr address in
    let%map () =
      Fd.syscall_in_thread_exn t.fd ~name:"bind" (fun file_descr ->
        Unix.bind file_descr ~addr:sockaddr)
    in
    mark_bound t address;
    t
  ;;

  let bind_inet ?(reuseaddr = true) t address =
    setopt t Opt.reuseaddr reuseaddr;
    set_close_on_exec t.fd;
    let sockaddr = Address.to_sockaddr address in
    Fd.syscall_exn t.fd (fun file_descr -> Unix.bind file_descr ~addr:sockaddr);
    mark_bound t address;
    t
  ;;

  let listen ?(backlog = 64) t =
    let fd = t.fd in
    Fd.syscall_exn fd (fun file_descr -> Unix.listen file_descr ~backlog);
    let info =
      match t.for_info with
      | Some i ->
        i.listening <- true;
        `Set (For_info.info i)
      | None -> `Extend (Info.of_string "listening")
    in
    Fd.Private.replace fd (Socket `Passive) info;
    t
  ;;

  let turn_off_nagle (addr : Unix.sockaddr) t =
    match addr, t.type_.socket_type with
    | ADDR_INET _, SOCK_STREAM -> setopt t Opt.nodelay true
    | (ADDR_UNIX _ | ADDR_INET _), _ -> ()
  ;;

  let accept_nonblocking t =
    (* We call [accept] with [~nonblocking:true] because there is no way to use
       [select] to guarantee that an [accept] will not block (see Stevens' book on
       Unix Network Programming, p422). *)
    match
      Fd.with_file_descr t.fd ~nonblocking:true (fun file_descr ->
        Unix.accept file_descr ~close_on_exec:true)
    with
    | `Already_closed -> `Socket_closed
    | `Ok (file_descr, sockaddr) ->
      let address = Family.address_of_sockaddr_exn t.type_.family sockaddr in
      let fd =
        Fd.create
          (Fd.Kind.Socket `Active)
          file_descr
          (Info.create
             "socket"
             (`listening_on t, `client address)
             (let sexp_of_address = sexp_of_address t in
              [%sexp_of: [ `listening_on of (_, _) t ] * [ `client of address ]]))
      in
      let s = { fd; type_ = t.type_; for_info = None } in
      turn_off_nagle sockaddr s;
      `Ok (s, address)
    | `Error (Unix_error ((EAGAIN | EWOULDBLOCK | ECONNABORTED | EINTR), _, _)) ->
      (* If [accept] would have blocked (EAGAIN|EWOULDBLOCK) or got interrupted
         (EINTR), then we return [`Would_block].

         If the kernel returns ECONNABORTED, this means that we first got a connection
         and therefore woke up in "select" (ready to read).  But due to slowness
         (e.g. other long async jobs getting to run first) we could not call accept
         quickly enough, and the other side terminated the connection in the meanwhile.
         Though one could imagine weird client/server applications that absolutely need
         to know that some client aborted the connection before we could accept it, this
         seems quite contrived and unlikely.  In virtually all cases people just want to
         continue waiting for a new connection.

         [Sys_blocked_io] cannot be raised here.  This is a Unix-function, not a
         standard OCaml I/O-function (e.g. for reading from channels).  *)
      `Would_block
    | `Error exn -> raise exn
  ;;

  let accept_interruptible t ~interrupt =
    Deferred.repeat_until_finished () (fun () ->
      match accept_nonblocking t with
      | (`Socket_closed | `Ok _) as x -> return (`Finished x)
      | `Would_block ->
        (match%map Fd.interruptible_ready_to t.fd `Read ~interrupt with
         | `Ready -> `Repeat ()
         | `Interrupted as x -> `Finished x
         | `Closed -> `Finished `Socket_closed
         | `Bad_fd ->
           raise_s [%message "accept on bad file descriptor" ~_:(t.fd : Fd.t)]))
  ;;

  let accept t =
    match%map accept_interruptible t ~interrupt:(Fd.close_started t.fd) with
    | `Interrupted -> `Socket_closed
    | (`Socket_closed | `Ok _) as x -> x
  ;;

  let accept_at_most_interruptible t ~limit ~interrupt =
    if limit < 1
    then
      raise_s
        [%message "[Socket.accept_at_most_interruptible] got [limit] < 1" (limit : int)];
    match%map accept_interruptible t ~interrupt with
    | (`Socket_closed | `Interrupted) as x -> x
    | `Ok connection ->
      (* Now that we have a connection, accept without blocking as many other connections
         as we can, up to [limit] total connections. *)
      let rec loop limit connections =
        if limit = 0
        then connections
        else (
          match accept_nonblocking t with
          | `Ok connection -> loop (limit - 1) (connection :: connections)
          | `Socket_closed | `Would_block -> connections
          | exception exn ->
            don't_wait_for
              (Deferred.List.iter connections ~f:(fun (conn, _) -> Fd.close conn.fd));
            raise exn)
      in
      `Ok (List.rev (loop (limit - 1) [ connection ]))
  ;;

  let accept_at_most t ~limit =
    match%map
      accept_at_most_interruptible t ~limit ~interrupt:(Fd.close_started t.fd)
    with
    | `Interrupted -> `Socket_closed
    | (`Socket_closed | `Ok _) as x -> x
  ;;

  let connect_interruptible t address ~interrupt =
    let sockaddr = Address.to_sockaddr address in
    turn_off_nagle sockaddr t;
    let success () =
      let info =
        match t.for_info with
        | Some i ->
          i.connected_to <- Some address;
          `Set (For_info.info i)
        | None ->
          let sexp_of_address = sexp_of_address t in
          `Extend (Info.create "connected to" address [%sexp_of: address])
      in
      Fd.Private.replace t.fd (Fd.Kind.Socket `Active) info;
      `Ok t
    in
    let fail_closed () = raise_s [%message "connect on closed fd" ~_:(t.fd : Fd.t)] in
    (* We call [connect] with [~nonblocking:true] to initiate an asynchronous connect
       (see Stevens' book on Unix Network Programming, p413).  Once the connect succeeds
       or fails, [select] on the socket will return it in the writeable set. *)
    match
      Fd.with_file_descr t.fd ~nonblocking:true (fun file_descr ->
        Unix.connect file_descr ~addr:sockaddr)
    with
    | `Already_closed -> fail_closed ()
    | `Ok () -> return (success ())
    | `Error (Unix_error ((EINPROGRESS | EINTR), _, _)) ->
      (match%map Fd.interruptible_ready_to t.fd `Write ~interrupt with
       | `Closed -> fail_closed ()
       | `Bad_fd -> raise_s [%message "connect on bad file descriptor" ~_:(t.fd : Fd.t)]
       | `Interrupted as x -> x
       | `Ready ->
         (* We call [getsockopt] to find out whether the connect has succeed or failed. *)
         (match
            Fd.with_file_descr t.fd (fun file_descr ->
              Unix.getsockopt_int file_descr SO_ERROR)
          with
          | `Already_closed -> fail_closed ()
          | `Error exn -> raise exn
          | `Ok err ->
            if err = 0
            then success ()
            else Unix.unix_error err "connect" (Address.to_string address)))
    | `Error e -> raise e
  ;;

  let connect t addr =
    match%map connect_interruptible t addr ~interrupt:(Deferred.never ()) with
    | `Interrupted -> assert false (* impossible *)
    | `Ok t -> t
  ;;

  let shutdown t mode =
    let mode : Unix.shutdown_command =
      match mode with
      | `Receive -> SHUTDOWN_RECEIVE
      | `Send -> SHUTDOWN_SEND
      | `Both -> SHUTDOWN_ALL
    in
    Fd.syscall_exn t.fd (fun file_descr -> Unix.shutdown file_descr ~mode)
  ;;

  let getsockname t =
    Family.address_of_sockaddr_exn
      t.type_.family
      (Unix.getsockname (Fd.file_descr_exn t.fd))
  ;;

  let getpeername t =
    Family.address_of_sockaddr_exn
      t.type_.family
      (Unix.getpeername (Fd.file_descr_exn t.fd))
  ;;

  let bind_to_interface_exn =
    Or_error.map bind_to_interface_exn ~f:(fun f t ifname -> f t.fd ifname)
  ;;
end

let socketpair () =
  let s1, s2 =
    Unix.socketpair ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ~close_on_exec:true ()
  in
  let make_fd s = Fd.create (Fd.Kind.Socket `Active) s (Info.of_string "<socketpair>") in
  make_fd s1, make_fd s2
;;

module Protocol_family = Unix.Protocol_family

module Host = struct
  type t = Unix.Host.t =
    { name : string
    ; aliases : string array
    ; family : Protocol_family.t
    ; addresses : Inet_addr.t array
    }

  let getbyname n =
    In_thread.syscall_exn ~name:"gethostbyname" (fun () -> Unix.Host.getbyname n)
  ;;

  let getbyname_exn n =
    In_thread.syscall_exn ~name:"gethostbyname" (fun () -> Unix.Host.getbyname_exn n)
  ;;

  let getbyaddr a =
    In_thread.syscall_exn ~name:"gethostbyaddr" (fun () -> Unix.Host.getbyaddr a)
  ;;

  let getbyaddr_exn a =
    In_thread.syscall_exn ~name:"gethostbyaddr" (fun () -> Unix.Host.getbyaddr_exn a)
  ;;

  let have_address_in_common = Unix.Host.have_address_in_common
end

type socket_domain = Unix.socket_domain =
  | PF_UNIX
  | PF_INET
  | PF_INET6
[@@deriving bin_io, compare, hash, sexp]

type socket_type = Unix.socket_type =
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET
[@@deriving bin_io, compare, hash, sexp]

type sockaddr = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of Inet_addr.t * int
[@@deriving bin_io, compare, sexp_of]

type sockaddr_blocking_sexp = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of Inet_addr.Blocking_sexp.t * int
[@@deriving bin_io, sexp]

module Addr_info = struct
  type t = Unix.addr_info =
    { ai_family : socket_domain
    ; ai_socktype : socket_type
    ; ai_protocol : int
    ; ai_addr : sockaddr
    ; ai_canonname : string
    }
  [@@deriving bin_io, sexp_of]

  module Blocking_sexp = struct
    type t = Unix.addr_info =
      { ai_family : socket_domain
      ; ai_socktype : socket_type
      ; ai_protocol : int
      ; ai_addr : sockaddr_blocking_sexp
      ; ai_canonname : string
      }
    [@@deriving bin_io, sexp]
  end

  type getaddrinfo_option = Unix.getaddrinfo_option =
    | AI_FAMILY of socket_domain
    | AI_SOCKTYPE of socket_type
    | AI_PROTOCOL of int
    | AI_NUMERICHOST
    | AI_CANONNAME
    | AI_PASSIVE
  [@@deriving bin_io, sexp]

  let get ?(service = "") ~host options =
    In_thread.syscall_exn ~name:"getaddrinfo" (fun () ->
      Unix.getaddrinfo host service options)
  ;;
end

module Name_info = struct
  type t = Unix.name_info =
    { ni_hostname : string
    ; ni_service : string
    }
  [@@deriving bin_io, sexp]

  type getnameinfo_option = Unix.getnameinfo_option =
    | NI_NOFQDN
    | NI_NUMERICHOST
    | NI_NAMEREQD
    | NI_NUMERICSERV
    | NI_DGRAM
  [@@deriving sexp, bin_io]

  let get addr options =
    In_thread.syscall_exn ~name:"getnameinfo" (fun () -> Unix.getnameinfo addr options)
  ;;
end

let gethostname () = Unix.gethostname ()
let setuid uid = Unix.setuid uid
let getuid () = Unix.getuid ()
let getgid () = Unix.getgid ()
let getegid () = Unix.getegid ()
let geteuid () = Unix.geteuid ()

module Terminal_io = struct
  include Unix.Terminal_io

  let tcgetattr fd =
    Fd.syscall_in_thread_exn fd ~name:"tcgetattr" (fun file_descr ->
      tcgetattr file_descr)
  ;;

  let tcsetattr t fd ~mode =
    Fd.syscall_in_thread_exn fd ~name:"tcsetattr" (fun file_descr ->
      tcsetattr t file_descr ~mode)
  ;;
end

module Passwd = struct
  type t = Unix.Passwd.t =
    { name : string
    ; passwd : string
    ; uid : int
    ; gid : int
    ; gecos : string
    ; dir : string
    ; shell : string
    }
  [@@deriving fields, sexp]

  (* The four [Unix.Passwd] functions call C functions that release the OCaml lock and do
     a reentrant system call. *)
  let getbyname n = In_thread.run (fun () -> Unix.Passwd.getbyname n)
  let getbyname_exn n = In_thread.run (fun () -> Unix.Passwd.getbyname_exn n)
  let getbyuid uid = In_thread.run (fun () -> Unix.Passwd.getbyuid uid)
  let getbyuid_exn uid = In_thread.run (fun () -> Unix.Passwd.getbyuid_exn uid)
end

module Group = struct
  type t = Unix.Group.t =
    { name : string
    ; passwd : string
    ; gid : int
    ; mem : string array
    }
  [@@deriving fields, sexp]

  (* The four [Unix.Group] functions call C functions that release the OCaml lock and do a
     reentrant system call. *)
  let getbyname n = In_thread.run (fun () -> Unix.Group.getbyname n)
  let getbyname_exn n = In_thread.run (fun () -> Unix.Group.getbyname_exn n)
  let getbygid gid = In_thread.run (fun () -> Unix.Group.getbygid gid)
  let getbygid_exn gid = In_thread.run (fun () -> Unix.Group.getbygid_exn gid)
end

let getlogin () = In_thread.syscall_exn ~name:"getlogin" (fun () -> Unix.getlogin ())

module Ifaddr = Unix.Ifaddr

let getifaddrs () = In_thread.run Unix.getifaddrs

let wordexp =
  Or_error.map Unix.wordexp ~f:(fun wordexp ?flags glob ->
    In_thread.syscall_exn ~name:"wordexp" (fun () -> wordexp ?flags glob))
;;

module Private = struct
  module Wait = Wait
end
