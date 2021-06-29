(** [Unix_syscalls] provides an interface to many of the functions in OCaml's standard
    Unix module.  It uses a deferred in the return type of functions that would block.
    The idea is that in an Async program one does not use the standard Unix module, since
    in doing so one could accidentally block the whole program.

    There are also a number of cosmetic changes (e.g., polymorphic variants) and other
    improvements (e.g., phantom types on sockets) over the standard Unix module. *)

open! Core
open! Import
module Syscall_result = Unix.Syscall_result
module Exit = Unix.Exit
module Exit_or_signal = Unix.Exit_or_signal
module Exit_or_signal_or_stop = Unix.Exit_or_signal_or_stop

val system : string -> Exit_or_signal.t Deferred.t
val system_exn : string -> unit Deferred.t
val getpid : unit -> Pid.t
val getppid : unit -> Pid.t option
val getppid_exn : unit -> Pid.t

(** [this_process_became_child_of_init] returns a deferred that becomes determined when
    the current process becomes a child of [init(8)].  This is useful for determining
    whether one's parent has died, because in that case [init] will become one's parent.

    See [Linux_ext.pr_set_pdeathsig : Signal.t -> unit] for a related way to get
    information about parent death.

    [?poll_delay] controls how often to check. *)
val this_process_became_child_of_init
  :  ?poll_delay:Time.Span.t
  -> unit
  -> unit Deferred.t

val nice : int -> int

(** [cores ()] Returns the number of cores. *)
val cores : (unit -> int Deferred.t) Or_error.t

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

type file_perm = int

val openfile : ?perm:file_perm -> string -> mode:open_flag list -> Fd.t Deferred.t

module Lock_mode : sig
  type t =
    | Shared (** Does not exclude other Shared locks, only other Exclusive locks *)
    | Exclusive (** Excludes other Shared and Exclusive locks *)
  [@@deriving sexp_of]
end

module Lock_mechanism : sig
  type t =
    | Lockf
    (** Lockf refers to the ocaml [lockf] function, which, despite the name, does not call
        the UNIX lockf() system call, but rather calls fcntl() with F_SETLKW. *)
    | Flock
  [@@deriving compare, enumerate, sexp]

  include Stringable.S with type t := t

  val arg_type : t Command.Arg_type.t
end

module Lock : sig
  type t =
    { mode : Lock_mode.t
    ; mechanism : Lock_mechanism.t
    }
  [@@deriving sexp_of]
end

(** [with_file file ~mode ~perm ~f] opens [file], and applies [f] to the
    resulting file descriptor.  When the result of [f] becomes determined, it closes the
    descriptor and returns the result of [f].

    If [lock] is supplied, then the file descriptor is locked before calling [f] with
    the specified [lock_mechanism]. Note that it is not unlocked before close, which might
    be significant if this file descriptior is held elsewhere (e.g., by fork() or
    dup()). *)
val with_file
  :  ?lock:Lock.t (** default is no lock *)
  -> ?perm:file_perm
  -> string
  -> mode:open_flag list
  -> f:(Fd.t -> 'a Deferred.t)
  -> 'a Deferred.t

module Open_flags = Unix.Open_flags

(** [fcntl_getfl] and [fcntl_setf] are deferred wrappers around the corresponding
    functions in [Core.Unix] for accessing the open-file-descriptor table. *)
val fcntl_getfl : Fd.t -> Open_flags.t Deferred.t

val fcntl_setfl : Fd.t -> Open_flags.t -> unit Deferred.t

include module type of Fd.Close

val lseek : Fd.t -> int64 -> mode:[< `Set | `Cur | `End ] -> int64 Deferred.t
val truncate : string -> len:int64 -> unit Deferred.t
val ftruncate : Fd.t -> len:int64 -> unit Deferred.t
val fsync : Fd.t -> unit Deferred.t
val fdatasync : Fd.t -> unit Deferred.t
val sync : unit -> unit Deferred.t

(** [lockf fd lock_mode ?len] locks the section of the open file [fd] specified by the
    current file position and [len] (see man lockf).  It returns when the lock has been
    acquired.  It raises if [fd] is closed.

    Note that, despite the name, this function does not call the UNIX lockf() system call;
    rather it calls fcntl() with F_SETLKW *)
val lockf : ?len:Int64.t -> Fd.t -> Lock_mode.t -> unit Deferred.t

(** [try_lockf fd lock_mode ?len] attempts to lock the section of the open file
    [fd] specified by the current file position and [len] (see man lockf).  It returns
    [true] if it acquired the lock.  It raises if [fd] is closed.

    Note that, despite the name, this function does not call the UNIX lockf() system call;
    rather it calls fcntl() with F_SETLK *)
val try_lockf : ?len:Int64.t -> Fd.t -> Lock_mode.t -> bool

(** [test_lockf fd ?len] checks the lock on section of the open file [fd] specified by the
    current file position and [len].  If the section is unlocked or locked by this
    process, it returns true, else it returns false.  It raises if [fd] is closed.

    Note that, despite the name, this function does not call the UNIX lockf() system call;
    rather it calls fcntl() with F_GETLK *)
val test_lockf : ?len:Int64.t -> Fd.t -> bool

(** [unlockf fd ?len] unlocks the section of the open file [fd] specified by the current
    file position and [len].  It raises if [fd] is closed.

    Note that, despite the name, this function does not call the UNIX lockf() system call;
    rather it calls fcntl() with F_UNLCK *)
val unlockf : ?len:Int64.t -> Fd.t -> unit

(** [flock fd lock_mode] locks the open file [fd] (see man 2 flock).  It returns when the
    lock has been acquired.  It raises if [fd] is closed. *)
val flock : Fd.t -> Lock_mode.t -> unit Deferred.t

(** [try_flock fd lock_mode] attempts to lock the open file [fd] (see man 2 flock).  It
    returns [true] if it acquired the lock or [false] if a conflicting lock was already
    present.  It raises if [fd] is closed. *)
val try_flock : Fd.t -> Lock_mode.t -> bool

(** [funlock fd] unlocks the open file [fd] (see [man 2 flock]).  It raises if [fd] is
    closed. *)
val funlock : Fd.t -> unit

module File_kind : sig
  type t =
    [ `File
    | `Directory
    | `Char
    | `Block
    | `Link
    | `Fifo
    | `Socket
    ]
  [@@deriving sexp]

  include Comparable.S with type t := t

  val of_unix : Core.Unix.file_kind -> t
end

module Stats : sig
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

  val of_unix : Core.Unix.stats -> t
  val to_string : t -> string
end

val fstat : Fd.t -> Stats.t Deferred.t


val stat : string -> Stats.t Deferred.t
val lstat : string -> Stats.t Deferred.t
val isatty : Fd.t -> bool Deferred.t
val unlink : string -> unit Deferred.t
val remove : string -> unit Deferred.t
val rename : src:string -> dst:string -> unit Deferred.t

val link
  :  ?force:bool (** default is [false] *)
  -> target:string
  -> link_name:string
  -> unit
  -> unit Deferred.t

val chmod : string -> perm:file_perm -> unit Deferred.t
val fchmod : Fd.t -> perm:file_perm -> unit Deferred.t
val chown : string -> uid:int -> gid:int -> unit Deferred.t
val fchown : Fd.t -> uid:int -> gid:int -> unit Deferred.t

val access
  :  string
  -> [ `Read | `Write | `Exec | `Exists ] list
  -> (unit, exn) Result.t Deferred.t

val access_exn : string -> [ `Read | `Write | `Exec | `Exists ] list -> unit Deferred.t
val set_close_on_exec : Fd.t -> unit
val clear_close_on_exec : Fd.t -> unit
val mkdir : ?p:unit -> ?perm:file_perm -> string -> unit Deferred.t
val rmdir : string -> unit Deferred.t
val chdir : string -> unit Deferred.t
val getcwd : unit -> string Deferred.t
val chroot : string -> unit Deferred.t

type dir_handle = Unix.dir_handle

val opendir : string -> dir_handle Deferred.t

(** [readdir_opt dir_handle] returns the next directory member, or [None] when there are
    no more directory members to return. *)
val readdir_opt : dir_handle -> string option Deferred.t

val rewinddir : dir_handle -> unit Deferred.t
val closedir : dir_handle -> unit Deferred.t

(** The [info] supplied to pipe is debugging information that will be included in the
    returned [Fd]s. *)
val pipe : Info.t -> ([ `Reader of Fd.t ] * [ `Writer of Fd.t ]) Deferred.t

(** Create a named pipe with the given permissions. *)
val mkfifo : ?perm:file_perm (** default is [0o666] *) -> string -> unit Deferred.t

val symlink : target:string -> link_name:string -> unit Deferred.t
val readlink : string -> string Deferred.t

(** [mkstemp prefix] creates and opens a unique temporary file with [prefix],
    automatically appending a suffix of six random characters to make the name unique.
    Unlike C's [mkstemp], [prefix] should not include six X's at the end.

    @raise Unix_error on errors. *)
val mkstemp : string -> (string * Fd.t) Deferred.t

val mkdtemp : string -> string Deferred.t
val getgrouplist : string -> int -> int array Deferred.t

(** Time functions. *)
type process_times = Unix.process_times =
  { tms_utime : float (** User time for the process *)
  ; tms_stime : float (** System time for the process *)
  ; tms_cutime : float (** User time for the children processes *)
  ; tms_cstime : float (** System time for the children processes *)
  }

val times : unit -> process_times

type tm = Unix.tm =
  { tm_sec : int (** Seconds 0..59 *)
  ; tm_min : int (** Minutes 0..59 *)
  ; tm_hour : int (** Hours 0..23 *)
  ; tm_mday : int (** Day of month 1..31 *)
  ; tm_mon : int (** Month of year 0..11 *)
  ; tm_year : int (** Year - 1900 *)
  ; tm_wday : int (** Day of week (Sunday is 0) *)
  ; tm_yday : int (** Day of year 0..365 *)
  ; tm_isdst : bool (** Daylight time savings in effect *)
  }

val time : unit -> float
val gettimeofday : unit -> float
val gmtime : float -> tm
val localtime : float -> tm
val mktime : tm -> float * tm
val utimes : string -> access:float -> modif:float -> unit Deferred.t

type env = Unix.env [@@deriving sexp]

val environment : unit -> string array
val getenv : string -> string option
val getenv_exn : string -> string
val unsafe_getenv : string -> string option
val unsafe_getenv_exn : string -> string
val putenv : key:string -> data:string -> unit
val unsetenv : string -> unit

(** [fork_exec ~prog ~argv ?path ?env] forks and execs [prog] with [argv], and returns the
    child pid.  If [use_path = true] (the default) and [prog] doesn't contain a slash,
    then [fork_exec] searches the PATH environment variable for [prog].  If [env] is
    supplied, it specifies the environment when [prog] is executed.

    If [env] contains multiple bindings for the same variable, the last takes precedence.
    In the case of [`Extend], bindings in [env] take precedence over the existing
    environment.  See {!Unix.exec}. *)
val fork_exec
  :  prog:string
  -> argv:string list
  -> ?use_path:bool (** default is [true] *)
  -> ?env:[ env | `Replace_raw of string list ]
  -> unit
  -> Pid.t Deferred.t

type wait_on =
  [ `Any
  | `Group of Pid.t
  | `My_group
  | `Pid of Pid.t
  ]
[@@deriving sexp]

val wait : wait_on -> (Pid.t * Exit_or_signal.t) Deferred.t
val wait_nohang : wait_on -> (Pid.t * Exit_or_signal.t) option
val wait_untraced : wait_on -> (Pid.t * Exit_or_signal_or_stop.t) Deferred.t
val wait_nohang_untraced : wait_on -> (Pid.t * Exit_or_signal_or_stop.t) option

(** [waitpid pid] returns a deferred that becomes determined with the child's exit
    status, when the child process with process id [pid] exits.  [waitpid_exn] is like
    [waitpid], except the result only becomes determined if the child exits with status
    zero; it raises if the child terminates in any other way. *)
val waitpid : Pid.t -> Exit_or_signal.t Deferred.t

(** Same as {!waitpid}, but guarantees that the resulting [Deferred] is determined
    in the same async job as the [wait] system call, so that it's safe to keep using
    the [pid] if the deferred is not determined. *)
val waitpid_prompt : Pid.t -> Exit_or_signal.t Deferred.t

val waitpid_exn : Pid.t -> unit Deferred.t

module Inet_addr : sig
  include module type of struct
    include Unix.Inet_addr
  end

  (** [of_string_or_getbyname hostname] does a DNS lookup of hostname and returns the
      resulting IP address. *)
  val of_string_or_getbyname : string -> t Deferred.t
end

module Cidr = Core.Unix.Cidr

module Protocol_family : sig
  type t = Unix.Protocol_family.t
end

val socketpair : unit -> Fd.t * Fd.t

module Socket : sig
  module Address : sig
    module Unix : sig
      type t = [ `Unix of string ] [@@deriving bin_io, sexp, compare]

      val create : string -> t
      val to_string : t -> string
      val to_sockaddr : t -> Core.Unix.sockaddr
    end

    module Inet : sig
      type t = [ `Inet of Inet_addr.t * int ] [@@deriving bin_io, compare, hash, sexp_of]

      (** [Blocking_sexp] performs DNS lookup to resolve hostnames to IP addresses. *)
      module Blocking_sexp : sig
        type nonrec t = t [@@deriving bin_io, compare, hash, sexp_poly]
      end

      (** [Show_port_in_test] renders the port as an integer, even in tests, unlike the
          normal [sexp_of_t] and [to_string], which render the port as "PORT" in
          tests. *)
      module Show_port_in_test : sig
        type nonrec t = t [@@deriving sexp_of]

        val to_string : t -> string
      end

      val create : Inet_addr.t -> port:int -> t
      val create_bind_any : port:int -> t
      val addr : t -> Inet_addr.t
      val port : t -> int
      val to_string : t -> string
      val to_host_and_port : t -> Host_and_port.t
      val to_sockaddr : t -> Core.Unix.sockaddr
    end

    type t =
      [ Inet.t
      | Unix.t
      ]
    [@@deriving bin_io, sexp_of]

    (** [Blocking_sexp] performs DNS lookup to resolve hostnames to IP addresses. *)
    module Blocking_sexp : sig
      type nonrec t = t [@@deriving bin_io, hash, sexp]
    end

    val to_string : [< t ] -> string
    val to_sockaddr : [< t ] -> Core.Unix.sockaddr
  end

  module Family : sig
    type 'a t constraint 'a = [< Address.t ]

    val unix : Address.Unix.t t
    val inet : Address.Inet.t t
    val to_string : 'a t -> string
  end

  (** Sockets have a phantom type parameter that tracks the state of the socket in order
      to eliminate certain errors in which socket functions are called in the wrong order.
      Initially, a socket is [`Unconnected].  As various socket functions are called, they
      return a socket with a new phantom state.  Here is a chart of the allowed state
      transitions.

      {v
        Unconnected ---connect--> Active
        |
        | ---bind--> Bound ---listen--> Passive ---accept---> Active
                     |
                     | ---connect--> Active
      v} *)
  type (+'a, 'b) t
    constraint 'a = [< `Unconnected | `Bound | `Passive | `Active ]
    constraint 'b = [< Address.t ]
  [@@deriving sexp_of]

  module Type : sig
    type 'a t constraint 'a = [< Address.t ] [@@deriving sexp_of]

    val tcp : Address.Inet.t t
    val udp : Address.Inet.t t
    val unix : Address.Unix.t t
    val unix_dgram : Address.Unix.t t
  end

  val create : 'addr Type.t -> ([ `Unconnected ], 'addr) t

  val connect
    :  ([< `Unconnected | `Bound ], 'addr) t
    -> 'addr
    -> ([ `Active ], 'addr) t Deferred.t

  val connect_interruptible
    :  ([< `Unconnected | `Bound ], 'addr) t
    -> 'addr
    -> interrupt:unit Deferred.t
    -> [ `Ok of ([ `Active ], 'addr) t | `Interrupted ] Deferred.t


  (** [bind socket addr] sets close_on_exec for the fd of [socket]. *)
  val bind
    :  ?reuseaddr:bool (** default is [true] *)
    -> ([ `Unconnected ], 'addr) t
    -> 'addr
    -> ([ `Bound ], 'addr) t Deferred.t

  (** [bind_inet socket addr] is just like [bind] but is restricted to [Inet.t] addresses
      and is therefore guaranteed not to block. *)
  val bind_inet
    :  ?reuseaddr:bool (** default is [true] *)
    -> ([ `Unconnected ], Address.Inet.t) t
    -> Address.Inet.t
    -> ([ `Bound ], Address.Inet.t) t

  val listen
    :  ?backlog:int (** default is 64; see {!Unix.listen} *)
    -> ([ `Bound ], 'addr) t
    -> ([ `Passive ], 'addr) t

  val accept
    :  ([ `Passive ], 'addr) t
    -> [ `Ok of ([ `Active ], 'addr) t * 'addr | `Socket_closed ] Deferred.t

  val accept_interruptible
    :  ([ `Passive ], 'addr) t
    -> interrupt:unit Deferred.t
    -> [ `Ok of ([ `Active ], 'addr) t * 'addr | `Socket_closed | `Interrupted ]
         Deferred.t

  (** [accept_at_most] is like [accept], but will return up to [limit] connections before
      yielding, where [limit >= 1].  [accept_at_most] first waits for one connection and
      then attempts to retrieve up to [limit] connections through non-blocking
      {!Unix.accept} calls.  If a call to {!Unix.accept} would block before [limit] is
      reached, [accept_at_most] returns the connections retrieved thus far.

      Servers that must service a large number of connections tend to observe a stall in
      connection accept rates when under heavy load.  Increasing [limit] will ameliorate
      this effect, and increase accept rates and overall throughput of the server at the
      cost of increased contention for resources amongst connections.

      For details, see:

      {v
         Acceptable strategies for improving web server performance
         Brecht, Pariag, and Gammo.  USENIX ATEC '04
      v} *)
  val accept_at_most
    :  ([ `Passive ], 'addr) t
    -> limit:int
    -> [ `Ok of (([ `Active ], 'addr) t * 'addr) list | `Socket_closed ] Deferred.t

  val accept_at_most_interruptible
    :  ([ `Passive ], 'addr) t
    -> limit:int
    -> interrupt:unit Deferred.t
    -> [ `Ok of (([ `Active ], 'addr) t * 'addr) list | `Socket_closed | `Interrupted ]
         Deferred.t

  val shutdown : ('a, 'addr) t -> [ `Receive | `Send | `Both ] -> unit
  val fd : ('a, 'addr) t -> Fd.t
  val of_fd : Fd.t -> 'addr Type.t -> ('a, 'addr) t
  val getsockname : ('a, 'addr) t -> 'addr
  val getpeername : ('a, 'addr) t -> 'addr

  module Opt : sig
    type 'a t

    val debug : bool t
    val broadcast : bool t
    val reuseaddr : bool t
    val keepalive : bool t
    val dontroute : bool t
    val oobinline : bool t
    val acceptconn : bool t
    val nodelay : bool t
    val sndbuf : int t
    val rcvbuf : int t
    val error : int t
    val typ : int t
    val rcvlowat : int t
    val sndlowat : int t
    val linger : int option t
    val rcvtimeo : float t
    val sndtimeo : float t
    val mcast_loop : bool t
    val mcast_ttl : int t
    val to_string : 'a t -> string
  end

  val getopt : ('a, 'addr) t -> 'c Opt.t -> 'c
  val setopt : ('a, 'addr) t -> 'c Opt.t -> 'c -> unit

  val mcast_join
    :  ?ifname:string
    -> ?source:Inet_addr.t
    -> ('a, 'addr) t
    -> 'addr
    -> unit

  val mcast_leave
    :  ?ifname:string
    -> ?source:Inet_addr.t
    -> ('a, 'addr) t
    -> 'addr
    -> unit

  (** [bind_to_interface_exn t (`Interface_name "eth0")] restricts messages from being
      received or sent on interfaces other than [eth0].  See
      {!Linux_ext.bind_to_interface}.

      Typically, one would use this function for very specific non-multicast requirements.
      For similar functionality when using multicast, see
      {!Core_unix.mcast_set_ifname}. *)
  val bind_to_interface_exn
    : (([ `Unconnected ], Address.t) t -> Linux_ext.Bound_to_interface.t -> unit) Or_error.t
end

val bind_to_interface_exn : (Fd.t -> Linux_ext.Bound_to_interface.t -> unit) Or_error.t

module Host : sig
  type t = Unix.Host.t =
    { name : string
    ; aliases : string array
    ; family : Protocol_family.t
    ; addresses : Inet_addr.t array
    }

  val getbyname : string -> t option Deferred.t
  val getbyname_exn : string -> t Deferred.t
  val getbyaddr : Inet_addr.t -> t option Deferred.t
  val getbyaddr_exn : Inet_addr.t -> t Deferred.t
  val have_address_in_common : t -> t -> bool
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

(** [sockaddr_blocking_sexp] is like [sockaddr], with [of_sexp] that performs DNS lookup
    to resolve [Inet_addr.t]. *)
type sockaddr_blocking_sexp = sockaddr [@@deriving bin_io, sexp]

module Addr_info : sig
  type t = Unix.addr_info =
    { ai_family : socket_domain
    ; ai_socktype : socket_type
    ; ai_protocol : int
    ; ai_addr : sockaddr
    ; ai_canonname : string
    }
  [@@deriving bin_io, sexp_of]

  (** [Blocking_sexp] performs DNS lookup to resolve hostnames to IP addresses. *)
  module Blocking_sexp : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end

  type getaddrinfo_option = Unix.getaddrinfo_option =
    | AI_FAMILY of socket_domain
    | AI_SOCKTYPE of socket_type
    | AI_PROTOCOL of int
    | AI_NUMERICHOST
    | AI_CANONNAME
    | AI_PASSIVE
  [@@deriving bin_io, sexp]

  val get
    :  ?service:string
    -> host:string
    -> getaddrinfo_option list
    -> t list Deferred.t
end

module Name_info : sig
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
  [@@deriving bin_io, sexp]

  val get : sockaddr -> getnameinfo_option list -> t Deferred.t
end

(** The following functions correspond to the system calls of the same names.
    They can't block so they don't need to return a deferred. *)

val gethostname : unit -> string
val getuid : unit -> int
val geteuid : unit -> int
val getgid : unit -> int
val getegid : unit -> int
val setuid : int -> unit

module Error = Unix.Error

exception Unix_error of Error.t * string * string

module Terminal_io : sig
  type t = Caml.Unix.terminal_io =
    { mutable c_ignbrk : bool (** Ignore the break condition. *)
    ; mutable c_brkint : bool (** Signal interrupt on break condition. *)
    ; mutable c_ignpar : bool (** Ignore characters with parity errors. *)
    ; mutable c_parmrk : bool (** Mark parity errors. *)
    ; mutable c_inpck : bool (** Enable parity check on input. *)
    ; mutable c_istrip : bool (** Strip 8th bit on input characters. *)
    ; mutable c_inlcr : bool (** Map NL to CR on input. *)
    ; mutable c_igncr : bool (** Ignore CR on input. *)
    ; mutable c_icrnl : bool (** Map CR to NL on input. *)
    ; mutable c_ixon : bool (** Recognize XON/XOFF characters on input. *)
    ; mutable c_ixoff : bool (** Emit XON/XOFF chars to control input flow. *)
    ; mutable c_opost : bool (** Enable output processing. *)
    ; mutable c_obaud : int (** Output baud rate (0 means close connection).*)
    ; mutable c_ibaud : int (** Input baud rate. *)
    ; mutable c_csize : int (** Number of bits per character (5-8). *)
    ; mutable c_cstopb : int (** Number of stop bits (1-2). *)
    ; mutable c_cread : bool (** Reception is enabled. *)
    ; mutable c_parenb : bool (** Enable parity generation and detection. *)
    ; mutable c_parodd : bool (** Specify odd parity instead of even. *)
    ; mutable c_hupcl : bool (** Hang up on last close. *)
    ; mutable c_clocal : bool (** Ignore modem status lines. *)
    ; mutable c_isig : bool (** Generate signal on INTR, QUIT, SUSP. *)
    ; mutable c_icanon : bool
    (** Enable canonical processing
        (line buffering and editing) *)
    ; mutable c_noflsh : bool (** Disable flush after INTR, QUIT, SUSP. *)
    ; mutable c_echo : bool (** Echo input characters. *)
    ; mutable c_echoe : bool (** Echo ERASE (to erase previous character). *)
    ; mutable c_echok : bool (** Echo KILL (to erase the current line). *)
    ; mutable c_echonl : bool (** Echo NL even if c_echo is not set. *)
    ; mutable c_vintr : char (** Interrupt character (usually ctrl-C). *)
    ; mutable c_vquit : char (** Quit character (usually ctrl-\ ). *)
    ; mutable c_verase : char (** Erase character (usually DEL or ctrl-H). *)
    ; mutable c_vkill : char (** Kill line character (usually ctrl-U). *)
    ; mutable c_veof : char (** End-of-file character (usually ctrl-D). *)
    ; mutable c_veol : char (** Alternate end-of-line char. (usually none). *)
    ; mutable c_vmin : int
    (** Minimum number of characters to read
        before the read request is satisfied. *)
    ; mutable c_vtime : int (** Maximum read wait (in 0.1s units). *)
    ; mutable c_vstart : char (** Start character (usually ctrl-Q). *)
    ; mutable c_vstop : char (** Stop character (usually ctrl-S). *)
    }

  type setattr_when = Caml.Unix.setattr_when =
    | TCSANOW
    | TCSADRAIN
    | TCSAFLUSH

  val tcgetattr : Fd.t -> t Deferred.t
  val tcsetattr : t -> Fd.t -> mode:setattr_when -> unit Deferred.t
end

(** Structure of entries in the [passwd] database. *)
module Passwd : sig
  type t = Core.Unix.Passwd.t =
    { name : string
    ; passwd : string
    ; uid : int
    ; gid : int
    ; gecos : string
    ; dir : string
    ; shell : string
    }
  [@@deriving fields, sexp]

  val getbyname : string -> t option Deferred.t
  val getbyname_exn : string -> t Deferred.t
  val getbyuid : int -> t option Deferred.t
  val getbyuid_exn : int -> t Deferred.t
end

(** Structure of entries in the [groups] database. *)
module Group : sig
  type t = Core.Unix.Group.t =
    { name : string
    ; passwd : string
    ; gid : int
    ; mem : string array
    }
  [@@deriving fields, sexp]

  val getbyname : string -> t option Deferred.t
  val getbyname_exn : string -> t Deferred.t
  val getbygid : int -> t option Deferred.t
  val getbygid_exn : int -> t Deferred.t
end

module Ifaddr = Core.Unix.Ifaddr

(** Gets the information using the socket-based netlink interface, which can block; see
    https://www.infradead.org/~tgr/libnl/doc/core.html. *)
val getifaddrs : unit -> Ifaddr.t list Deferred.t

(** Returns the login name of the user executing the process.

    This returns a deferred because the username may need to be looked up in what is
    essentially a database elsewhere on the network (winbound user, or NIS). *)
val getlogin : unit -> string Deferred.t

val wordexp
  : (?flags:[ `No_cmd | `Show_err | `Undef ] list -> string -> string array Deferred.t)
      Or_error.t

module Private : sig
  (** [Wait] exposes some internals of the implementation of [wait] and [wait_untraced].
      Those functions return a deferred that becomes determined when a particular child
      process exits.  The implementation, by default, installs a signal handler for
      SIGCHLD that calls [check_all], which considers all undetermined wait results, calls
      [wait_nohang], and fills them in if appropriate.  If OCaml is being used as a plugin
      in some other process that is already managing signals, e.g. in Ecaml, then we can't
      install a signal handler.  So, we expose [do_not_handle_sigchld], which the plugin
      should call before any call to [wait], and will prevent the SIGCHLD handler from
      being installed.  It is then the responsibility of the plugin to call [check_all]
      regularly, so that child processes created by Async are reaped and the corresponding
      deferreds become determined. *)
  module Wait : sig
    val check_all : unit -> unit
    val do_not_handle_sigchld : unit -> unit
  end
end
