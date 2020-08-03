(** Extensions to [Core.Unix]. *)
open! Core

(** [fork_exec prog args ~stdin ~stdout ~stderr ~setuid ~setgid]
    forks a new process that executes the program
    in file [prog], with arguments [args]. The pid of the new
    process is returned immediately; the new process executes
    concurrently with the current process.

    The function raises EPERM if when using [set{gid,uid}] and the user id is
    not 0.

    The standard input and outputs of the new process are connected
    to the descriptors [stdin], [stdout] and [stderr].

    The close_on_exec flag is cleared from [stderr] [stdout] and [stdin] so it's
    safe to pass in fds with [close_on_exec] set.

    @param path_lookup if [true] than we use PATH to find the process to exec.
    @env specifies the environment the process runs in

    ERRORS:
    Unix.unix_error. This function should not raise EINTR; it will restart
    itself automatically.

    RATIONAL:
    [setuid] and [setgid] do not do a full id drop (e.g.: they save the id in
    saved id) when the user does not have the privileges required to setuid to
    anyone.

    By default all file descriptors should be set_closexec ASAP after being open
    to avoid being captured in parallel execution of fork_exec; resetting the
    closexec flag on the forked flag is a cleaner and more thread safe approach.

    BUGS:
    The capabilities for setuid in linux are not tied to the uid 0 (man 7
    capabilities). It is still fair to assume that under most system this
    capability is there IFF uid == 0. A more fine grain permissionning approach
    would make this function non-portable and be hard to implement in an
    async-signal-way.

    Because this function keeps the lock for most of its lifespan and restarts
    automatically on EINTR it might prevent the OCaml signal handlers to run in
    that thread.
*)
val fork_exec
  :  ?stdin:Unix.File_descr.t
  -> ?stdout:Unix.File_descr.t
  -> ?stderr:Unix.File_descr.t
  -> ?path_lookup:bool
  -> ?env:[ `Extend of (string * string) list | `Replace of (string * string) list ]
  -> ?working_dir:string
  -> ?setuid:int
  -> ?setgid:int
  -> string
  -> string list
  -> Pid.t

val seteuid : int -> unit
val setreuid : uid:int -> euid:int -> unit

(** Network to host order long, like C. *)
external ntohl : Int32.t -> Int32.t = "extended_ml_ntohl"

(** Host to network order long, like C. *)
external htonl : Int32.t -> Int32.t = "extended_ml_htonl"

type statvfs =
  { bsize : int (** file system block size *)
  ; frsize : int (** fragment size *)
  ; blocks : int (** size of fs in frsize units *)
  ; bfree : int (** # free blocks *)
  ; bavail : int (** # free blocks for non-root *)
  ; files : int (** # inodes *)
  ; ffree : int (** # free inodes *)
  ; favail : int (** # free inodes for non-root *)
  ; fsid : int (** file system ID *)
  ; flag : int (** mount flags *)
  ; namemax : int (** maximum filename length *)
  }
[@@deriving sexp, bin_io]

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd : sig
  open Unix.Passwd

  (** [of_passwd_line] parse a passwd-like line *)
  val of_passwd_line : string -> t option

  (** [of_passwd_line_exn] parse a passwd-like line *)
  val of_passwd_line_exn : string -> t

  (** [of_passwd_file] parse a passwd-like file *)
  val of_passwd_file : string -> t list option

  (** [of_passwd_file_exn] parse a passwd-like file *)
  val of_passwd_file_exn : string -> t list
end

val strptime : fmt:string -> string -> Unix.tm
[@@deprecated "[since 2019-07] use Core.Unix.strptime"]

(** The CIDR module moved into Core.Unix *)

(** Simple int wrapper to be explicit about ports. *)
module Inet_port : sig
  type t [@@deriving sexp_of, compare, hash]

  val of_int : int -> t option
  val of_int_exn : int -> t
  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_int : t -> int
  val to_string : t -> string
  val arg_type : t Command.Arg_type.t

  include Comparable.S_plain with type t := t

  module Stable : sig
    module V1 :
      Stable_comparable.V1
      with type t = t
       and type comparator_witness = comparator_witness
  end
end

(* MAC-48 (Ethernet) adddresses *)
module Mac_address : sig
  type t [@@deriving sexp, bin_io]

  val equal : t -> t -> bool

  (* Supports standard "xx:xx:xx:xx:xx:xx", "xx-xx-xx-xx-xx-xx", and cisco
     "xxxx.xxxx.xxxx" representations. *)
  val of_string : string -> t

  (* To standard representation "xx:xx:xx:xx:xx:xx".  Note the hex chars
     will be downcased! *)
  val to_string : t -> string

  (* To cisco representation "xxxx.xxxx.xxxx" *)
  val to_string_cisco : t -> string

  include Hashable.S with type t := t
end

module Quota : sig
  type bytes = private Int63.t [@@deriving sexp]
  type inodes = private Int63.t [@@deriving sexp]

  val bytes : Int63.t -> bytes
  val inodes : Int63.t -> inodes

  type 'units limit =
    { soft : 'units option
    ; hard : 'units option
    ; grace : Time.t option
    }
  [@@deriving sexp]

  type 'units usage = private 'units

  val query
    :  [ `User | `Group ]
    -> id:int
    -> path:string
    -> (bytes limit * bytes usage * inodes limit * inodes usage) Or_error.t

  val set
    :  [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes limit
    -> inodes limit
    -> unit Or_error.t
end

module Mount_entry : sig
  (* see: man 3 getmntent *)
  type t [@@deriving sexp]

  val parse_line : string -> t option Or_error.t
  val fsname : t -> string
  val directory : t -> string
  val fstype : t -> string
  val options : t -> string
  val dump_freq : t -> int option
  val fsck_pass : t -> int option
  val visible_filesystem : t list -> t String.Map.t
end

val terminal_width : int Lazy.t
