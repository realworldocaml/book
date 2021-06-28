(** An output channel for doing blocking writes to destinations like files and sockets.

    Note that an [Out_channel.t] is a custom block with a finalizer, and so is allocated
    directly to the major heap. Creating a lot of out_channels can result in many major
    collections and poor performance.

    Note that this is simply another interface on the [out_channel] type in the OCaml
    standard library.

    As for the output functions in the standard library, all the functions in this module,
    unless otherwise specified,  can raise [Sys_error] when the system calls they invoke
    fail.
*)

open! Base
open! Import

type t = Caml.out_channel [@@deriving_inline sexp_of]
include
  sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end[@@ocaml.doc "@inline"]
[@@@end]

include Equal.S with type t := t

val stdout : t
val stderr : t

type 'a with_create_args =
  ?binary:bool            (** defaults to [true] *)
  -> ?append:bool         (** defaults to [false] *)
  -> ?fail_if_exists:bool (** defaults to [false] *)
  -> ?perm:int
  -> 'a


val create : (string -> t) with_create_args
val with_file : (string -> f:(t -> 'a) -> 'a) with_create_args


(** [close t] flushes and closes [t], and may raise an exception.  [close] returns () and
    does not raise if [t] is already closed.  [close] raises an exception if the close()
    system call on the underlying file descriptor fails (i.e. returns -1), which would
    happen in the following cases:

    EBADF -- this would happen if someone else did close() system call on the underlying
    fd, which I would think a rare event.

    EINTR -- would happen if the system call was interrupted by a signal, which would be
    rare.  Also, I think we should probably just catch EINTR and re-attempt the close.
    Unfortunately, we can't do that in OCaml because the OCaml library marks the
    out_channel as closed even if the close syscall fails, so a subsequent call
    [close_out_channel] will be a no-op.  This should really be fixed in the OCaml library
    C code, having it restart the close() syscall on EINTR.  I put a couple CRs in
    [fixed_close_channel], our rework of OCaml's [caml_ml_close_channel],

    EIO -- I don't recall seeing this.  I think it's rare.

    See "man 2 close" for details.
*)
val close : t -> unit

(** [close_no_err] tries to flush and close [t]. It does not raise.*)
val close_no_err : t -> unit

val set_binary_mode : t -> bool -> unit

val flush : t -> unit

val output : t -> buf:bytes -> pos:int -> len:int -> unit
val output_string : t -> string -> unit
val output_substring : t -> buf:string -> pos:int -> len:int -> unit
val output_bytes : t -> Bytes.t -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_binary_int : t -> int -> unit
val output_buffer : t -> Buffer.t -> unit
val output_value : t -> _ -> unit  (** OCaml's internal Marshal format *)

val newline : t -> unit

(** Outputs a list of lines, each terminated by a newline character *)
val output_lines : t -> string list -> unit

(** Formatted printing to an out channel.  This is the same as [Printf.sprintf] except
    that it outputs to [t] instead of returning a string.  Similarly, the function
    arguments corresponding to conversions specifications such as [%a] or [%t] takes [t]
    as argument and must print to it instead of returning a string. *)
val fprintf : t -> ('a, t, unit) format -> 'a

(** [printf fmt] is the same as [fprintf stdout fmt] *)
val printf : ('a, t, unit) format -> 'a

(** [print_s sexp] outputs [sexp] on [stdout], by default using [Sexp.to_string_hum],
    or, with [~mach:()], [Sexp.to_string_mach]. *)
val print_s : ?mach : unit -> Sexp.t -> unit

(** [eprint_s sexp] outputs [sexp] on [stderr], by default using [Sexp.to_string_hum],
    or, with [~mach:()], [Sexp.to_string_mach]. *)
val eprint_s : ?mach : unit -> Sexp.t -> unit

(** [eprintf fmt] is the same as [fprintf stderr fmt] *)
val eprintf : ('a, t, unit) format -> 'a

(** [kfprintf k t fmt] is the same as [fprintf t fmt], but instead of returning
    immediately, passes the out channel to [k] at the end of printing. *)
val kfprintf : (t -> 'a) -> t -> ('b, t, unit, 'a) format4 -> 'b

(** [print_string s] = [output_string stdout s] *)
val print_string : string -> unit

(** [print_endline str] outputs [str] to [stdout] followed by a newline then flushes
    [stdout] *)
val print_endline : string -> unit

(** [prerr_endline str] outputs [str] to [stderr] followed by a newline then flushes
    [stderr] *)
val prerr_endline : string -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64

(** The first argument of these is the file name to write to. *)
val write_lines : string -> string list -> unit
val write_all : string -> data:string -> unit



