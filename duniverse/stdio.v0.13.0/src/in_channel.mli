(** An input channel for doing blocking reads from input sources like files and sockets.

    Note that an [In_channel.t] is a custom block with a finalizer, and so is allocated
    directly to the major heap. Creating a lot of in_channels can result in many major
    collections and poor performance.

    Note that this is simply another interface on the [in_channel] type in the OCaml
    standard library.
*)

open! Base
open! Import

type t = Caml.in_channel

include Equal.S with type t := t

val stdin : t

(** Channels are opened in binary mode iff [binary] is true.  This only has an effect on
    Windows.
*)

val create : ?binary:bool (** defaults to [true] *) -> string -> t

(** [with_file ~f fname] executes [~f] on the open channel from
    [fname], and closes it afterwards. *)
val with_file : ?binary:bool (** defaults to [true] *) -> string -> f:(t -> 'a) -> 'a

(** [close t] closes [t], or does nothing if [t] is already closed, and may raise an
    exception. *)
val close : t -> unit

val input : t -> buf:bytes -> pos:int -> len:int -> int
val really_input     : t -> buf:bytes -> pos:int -> len:int -> unit option

(** Same as [Pervasives.really_input], for backwards compatibility *)
val really_input_exn : t -> buf:bytes -> pos:int -> len:int -> unit

(** Read one character from the given input channel.  Return [None] if there are no more
    characters to read. *)
val input_char : t -> char option

(** Same as [input_char], but return the 8-bit integer representing the character. Return
    [None] if an end of file was reached. *)
val input_byte : t -> int option

(** Read an integer encoded in binary format (4 bytes, big-endian) from the given input
    channel.  See {!Pervasives.output_binary_int}.  Return [None] if an end of file was
    reached while reading the integer. *)
val input_binary_int : t -> int option

(** Ocaml's built-in marshal format *)
val unsafe_input_value : t -> _ option

(** [input_buffer t buf ~len] reads at most [len] characters from the input channel [t]
    and stores them at the end of buffer [buf].  Return [None] if the channel contains
    fewer than [len] characters. In this case, the characters are still added to the
    buffer, so as to avoid loss of data. *)
val input_buffer : t -> Buffer.t -> len:int -> unit option

val input_all : t -> string

(** [input_line ?fix_win_eol t] reads a line from [t] and returns it, without
    the newline ("\n") character at the end, and, if [fix_win_eol] the trailing
    "\r\n" is dropped.
*)
val input_line     : ?fix_win_eol:bool (** defaults to [true] *) -> t -> string option
val input_line_exn : ?fix_win_eol:bool (** defaults to [true] *) -> t -> string

(** [fold_lines ?fix_win_eol t ~init ~f] folds over the lines read from [t]
    using [input_line].  Lines are provided to [f] in the order they are
    found in the file. *)
val fold_lines
  :  ?fix_win_eol:bool (** defaults to [true] *)
  -> t
  -> init:'a
  -> f:('a -> string -> 'a)
  -> 'a

(** Completely reads an input channel and returns the results as a list of
    strings. Each line in one string. *)
val input_lines : ?fix_win_eol:bool (** defaults to [true] *) -> t -> string list

(** [iter_lines ?fix_win_eol t ~f] applies [f] to each line read from [t] using
    [input_line]. *)
val iter_lines
  :  ?fix_win_eol:bool (** defaults to [true] *)
  -> t
  -> f:(string -> unit)
  -> unit

(** This works only for regular files.  On files of other kinds, the behavior is
    unspecified. *)
val seek : t -> int64 -> unit

val pos : t -> int64

(** Return the size (number of characters) of the regular file on which the given channel
    is opened.  If the channel is opened on a file that is not a regular file, the result
    is meaningless.  The returned size does not take into account the end-of-line
    translations that can be performed when reading from a channel opened in text mode. *)
val length : t -> int64

(** same as [Pervasives.set_binary_mode_in], only applicable for Windows or Cygwin, no-op
    otherwise *)
val set_binary_mode : t -> bool -> unit

(** [read_lines filename] reads the full contents of file and returns it as a list of
    lines, closing the file when it's done.  It's the equivalent of [with_file fname
    ~f:input_lines] *)
val read_lines : ?fix_win_eol:bool -> string -> string list

(** [read_all filename] reads the full contents of file and returns it as a single
    string, closing the file when it's done.  It's the equivalent of [with_file fname
    ~f:input_all] *)
val read_all : string -> string
