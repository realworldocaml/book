open Core_kernel

(** Kitchen-sink module for functions shared by multiple parsers. *)

(** If strip is true (default is false) then spaces will be stripped from the beginning
    and end of fields.

    If [skip_lines] is given then that number of lines will be read and discarded from the
    top of the file or Reader.t given.

    If [on_parse_error] is `Raise any lines that fail to parse will raise an exception.
    If `Handle is given the offending line will be passed to the function given, which may
    then indicate that processing should continue or finish.
*)
type ('a, 'b) reader =
  ?strip:bool
  -> ?skip_lines:int
  -> ?on_parse_error:[`Raise | `Handle of string Queue.t -> exn -> [`Continue | `Finish]]
  -> header:'a
  -> 'b

(** Recommendation based on the largest reads and writes observed off a disk or socket. *)
val buffer_size : int

val strip_buffer : Buffer.t -> string

(** [make_emit_field ~strip row buffer] returns a closure which enqueues [buffer] into
    [row], stripping leading and trailing whitespace if [strip = true]. *)
val make_emit_field : strip:bool -> string Queue.t -> Buffer.t -> (unit -> unit) Staged.t

(** [make_emit_row row rows header ~lineno] returns [(`on_eof on_eof, emit_row)]. After
    zero or more calls to [emit_row] and a call to [on_eof], [rows] will contain the
    parsed file.

    Note that [rows] may have fewer elements than the number of calls to [emit_row], if
    [header] specifies that the first [row] is a header line. *)
val make_emit_row
  :  string Queue.t
  -> Row.t Queue.t
  -> Header.t
  -> lineno:int ref
  -> [`on_eof of unit -> unit] * (unit -> unit)
