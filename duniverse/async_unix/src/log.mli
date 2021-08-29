(** A library for general logging.

    Although this module is fully Async-safe it exposes almost no Deferreds.  This is
    partially a design choice to minimize the impact of logging in code, and partially the
    result of organic design (i.e., older versions of this interface did the same thing).

    A (limited) [Blocking] module is supplied to accommodate the portion of a program that
    runs outside of Async. *)

open! Core
open! Import

module Level : sig
  (** Describes both the level of a log and the level of a message sent to a log.  There
      is an ordering to levels (`Debug < `Info < `Error), and a log set to a level will
      never display messages at a lower log level.

      Messages without a level are treated as `Info. *)
  type t =
    [ `Debug
    | `Info (** default level *)
    | `Error
    ]
  [@@deriving bin_io, compare, sexp]

  include Stringable with type t := t

  val arg : t Command.Spec.Arg_type.t
  val as_or_more_verbose_than : log_level:t -> msg_level:t option -> bool

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
  end
end

module Message : sig
  type t [@@deriving sexp_of]

  val create
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?time_source:Synchronous_time_source.t
    (** [time_source] is used to provide a default time, if none is specified *)
    -> ?tags:(string * string) list
    -> [ `String of string | `Sexp of Sexp.t ]
    -> t

  val time : t -> Time.t
  val message : t -> string
  val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
  val level : t -> Level.t option
  val set_level : t -> Level.t option -> t
  val tags : t -> (string * string) list
  val add_tags : t -> (string * string) list -> t

  module Stable : sig
    module V0 : sig
      (** [V0.bin_t] is the [Message.bin_t] in jane-111.18 and before *)
      type nonrec t = t [@@deriving bin_io, sexp]
    end

    module V2 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
  end
end

module Rotation : sig
  (** Description of boundaries for file rotation.

      If all fields are [None] the file will never be rotated.  Any field set to [Some]
      will cause rotation to happen when that boundary is crossed.  Multiple boundaries
      may be set.  Log rotation always causes incrementing rotation conditions (e.g.,
      size) to reset.

      The condition [keep] is special and does not follow the rules above.  When a log is
      rotated, [keep] is examined and logs that do not fall under its instructions are
      deleted.  This deletion takes place on rotation only, and so may not happen.  The
      meaning of keep options are:

      - [`All] -- never delete
      - [`Newer_than span] -- delete files with a timestamp older than [Time.sub (Time.now
        ()) span].  This normally means keeping files that contain at least one message
        logged within [span].  If [span] is short enough this option can delete a
        just-rotated file.
      - [`At_least i] -- keep the [i] most recent files

      Log rotation does not support symlinks, and you're encouraged to avoid them in
      production applications. Issues with symlinks:

      - You can't tail symlinks without being careful (e.g., you must remember to pass
        [-F] to [`tail`]).
      - Symlinks are hard to reason about when the program crashes, especially on
        startup (i.e., is the symlink pointing me at the right log file?).
      - Atomicity is hard.
      - Symlinks encourage tailing, which is a bad way to communicate information.
      - They complicate archiving processes (the symlink must be skipped). *)
  type t [@@deriving sexp_of]

  module type Id_intf = sig
    type t

    val create : ?time_source:Synchronous_time_source.t -> Time.Zone.t -> t

    (** For any rotation scheme that renames logs on rotation, this defines how to do
        the renaming. *)
    val rotate_one : t -> t

    val to_string_opt : t -> string option
    val of_string_opt : string option -> t option
    val cmp_newest_first : t -> t -> int
  end


  val create
    :  ?messages:int
    -> ?size:Byte_units.t
    -> ?time:Time.Ofday.t
    -> ?zone:Time.Zone.t
    -> keep:[ `All | `Newer_than of Time.Span.t | `At_least of int ]
    -> naming_scheme:[ `Numbered
                     | `Timestamped
                     | `Dated
                     | `User_defined of (module Id_intf)
                     ]
    -> unit
    -> t

  (** Sane defaults for log rotation.

      Writes dated log files. Files are rotated every time the day changes in the given
      zone (uses the machine's zone by default). If the dated log file already exists,
      it's appended to.

      Logs are never deleted. Best practice is to have an external mechanism archive old
      logs for long-term storage. *)
  val default : ?zone:Time.Zone.t -> unit -> t
end

module Output : sig
  module Format : sig

    type machine_readable =
      [ `Sexp
      | `Sexp_hum
      | `Bin_prot
      ]
    [@@deriving sexp]

    type t =
      [ machine_readable
      | `Text
      ]
    [@@deriving sexp]

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving sexp]
      end
    end
  end

  type t


  (** [create f] returns a [t], given a function that actually performs the final output
      work. It is the responsibility of the write function to contain all state, and to
      clean up after itself when it is garbage collected (which may require a finalizer).
      The function should avoid modifying the contents of the queue; it's reused for each
      [Output.t].

      The "stock" output modules support a sexp and bin_prot output format, and other
      output modules should make efforts to support them as well where it is
      meaningful/appropriate to do so.

      [flush] should return a deferred that is fulfilled only when all previously written
      messages are durable (e.g., on disk, out on the network, etc.).  It is automatically
      called on shutdown by [Log], but isn't automatically called at any other time.  It
      can be called manually by calling [Log.flushed t].

      The [unit Deferred] returned by the function provides an opportunity for pushback if
      that is important.  Only one batch of messages will be "in flight" at any time based
      on this deferred.

      An optional [rotate] function may be given which will be called when [Log.rotate t]
      is called while this output is in effect.  This is useful for programs that want
      very precise control over rotation.

      If [close] is provided it will be called when the log falls out of scope. (Note that
      it is not called directly, even if you close a log which is using this output,
      because outputs are sometimes reused.)  *)
  val create
    :  ?rotate:(unit -> unit Deferred.t)
    -> ?close:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message.t Queue.t -> unit Deferred.t)
    -> t

  (** [stdout] defaults to [format=`Text] *)
  val stdout : ?format:Format.t -> unit -> t

  (** [stderr] defaults to [format=`Text] *)
  val stderr : ?format:Format.t -> unit -> t

  val writer : Format.t -> Writer.t -> t

  (** The [perm] argument is passed through to [Writer.open_file], and so has the default
      behavior described there. *)
  val file : ?perm:Unix.file_perm -> Format.t -> filename:string -> t

  val rotating_file
    :  ?perm:Unix.file_perm
    -> ?time_source:Synchronous_time_source.t
    -> Format.t
    -> basename:string
    -> Rotation.t
    -> t

  (** Returns a tail of the filenames. When [rotate] is called, the previous filename is
      put on the tail *)
  val rotating_file_with_tail
    :  ?perm:Unix.file_perm
    -> ?time_source:Synchronous_time_source.t
    -> Format.t
    -> basename:string
    -> Rotation.t
    -> t * string Tail.t

  (** [filter_to_level] wraps an output and gives you a new output which only
      logs messages which are as/more verbose than [level].

      This functionality is intended for when you have multiple outputs being displayed in
      different places, and they need to be at different levels.

      If you have one output (or multiple outputs all at the same level), it is better to
      set the [Log.t]'s output directly with [set_level], which is equivalent and more
      efficient. *)
  val filter_to_level : t -> level:Level.t -> t

  (** See {!Log_extended} for syslog and colorized console output. *)
end

module Blocking : sig
  (** Async programs often have a non-Async portion that runs before the scheduler begins
      to capture command line options, do setup, read configs, etc.  This module provides
      limited global logging functions to be used during that period.  Calling these
      functions after the scheduler has started will raise an exception.  They otherwise
      behave similarly to the logging functions in the Async world.

      There are more detailed comments for the API below near the non-blocking
      signatures. *)

  module Output : sig
    type t

    val stdout : t
    val stderr : t

    (** See {!Log_extended} for syslog and colorized console output. *)

    val create : (Message.t -> unit) -> t
  end

  val level : unit -> Level.t
  val set_level : Level.t -> unit
  val set_output : Output.t -> unit
  val set_time_source : Synchronous_time_source.t -> unit
  val set_transform : (Message.t -> Message.t) option -> unit

  val raw
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val info
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val error
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val debug
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val raw_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val info_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val error_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val debug_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit

  val sexp
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> unit

  val surround_s
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> (unit -> 'a)
    -> 'a

  val surroundf
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, (unit -> 'b) -> 'b) format4
    -> 'a
end

type t [@@deriving sexp_of]

(** An interface for singleton logs. *)
module type Global_intf = sig
  val log : t Lazy.t
  val level : unit -> Level.t
  val set_level : Level.t -> unit
  val set_output : Output.t list -> unit
  val get_output : unit -> Output.t list
  val set_on_error : [ `Raise | `Call of Error.t -> unit ] -> unit
  val get_time_source : unit -> Synchronous_time_source.t
  val set_time_source : Synchronous_time_source.t -> unit
  val get_transform : unit -> (Message.t -> Message.t) option
  val set_transform : (Message.t -> Message.t) option -> unit
  val would_log : Level.t option -> bool
  val set_level_via_param : unit -> unit Command.Param.t

  (** Functions that operate on a given log.  In this case they operate on a single log
      global to the module. *)

  val raw
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val info
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val error
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val debug
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val flushed : unit -> unit Deferred.t
  val rotate : unit -> unit Deferred.t

  val printf
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val raw_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val info_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val error_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val debug_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit

  val sexp
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> unit

  val string
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> string
    -> unit

  val message : Message.t -> unit

  val surround_s
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> (unit -> 'a Deferred.t)
    -> 'a Deferred.t

  val surroundf
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
    -> 'a
end

(** This functor can be called to generate "singleton" logging modules. *)
module Make_global () : Global_intf

(** Programs that want simplistic single-channel logging can open this module.  It
    provides a global logging facility to a single output type at a single level.  More
    nuanced logging can be had by using the functions that operate on a distinct [Log.t]
    type. *)
module Global : Global_intf

(** Sets the log level via a flag, if provided. *)
val set_level_via_param : t -> unit Command.Param.t

(** Messages sent at a level less than the current level will not be output. *)
val set_level : t -> Level.t -> unit

(** Returns the last level passed to [set_level], which will be the log level
    checked as a threshold against the level of the next message sent. *)
val level : t -> Level.t

(** Changes the output type of the log, which can be useful when daemonizing.
    The new output type will be applied to all subsequent messages. *)
val set_output : t -> Output.t list -> unit

val get_output : t -> Output.t list

(** Changes the time source of the log, which controls the default timestamp on
    messages. *)
val get_time_source : t -> Synchronous_time_source.t

val set_time_source : t -> Synchronous_time_source.t -> unit

(** Changes the [transform] function within log.  This allows you to *synchronously*
    change things about the message at the time that they were written.

    The transform function *will not* be called if the initial message is of a level that
    would not currently be logged.

    The transform function *will* be called if even if there are no log outputs. *)
val get_transform : t -> (Message.t -> Message.t) option

val set_transform : t -> (Message.t -> Message.t) option -> unit


(** If [`Raise] is given, then background errors raised by logging will be raised to the
    monitor that was in scope when [create] was called.  Errors can be redirected anywhere
    by providing [`Call f]. *)
val set_on_error : t -> [ `Raise | `Call of Error.t -> unit ] -> unit

(** Any call that writes to a log after [close] is called will raise. *)
val close : t -> unit Deferred.t

(** Returns true if [close] has been called. *)
val is_closed : t -> bool

(** Returns a [Deferred.t] that is fulfilled when the last message delivered to [t] before
    the call to [flushed] is out the door. *)
val flushed : t -> unit Deferred.t

(** Informs the current [Output]s to rotate if possible. *)
val rotate : t -> unit Deferred.t

(** Creates a new log.  See [set_level], [set_on_error], [set_output],
    [set_time_source], and [set_transform] for more. *)
val create
  :  level:Level.t
  -> output:Output.t list
  -> on_error:[ `Raise | `Call of Error.t -> unit ]
  -> ?time_source:Synchronous_time_source.t
  -> ?transform:(Message.t -> Message.t)
  -> unit
  -> t

(** Printf-like logging for messages at each log level or raw (no level) messages. Raw
    messages still include a timestamp. *)

val raw
  :  ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val debug
  :  ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val info
  :  ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val error
  :  ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Generalized printf-style logging. *)
val printf
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Sexp logging for messages at each log level or raw (no level) messages. Raw messages
    still include a timestamp *)

val raw_s : ?time:Time.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val info_s : ?time:Time.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val error_s : ?time:Time.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val debug_s : ?time:Time.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit

(** Generalized sexp-style logging. *)
val sexp
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> unit

(** Log a string directly. *)
val string
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> string
  -> unit

(** Log a pre-created message. *)
val message : t -> Message.t -> unit

(** [surround t message f] logs [message] and a UUID once before calling [f] and again
    after [f] returns or raises. If [f] raises, the second message will include the
    exception, and [surround] itself will re-raise the exception tagged with [message]. As
    usual, the logging happens only if [level] exceeds the minimum level of [t]. *)
val surround_s
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

val surroundf
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
  -> 'a

(** [would_log] returns true if a message at the given log level would be logged if sent
    immediately.

    This will return [false] if there are no outputs for the log, unless there is
    a [transform] set. *)
val would_log
  :  t
  -> Level.t option
  -> bool

module Reader : sig
  (** [pipe format filename] returns a pipe of all the messages in the log.  Errors
      encountered when opening or reading the file will be thrown as exceptions into the
      monitor current at the time [pipe] is called. *)
  val pipe : [< Output.Format.machine_readable ] -> string -> Message.t Pipe.Reader.t

  val pipe_of_reader
    :  [< Output.Format.machine_readable ]
    -> Reader.t
    -> Message.t Pipe.Reader.t

  module Expert : sig
    (** [read_one format reader] reads a single log message from the reader, advancing the
        position of the reader to the next log entry. *)
    val read_one
      :  [< Output.Format.machine_readable ]
      -> Reader.t
      -> Message.t Reader.Read_result.t Deferred.t
  end
end

module For_testing : sig
  (** [create_output ~map_output] creates a [Log.Output.t] which will print only
      [Message.message] to stdout, discarding any information about tags, levels, or
      timestamps.

      [map_output] will be applied to each string before printing, and is expected
      to be used to replace portions of output or identify which log a message came from.

      This function is best used with existing Log.ts, e.g. to replace Log.Global's
      outputs in expect tests. If you just want a full Log.t, see [create] below. *)
  val create_output : map_output:(string -> string) -> Output.t

  (** [create_log ~map_output level] creates a [Log.t] with its level set to [level] using
      the output returned by [create_output], and an [on_error] value of `Raise. *)
  val create : map_output:(string -> string) -> Level.t -> t
end

(**/**)

module Private : sig
  module Message : sig
    type t = Message.t [@@deriving of_sexp]

    include Equal.S with type t := t

    val to_write_only_text : ?zone:Time.Zone.t -> t -> string

    module Stable : sig
      module Version : sig
        type t [@@deriving of_sexp]
      end

      module V0 : sig
        type t = Message.t [@@deriving sexp_of]
      end

      module V2 : sig
        type t = Message.t [@@deriving sexp_of]
      end
    end
  end
end
