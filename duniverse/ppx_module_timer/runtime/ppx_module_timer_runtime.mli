open! Base

(** If true, ppx_module_timer records module startup times and reports them on stdout at
    process exit. Controlled by [am_recording_environment_variable]. *)
val am_recording : bool

(** If this environment variable is set (to anything) when this module starts up,
    [am_recording] is set to true.

    Equal to "PPX_MODULE_TIMER".

    If this is set to a valid duration string (see [Duration.format] below), that duration
    is used to override recorded times for each module. This is used to make test output
    deterministic.

    If this is set to "FAKE_MODULES", the entire set of recorded data is overridden with
    fake values. This is used to make test output both deterministic and stable, so that
    changes in external library dependencies do not affect it. The fake data is not
    particularly sensible, for example we are not careful to make the times for
    definitions add up to the time for the enclosing module. *)
val am_recording_environment_variable : string

module Duration : sig
  type t

  val to_nanoseconds : t -> Int63.t
  val of_nanoseconds : Int63.t -> t

  module type Format = sig
    val of_string : string -> t
    val to_string_with_same_unit : t list -> string list
  end

  (** Determines the format of durations when reading [am_recording_environment_variable]
      and when printing results. Defaults to integer nanoseconds with a "ns" suffix.

      [Core_kernel.Time_ns] overrides this to use [Time_ns.Span.to_string] on input and
      [Time_ns.Span.to_string_hum] on output. *)
  val format : (module Format) ref
end

(**/**)

(** {2 For Rewritten Code}

    These definitions are not meant to be called manually. *)

(** If [am_recording], records when the specified module begins its startup effects.
    Raises if a previous module started and has not finished. *)
val record_start : string -> unit

(** If [am_recording], records when the specified module finishes its startup effects.
    Raises if there is no corresponding start time. *)
val record_until : string -> unit

(** If [am_recording], records when the specified definition begins its startup effects.
    Raises if a previous definition started and has not finished, or if it is not called
    during startup of an enclosing module. *)
val record_definition_start : string -> unit

(** If [am_recording], records when the specified definition finishes its startup effects.
    Raises if there is no corresponding start time, or if it is not called during startup
    of an enclosing module. *)
val record_definition_until : string -> unit

(** Duplicate of [Pervasives.__MODULE__]. *)
external __MODULE__ : string = "%loc_MODULE"
