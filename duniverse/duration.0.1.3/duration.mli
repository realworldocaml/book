(** Duration - conversions to various time units

    A duration is represented in nanoseconds as an unsigned 64 bit integer.
    This has a range of up to 584 years, or 213503 days, or 5124095 hours, or
    307445734 minutes, or 18446744073 seconds, or 18446744073709 milliseconds,
    or 18446744073709549 milliseconds.

    All functions converting to [t] raise [Invalid_argument] on out of bound
    or negative input.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}
*)

(** The type for a duration, exposed as an int64 to provide interoperability. *)
type t = int64

(** [pp ppf t] prints the duration. *)
val pp : Format.formatter -> t -> unit

(** [of_us us] are the microseconds in nanoseconds. *)
val of_us : int -> t

(** [of_ms ms] are the milliseconds in nanoseconds. *)
val of_ms : int -> t

(** [of_sec s] are the seconds in nanoseconds. *)
val of_sec : int -> t

(** [of_min m] are the minutes in nanoseconds. *)
val of_min : int -> t

(** [of_hour h] are the hours in nanoseconds. *)
val of_hour : int -> t

(** [of_day d] are the days in nanoseconds. *)
val of_day : int -> t

(** [of_year y] are the years in nanoseconds. *)
val of_year : int -> t

(** [of_f f] is the floating point seconds in nanoseconds. *)
val of_f : float -> t


(** [to_us t] are the microseconds of [t]. *)
val to_us : t -> int

(** [to_ms t] are the milliseconds of [t]. *)
val to_ms : t -> int

(** [to_sec t] are the seconds of [t]. *)
val to_sec : t -> int

(** [to_min t] are the minutes of [t]. *)
val to_min : t -> int

(** [to_hour t] are the hours of [t]. *)
val to_hour : t -> int

(** [to_day t] are the days of [t]. *)
val to_day : t -> int

(** [to_year t] are the years of [t]. *)
val to_year : t -> int

(** [to_f t] is the floating point representation of [t]. *)
val to_f : t -> float

