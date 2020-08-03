(** {b RNG} seeding on {b Unix}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
    On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()] is used instead
    of [getrandom ()]. On Windows 10 or higher, [BCryptGenRandom()] is used
    with the default RNG. Windows 8 or lower are not supported by this library.
*)

(** [initialize ()] will bring the RNG into a working state. *)
val initialize : unit -> unit

(** [getrandom size] returns a buffer of [size] filled with random bytes. *)
val getrandom : int -> Cstruct.t
