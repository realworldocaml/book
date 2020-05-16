(** {b RNG} seeding on {b Unix}.

    This module provides the RNG [Getrandom] which calls [getrandom ()] for each
    [generate] request. On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()]
    is used instead.  On Windows 10 or higher, [BCryptGenRandom()] is used with
    the default RNG.  Windows 8 or lower are not supported by this library.
*)

module Getrandom : Mirage_crypto_rng.Generator

(** [initialize ()] will bring the RNG into a working state.
    [initialize] is idempotent as long as the default generator is unchanged.
    It is harmless to call it several times. *)
val initialize : unit -> unit
