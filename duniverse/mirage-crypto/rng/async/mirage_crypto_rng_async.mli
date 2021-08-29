open! Core
open! Async

(** {b RNG} seeding on {b Async}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
*)

(** [initialize ~sleep generator] will bring the RNG into a working state. The
    argument [sleep] is measured in ns (default 1s), and used to sleep between
    collection of entropy from the CPU RNG, every [10 * sleep] getrandom is used
    to collect entropy.  *)
val initialize
  :  ?g:'a
  -> ?time_source:Synchronous_time_source.t
  -> ?sleep:Time_ns.Span.t
  -> 'a Mirage_crypto_rng.generator
  -> unit
