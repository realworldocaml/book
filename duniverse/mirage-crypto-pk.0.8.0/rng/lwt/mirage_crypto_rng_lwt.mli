(** {b RNG} seeding on {b Lwt}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
*)

(** [initialize ~sleep ()] will bring the RNG into a working state. The argument
    [sleep] is measured in ns (default 1s), and used to sleep between collection
    of entropy from the CPU RNG, every [10 * sleep] getrandom is used to collect
    entropy.
*)
val initialize : ?sleep:int64 -> unit -> unit
