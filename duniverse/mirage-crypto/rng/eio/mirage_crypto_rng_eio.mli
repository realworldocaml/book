(** {b RNG} seeding on {b Eio backends}.

    This module initializes a given random number generator with [getrandom()] and a CPU RNG.
    [Eio.Stdenv.secure_random] is used as the [getrandom()] implementation.
*)

type env = <
  clock: Eio.Time.clock;
  secure_random: Eio.Flow.source;
  >

(** [run ~g ~sleep gen env fn] will bring the RNG into a working state. The argument
    [sleep] is measured in ns (default 1s), and is used to sleep between collection
    of entropy from the CPU RNG. Every [10 * sleep] getrandom is used to collect
    entropy.

    {b Note} In a multi-domain setting [run] ensures that entropy collection, feeding
    and RNG setup are limited to one domain.

    [fn] is the main function that will have access to a running RNG.

    [g] [gen] denotes random number generator scheme to be used, eg [Mirage_crypto_rng.Fortuna].

    [[
      open Mirage_crypto_rng

      let () =
        Eio_main.run @@ fun env ->
        Mirage_crypto_rng_eio.run (module Fortuna) env @@ fun () ->
        let random_num = Mirage_crypto_rng.generate 32 in
        Printf.printf "Random number: %S%!" (Cstruct.to_string random_num)
    ]]
*)
val run
  :  ?g:'a
  -> ?sleep:int64
  -> 'a Mirage_crypto_rng.generator
  -> <env; ..>
  -> (unit -> 'b) -> 'b
