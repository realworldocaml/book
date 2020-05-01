(* Should be thrown be generators and live in Rng, but Rng needs to
 * instantiate Fortuna for the latter can't depend on the former. *)
exception Unseeded_generator
