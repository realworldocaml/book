open Core
open Async

module Printing_rng = struct
  type g = unit

  let block = 16

  let create ?time:_ () = ()

  let generate ~g:_ _n = assert false

  let reseed ~g:_ data =
    Format.printf "reseeding: %a@.%!" Cstruct.hexdump_pp data

  let accumulate ~g:_ source =
    let print data =
      Format.printf "accumulate: (src: %a) %a@.%!"
        Mirage_crypto_rng.Entropy.pp_source source Cstruct.hexdump_pp data
    in
    `Acc print

  let seeded ~g:_ = true
  let pools = 1
end

module E = Mirage_crypto_rng_async


let main () =
  E.initialize (module Printing_rng);
  Format.printf "entropy sources: %a@,%!"
    (fun ppf -> List.iter ~f:(fun x ->
         Mirage_crypto_rng.Entropy.pp_source ppf x;
         Format.pp_print_space ppf ()))
    (Mirage_crypto_rng.Entropy.sources ());
  don't_wait_for (
    Time_source.after
      (Time_source.wall_clock ())
      (Time_ns.Span.of_int_sec 10)
    >>= fun () ->
    Shutdown.exit 0)

let () = ignore (Scheduler.go_main ~main () : never_returns)
