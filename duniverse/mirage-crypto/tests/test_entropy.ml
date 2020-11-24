open Lwt.Infix

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

module E = Mirage_crypto_rng_mirage.Make(Time)(Mclock)

let with_entropy act =
  E.initialize (module Printing_rng) >>= fun () ->
  Format.printf "entropy sources: %a@,%!"
    (fun ppf -> List.iter (fun x ->
         Mirage_crypto_rng.Entropy.pp_source ppf x;
         Format.pp_print_space ppf ()))
    (Mirage_crypto_rng.Entropy.sources ());
  act ()

let () =
  OS.(Main.run (with_entropy (fun () -> Time.sleep_ns 1_000L)))
