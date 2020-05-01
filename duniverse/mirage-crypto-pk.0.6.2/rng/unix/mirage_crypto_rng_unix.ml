
module Getrandom = struct
  type g = unit
  let create () = ()
  let reseed ~g:_ _reseed = ()
  let accumulate ~g:_ = `Acc (fun ~source:_ _buf -> ())
  let seeded ~g:_ = true

  let block = 256

  open Stdlib.Bigarray
  type buffer = (char, int8_unsigned_elt, c_layout) Array1.t
  external getrandom : buffer -> int -> unit = "mc_getrandom"

  let generate ~g:_ size =
    let data = Cstruct.create_unsafe size in
    getrandom data.Cstruct.buffer size;
    data
end

let initialize () =
  Mirage_crypto_rng.generator := Mirage_crypto_rng.create (module Getrandom)
