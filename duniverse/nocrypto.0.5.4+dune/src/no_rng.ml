open No_uncommon
module Numeric = No_numeric

type bits = int

exception Unseeded_generator = Boot.Unseeded_generator

module S = struct

  module type Generator = sig
    type g
    val block      : int
    val create     : unit -> g
    val generate   : g:g -> int -> Cstruct.t
    val reseed     : g:g -> Cstruct.t -> unit
    val accumulate : g:g -> [`Acc of source:int -> Cstruct.t -> unit]
    val seeded     : g:g -> bool
  end

  type 'a generator = (module Generator with type g = 'a)
  type g = Generator : ('a * bool * 'a generator) -> g

  module type N = sig
    type t
    val gen      : ?g:g -> t -> t
    val gen_r    : ?g:g -> t -> t -> t
    val gen_bits : ?g:g -> ?msb:int -> int -> t
  end
end

type g = S.g

let create (type a) ?g ?seed ?(strict=false) (m : a S.generator) =
  let module M = (val m) in
  let g = Option.get_or M.create () g in
  seed |> Option.cond ~f:(M.reseed ~g) ;
  S.Generator (g, strict, m)

let generator = ref (create (module No_fortuna))

let get = function Some g -> g | None -> !generator

let generate ?(g = !generator) n =
  let S.Generator (g, _, m) = g in let module M = (val m) in M.generate ~g n

let reseed ?(g = !generator) cs =
  let S.Generator (g, _, m) = g in let module M = (val m) in M.reseed ~g cs

let accumulate g =
  let S.Generator (g, _, m) = get g in let module M = (val m) in M.accumulate ~g

let seeded g =
  let S.Generator (g, _, m) = get g in let module M = (val m) in M.seeded ~g

let block g =
  let S.Generator (_, _, m) = get g in let module M = (val m) in M.block

let strict g =
  let S.Generator (_, s, _) = get g in s

module Make_N (N : Numeric.S) = struct

  type t = N.t

  let gen ?g n =
    if n < N.one then invalid_arg "Rng.gen: non-positive: %a" N.pp_print n;

    let bs     = block g in
    let bits   = N.(bits (pred n)) in
    let octets = bits // 8 in
    let batch  = if strict g then octets else 2 * octets // bs * bs in

    let rec attempt cs =
      if cs.Cstruct.len >= octets then
        let x = N.of_cstruct_be ~bits cs in
        if x < n then x else attempt (Cstruct.shift cs octets)
      else attempt (generate ?g batch) in
    attempt (generate ?g batch)

  let rec gen_r ?g a b =
    if strict g then
      let x = gen ?g b in if x < a then gen_r ?g a b else x
    else N.(a + gen ?g (b - a))

  let gen_bits ?g ?(msb = 0) bits =
    let res = generate ?g (bits // 8) in
    Cs.set_msb msb res ;
    N.of_cstruct_be ~bits res

end

module Int   = Make_N (Numeric.Int  )
module Int32 = Make_N (Numeric.Int32)
module Int64 = Make_N (Numeric.Int64)
module ZN    = Make_N (Numeric.Z    )


(* Invalid combinations of ~bits and ~msb will loop forever, but there is no
 * way to quickly determine upfront whether there are any primes in the
 * interval.
 * XXX Probability is distributed as inter-prime gaps. So?
 *)
let rec prime ?g ?(msb = 1) bits =
  let p = Z.(nextprime @@ ZN.gen_bits ?g ~msb bits) in
  if p < Z.(one lsl bits) then p else prime ?g ~msb bits

(* XXX Add ~msb param for p? *)
let rec safe_prime ?g bits =
  let q = prime ?g ~msb:1 (bits - 1) in
  let p = Z.(q * ~$2 + ~$1) in
  if Numeric.pseudoprime p then (q, p) else safe_prime ?g bits

(*     |+ Pocklington primality test specialized for `a = 2`. +|
  if Z.(gcd (of_int 3) p = one) then (q, p)
  else safe_prime ?g ~bits *)

module Z = ZN


module Generators = struct

  module Fortuna = No_fortuna

  module Hmac_drgb = No_hmac_drgb

  module Null = struct

    type g = Cstruct.t ref

    let block = 1

    let create () = ref Cs.empty

    let generate ~g n =
      try
        let (a, b) = Cstruct.split !g n in ( g := b ; a )
      with Invalid_argument _ -> raise Unseeded_generator

    let reseed ~g cs = g := Cs.(!g <+> cs)

    let seeded ~g = Cstruct.len !g > 0

    let accumulate ~g = `Acc (fun ~source:_ -> reseed ~g)
  end

end

type buffer = Cstruct.t
