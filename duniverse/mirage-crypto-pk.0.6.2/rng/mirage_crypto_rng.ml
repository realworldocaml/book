open Mirage_crypto.Uncommon

type bits = int

exception Unseeded_generator = Boot.Unseeded_generator

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

module Fortuna = Fortuna

module Hmac_drbg = Hmac_drbg

module Null = struct

  type g = Cstruct.t ref

  let block = 1

  let create () = ref Cstruct.empty

  let generate ~g n =
    try
      let (a, b) = Cstruct.split !g n in ( g := b ; a )
    with Invalid_argument _ -> raise Unseeded_generator

  let reseed ~g cs = g := Cs.(!g <+> cs)

  let seeded ~g = Cstruct.len !g > 0

  let accumulate ~g = `Acc (fun ~source:_ -> reseed ~g)
end

let create (type a) ?g ?seed ?(strict=false) (m : a generator) =
  let module M = (val m) in
  let g = Option.get_or M.create () g in
  seed |> Option.cond ~f:(M.reseed ~g) ;
  Generator (g, strict, m)

let generator = ref (create (module Null))

let get = function Some g -> g | None -> !generator

let generate ?(g = !generator) n =
  let Generator (g, _, m) = g in let module M = (val m) in M.generate ~g n

let reseed ?(g = !generator) cs =
  let Generator (g, _, m) = g in let module M = (val m) in M.reseed ~g cs

let accumulate g =
  let Generator (g, _, m) = get g in let module M = (val m) in M.accumulate ~g

let seeded g =
  let Generator (g, _, m) = get g in let module M = (val m) in M.seeded ~g

let block g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.block

let strict g =
  let Generator (_, s, _) = get g in s
