open No_uncommon
module Rng = No_rng
module Numeric = No_numeric
module Hash = No_hash
open Sexplib.Conv

type bits = int

type pub = { p : Z.t ; q : Z.t ; gg : Z.t ; y : Z.t }
[@@deriving sexp]

type priv = { p : Z.t ; q : Z.t ; gg : Z.t ; x : Z.t ; y : Z.t }
[@@deriving sexp]

let pub_of_priv { p; q; gg; y; _ } = { p; q; gg; y }

type keysize = [ `Fips1024 | `Fips2048 | `Fips3072 | `Exactly of int * int ]

let expand_size = function
  | `Fips1024       -> (1024, 160)
  | `Fips2048       -> (2048, 256)
  | `Fips3072       -> (3072, 256)
  | `Exactly (l, n) ->
      if 3 <= l && 2 <= n then (l, n) else
        invalid_arg "Dsa.generate: bits: `Exactly (%d, %d)" l n

type mask = [ `No | `Yes | `Yes_with of Rng.g ]

let expand_mask = function
  | `No         -> `No
  | `Yes        -> `Yes None
  | `Yes_with g -> `Yes (Some g)

(*
 * FIPS.186-4-style derivation:
 * - p and q are derived using a method numerically like the one described in
 *   A.1.1.2, adapted to use the native rng.
 * - g is derived as per A.2.1.
 *)
let params ?g size =
  let (l, n) = expand_size size in
  let q = Rng.prime ?g ~msb:1 n in
  let p =
    let q_q  = Z.(q * ~$2) in
    until Numeric.pseudoprime @@ fun () ->
      let x = Rng.Z.gen_bits ?g ~msb:1 l in
      Z.(x - (x mod q_q) + one) in
  let gg =
    let e = Z.(pred p / q) in
    until ((<>) Z.one) @@ fun () ->
      let h = Rng.Z.gen_r ?g Z.(~$2) Z.(pred p) in
      Z.(powm h e p) in
  (p, q, gg)

let generate ?g size =
  let (p, q, gg) = params ?g size in
  let x = Rng.Z.gen_r ?g Z.one q in
  let y = Z.(powm gg x p) in
  { p; q; gg; x; y }


module K_gen (H : Hash.S) = struct

  let drgb : 'a Rng.S.generator =
    let module M = Rng.Generators.Hmac_drgb.Make (H) in (module M)

  let z_gen ~key:{ q; x; _ } z =
    let repr = Numeric.Z.(to_cstruct_be ~size:(bits q // 8)) in
    let g    = Rng.create ~strict:true drgb in
    Rng.reseed ~g Cs.(repr x <+> repr Z.(z mod q));
    Rng.Z.gen_r ~g Z.one q

  let generate ~key cs =
    z_gen ~key Numeric.Z.(of_cstruct_be ~bits:(bits key.q) cs)
end

module K_gen_sha256 = K_gen (Hash.SHA256)

let rec sign_z ?(mask = `Yes) ?k:k0 ~key:({ p; q; gg; x; _ } as key) z =
  let k  = match k0 with Some k -> k | None -> K_gen_sha256.z_gen ~key z in
  let k' = Z.invert k q
  and r  = match expand_mask mask with
    | `No    -> Z.(powm gg k p mod q)
    | `Yes g ->
        let m  = Rng.Z.gen_r ?g Z.one q in
        let m' = Z.invert m q in
        Z.(powm (powm gg m p) (m' * k mod q) p mod q) in
  let s = Z.(k' * (z + x * r) mod q) in
  if r = Z.zero || s = Z.zero then sign_z ~mask ?k:k0 ~key z else (r, s)

let verify_z ~key:({ p; q; gg; y }: pub ) (r, s) z =
  let v () =
    let w  = Z.invert s q in
    let u1 = Z.(z * w mod q)
    and u2 = Z.(r * w mod q) in
    Z.((powm gg u1 p * powm y u2 p) mod p mod q) in
  Z.zero < r && r < q && Z.zero < s && s < q && v () = r

let sign ?mask ?k ~(key : priv) digest =
  let bits   = Numeric.Z.bits key.q in
  let size   = bits // 8 in
  let (r, s) = sign_z ?mask ?k ~key (Numeric.Z.of_cstruct_be ~bits digest) in
  Numeric.Z.(to_cstruct_be ~size r, to_cstruct_be ~size s)

let verify ~(key : pub) (r, s) digest =
  let z      = Numeric.Z.(of_cstruct_be ~bits:(bits key.q) digest)
  and (r, s) = Numeric.Z.(of_cstruct_be r, of_cstruct_be s) in
  verify_z ~key (r, s) z

let massage ~key:({ q; _ }: pub) digest =
  let bits = Numeric.Z.bits q in
  if bits >= Cstruct.len digest * 8 then digest else
    let cs = Numeric.Z.(to_cstruct_be Z.(of_cstruct_be digest mod q)) in
    Cs.(cs lsl ((8 - bits mod 8) mod 8))
