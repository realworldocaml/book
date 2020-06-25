open Mirage_crypto.Uncommon
open Sexplib.Conv
open Rresult

open Common

type pub = { p : Z_sexp.t ; q : Z_sexp.t ; gg : Z_sexp.t ; y : Z_sexp.t }
[@@deriving sexp]

let pub ?(fips = false) ~p ~q ~gg ~y () =
  guard Z.(one < gg && gg < p) (`Msg "bad generator") >>= fun () ->
  guard (Z_extra.pseudoprime q) (`Msg "q is not prime") >>= fun () ->
  guard (Z.is_odd p && Z_extra.pseudoprime p) (`Msg "p is not prime") >>= fun () ->
  guard Z.(zero < y && y < p) (`Msg "y not in 0..p-1") >>= fun () ->
  guard (q < p) (`Msg "q is not smaller than p") >>= fun () ->
  guard Z.(zero = (pred p) mod q) (`Msg "p - 1 mod q <> 0") >>= fun () ->
  (if fips then
     match Z.numbits p, Z.numbits q with
     | 1024, 160 | 2048, 224 | 2048, 256 | 3072, 256 -> Ok ()
     | _ -> Error (`Msg "bit length of p or q not FIPS specified")
   else
     Ok ()) >>= fun () ->
  Ok { p ; q ; gg ; y }

let pub_of_sexp s =
  let p = pub_of_sexp s in
  match pub ?fips:None ~p:p.p ~q:p.q ~gg:p.gg ~y:p.y () with
  | Ok p -> p
  | Error (`Msg m) -> invalid_arg "bad public %s" m

type priv =
  { p : Z_sexp.t ; q : Z_sexp.t ; gg : Z_sexp.t ; x : Z_sexp.t ; y : Z_sexp.t }
[@@deriving sexp]

let priv ?fips ~p ~q ~gg ~x ~y () =
  pub ?fips ~p ~q ~gg ~y () >>= fun _ ->
  guard Z.(zero < x && x < q) (`Msg "x not in 1..q-1") >>= fun () ->
  guard Z.(y = powm gg x p) (`Msg "y <> g ^ x mod p") >>= fun () ->
  Ok { p ; q ; gg ; x ; y }

let priv_of_sexp s =
  let p = priv_of_sexp s in
  match priv ?fips:None ~p:p.p ~q:p.q ~gg:p.gg ~x:p.x ~y:p.y () with
  | Ok p -> p
  | Error (`Msg m) -> invalid_arg "bad private %s" m

let pub_of_priv { p; q; gg; y; _ } = { p; q; gg; y }

type keysize = [ `Fips1024 | `Fips2048 | `Fips3072 | `Exactly of int * int ]

let expand_size = function
  | `Fips1024       -> (1024, 160)
  | `Fips2048       -> (2048, 256)
  | `Fips3072       -> (3072, 256)
  | `Exactly (l, n) ->
      if 3 <= l && 2 <= n then (l, n) else
        invalid_arg "Dsa.generate: bits: `Exactly (%d, %d)" l n

type mask = [ `No | `Yes | `Yes_with of Mirage_crypto_rng.g ]

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
  let two = Z.(~$2) in
  let (l, n) = expand_size size in
  let q = Z_extra.prime ?g ~msb:1 n in
  let p =
    let q_q  = Z.(q * two) in
    until Z_extra.pseudoprime @@ fun () ->
      let x = Z_extra.gen_bits ?g ~msb:1 l in
      Z.(x - (x mod q_q) + one)
  in
  let gg =
    let e = Z.(pred p / q) in
    until ((<>) Z.one) @@ fun () ->
      let h = Z_extra.gen_r ?g two Z.(pred p) in
      Z.(powm h e p)
  in
  (* all checks above are already satisfied *)
  (p, q, gg)

let generate ?g size =
  let (p, q, gg) = params ?g size in
  let x = Z_extra.gen_r ?g Z.one q in
  let y = Z.(powm gg x p) in
  (* checks are satisfied due to construction *)
  { p; q; gg; x; y }


module K_gen (H : Mirage_crypto.Hash.S) = struct

  let drbg : 'a Mirage_crypto_rng.generator =
    let module M = Mirage_crypto_rng.Hmac_drbg (H) in (module M)

  let z_gen ~key:{ q; x; _ } z =
    let repr = Z_extra.to_cstruct_be ~size:(Z.numbits q // 8) in
    let g    = Mirage_crypto_rng.create ~strict:true drbg in
    Mirage_crypto_rng.reseed ~g Cs.(repr x <+> repr Z.(z mod q));
    Z_extra.gen_r ~g Z.one q

  let generate ~key cs =
    z_gen ~key (Z_extra.of_cstruct_be ~bits:(Z.numbits key.q) cs)
end

module K_gen_sha256 = K_gen (Mirage_crypto.Hash.SHA256)

let sign_z ?(mask = `Yes) ?k:k0 ~key:({ p; q; gg; x; _ } as key) z =
  let k = match k0 with Some k -> k | None -> K_gen_sha256.z_gen ~key z in
  let k' = Z.invert k q
  and b, b' = match expand_mask mask with
    | `No -> Z.one, Z.one
    | `Yes g ->
      let m  = Z_extra.gen_r ?g Z.one q in
      m, Z.invert m q
  in
  let r = Z.(powm_sec gg k p mod q) in
  (* normal DSA sign is: s = k^-1 * (z + r * x) mod q *)
  (* we apply blinding where possible and compute:
     s = k^-1 * b^-1 * (b * z + b * r * x) mod q
     see https://github.com/openssl/openssl/pull/6524 for further details *)
  let s =
    let t1 =
      let t11 = Z.(b * x mod q) in
      Z.(t11 * r mod q)
    in
    let t2 = Z.(b * z mod q) in
    let t3 = Z.((t1 + t2) mod q) in
    let t4 = Z.(k' * t3 mod q) in
    Z.(b' * t4 mod q)
  in
  if r = Z.zero || s = Z.zero then invalid_arg "k unsuitable" else (r, s)

let verify_z ~key:({ p; q; gg; y }: pub ) (r, s) z =
  let v () =
    let w  = Z.invert s q in
    let u1 = Z.(z * w mod q)
    and u2 = Z.(r * w mod q) in
    Z.((powm gg u1 p * powm y u2 p) mod p mod q) in
  Z.zero < r && r < q && Z.zero < s && s < q && v () = r

let sign ?mask ?k ~(key : priv) digest =
  let bits   = Z.numbits key.q in
  let size   = bits // 8 in
  let (r, s) = sign_z ?mask ?k ~key (Z_extra.of_cstruct_be ~bits digest) in
  Z_extra.(to_cstruct_be ~size r, to_cstruct_be ~size s)

let verify ~(key : pub) (r, s) digest =
  let z      = Z_extra.of_cstruct_be ~bits:(Z.numbits key.q) digest
  and (r, s) = Z_extra.(of_cstruct_be r, of_cstruct_be s) in
  verify_z ~key (r, s) z

let massage ~key:({ q; _ }: pub) digest =
  let bits = Z.numbits q in
  if bits >= Cstruct.len digest * 8 then digest else
    let cs = Z_extra.(to_cstruct_be Z.(of_cstruct_be digest mod q)) in
    Cs.(cs lsl ((8 - bits mod 8) mod 8))
