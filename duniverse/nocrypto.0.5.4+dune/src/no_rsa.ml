open No_uncommon
module Numeric = No_numeric
module Rng = No_rng
module Hash = No_hash
open Sexplib.Conv

type bits = int

exception Insufficient_key

type pub  = { e : Z.t ; n : Z.t } [@@deriving sexp]

type priv = {
  e : Z.t ; d : Z.t ; n  : Z.t ;
  p : Z.t ; q : Z.t ; dp : Z.t ; dq : Z.t ; q' : Z.t
} [@@deriving sexp]

type mask = [ `No | `Yes | `Yes_with of Rng.g ]

let rprime a b = Z.(gcd a b = one)

let priv_of_primes ~e ~p ~q =
  let n  = Z.(p * q)
  and d  = Z.(invert e (pred p * pred q)) in
  let dp = Z.(d mod (pred p))
  and dq = Z.(d mod (pred q))
  and q' = Z.(invert q p) in
  { e; d; n; p; q; dp; dq; q' }

(* Handbook of applied cryptography, 8.2.2 (i). *)
let rec priv_of_exp ?g ?(attempts=100) ~e ~d n =
  let factor s t =
    let rec go ax = function
      | 0  -> None
      | i' -> let ax2 = Z.(ax * ax mod n) in
              if Z.(ax <> one && ax <> pred n && ax2 = one) then
                Some ax
              else go ax2 (i' - 1) in
    Option.(go Z.(powm (Rng.Z.gen ?g n) t n) s >>| Z.(gcd n &. pred)) in
  let err (k : _ format4 -> _) =
    Z.(k "Rsa.priv_of_exp: e: %a, d: %a, n: %a" pp e pp d pp n) in
  if attempts > 0 then
    if Z.(two < n && two < e && two < d && e < n && d < n) then
      match Numeric.strip_factor ~f:Z.two Z.(e * d |> pred) with
      | (0, _) -> err invalid_arg
      | (s, t) -> match factor s t with
        | None   -> priv_of_exp ?g ~attempts:(attempts - 1) ~e ~d n
        | Some p -> let q = Z.(div n p) in
                    priv_of_primes ~e ~p:(max p q) ~q:(min p q)
    else err invalid_arg
  else err failwith

let pub_of_priv ({ e; n; _ } : priv) = { e ; n }

(* XXX handle this more gracefully... *)
let pub_bits  ({ n; _ } : pub)  = Numeric.Z.bits n
and priv_bits ({ n; _ } : priv) = Numeric.Z.bits n

let encrypt_unsafe ~key: ({ e; n } : pub) msg = Z.(powm msg e n)

let decrypt_unsafe ~key: ({ p; q; dp; dq; q'; _} : priv) c =
  let m1 = Z.(powm c dp p)
  and m2 = Z.(powm c dq q) in
  let h  = Z.(erem (q' * (m1 - m2)) p) in
  Z.(h * q + m2)

let decrypt_blinded_unsafe ?g ~key: ({ e; n; _} as key : priv) c =
  let r  = until (rprime n) (fun _ -> Rng.Z.gen_r ?g Z.two n) in
  let r' = Z.(invert r n) in
  let x  = decrypt_unsafe ~key Z.(powm r e n * c mod n) in
  Z.(r' * x mod n)

let (encrypt_z, decrypt_z) =
  let check_params n msg =
    if msg < Z.two then invalid_arg "Rsa: message: %a" Z.pp_print msg;
    if n <= msg then raise Insufficient_key in
  (fun ~(key : pub) msg -> check_params key.n msg ; encrypt_unsafe ~key msg),
  (fun ~mask ~(key : priv) msg ->
    check_params key.n msg ;
    match mask with
    | `No         -> decrypt_unsafe            ~key msg
    | `Yes        -> decrypt_blinded_unsafe    ~key msg
    | `Yes_with g -> decrypt_blinded_unsafe ~g ~key msg )

let reformat out f msg =
  Numeric.Z.(of_cstruct_be msg |> f |> to_cstruct_be ~size:(out // 8))

let encrypt ~key              = reformat (pub_bits key)  (encrypt_z ~key)
and decrypt ?(mask=`Yes) ~key = reformat (priv_bits key) (decrypt_z ~mask ~key)

let well_formed ~e ~p ~q =
  Z.three <= e && p <> q &&
  Numeric.(pseudoprime e && pseudoprime p && pseudoprime q) &&
  rprime e Z.(pred p) && rprime e Z.(pred q)

let rec generate ?g ?(e = Z.(~$0x10001)) bits =
  if e < Z.three || Numeric.(bits <= Z.bits e || not (pseudoprime e)) then
    invalid_arg "Rsa.generate: e: %a, bits: %d" Z.pp_print e bits;
  let (pb, qb) = (bits / 2, bits - bits / 2) in
  let (p, q)   = Rng.(prime ?g ~msb:2 pb, prime ?g ~msb:2 qb) in
  if (p <> q) && rprime e Z.(pred p) && rprime e Z.(pred q) then
    priv_of_primes ~e ~p:(max p q) ~q:(min p q)
  else generate ?g ~e bits


type 'a or_digest = 'a Hash.or_digest

let b   = Cs.b
let cat = Cstruct.concat

let (bx00, bx01) = (b 0x00, b 0x01)

module PKCS1 = struct

  let min_pad = 8

  open Cstruct

  (* XXX Generalize this into `Rng.samplev` or something. *)
  let generate_with ?g ~f n =
    let cs = create n
    and k  = let b = Rng.block g in (n // b * b) in
    let rec go nonce i j =
      if i = n then cs else
      if j = k then go Rng.(generate ?g k) i 0 else
      match get_uint8 nonce j with
      | b when f b -> set_uint8 cs i b ; go nonce (succ i) (succ j)
      | _          -> go nonce i (succ j) in
    go Rng.(generate ?g k) 0 0

  let pad ~mark ~padding k msg =
    let pad = padding (k - len msg - 3 |> imax min_pad) in
    cat [ bx00 ; b mark ; pad ; bx00 ; msg ]

  let unpad ~mark ~is_pad cs =
    let f = not &. is_pad in
    let i = Cs.ct_find_uint8 ~off:2 ~f cs |> Option.get ~def:2 in
    let c1 = get_uint8 cs 0 = 0x00
    and c2 = get_uint8 cs 1 = mark
    and c3 = get_uint8 cs i = 0x00
    and c4 = min_pad <= i - 2 in
    if c1 && c2 && c3 && c4 then
      Some (sub cs (i + 1) (len cs - i - 1))
    else None

  let pad_01    = pad ~mark:0x01 ~padding:(Cs.create ~init:0xff)
  let pad_02 ?g = pad ~mark:0x02 ~padding:(generate_with ?g ~f:((<>) 0x00))

  let unpad_01 = unpad ~mark:0x01 ~is_pad:((=) 0xff)
  let unpad_02 = unpad ~mark:0x02 ~is_pad:((<>) 0x00)

  let padded pad transform keybits msg =
    let n = keybits // 8 in
    let p = pad n msg in
    if len p = n then transform p else raise Insufficient_key

  let unpadded unpad transform keybits msg =
    if len msg = keybits // 8 then
      try unpad (transform msg) with Insufficient_key -> None
    else None

  let sig_encode ?mask ~key msg =
    padded pad_01 (decrypt ?mask ~key) (priv_bits key) msg

  let sig_decode ~key msg =
    unpadded unpad_01 (encrypt ~key) (pub_bits key) msg

  let encrypt ?g ~key msg =
    padded (pad_02 ?g) (encrypt ~key) (pub_bits key) msg

  let decrypt ?mask ~key msg =
    unpadded unpad_02 (decrypt ?mask ~key) (priv_bits key) msg

  let asns = List.(combine Hash.hashes &. map of_string) [
    "\x30\x20\x30\x0c\x06\x08\x2a\x86\x48\x86\xf7\x0d\x02\x05\x05\x00\x04\x10"     (* md5 *)
  ; "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14"                 (* sha1 *)
  ; "\x30\x2d\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x04\x05\x00\x04\x1c" (* sha224 *)
  ; "\x30\x31\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20" (* sha256 *)
  ; "\x30\x41\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x02\x05\x00\x04\x30" (* sha384 *)
  ; "\x30\x51\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x03\x05\x00\x04\x40" (* sha512 *)
  ]

  let asn_of_hash hash = try List.assoc hash asns with Not_found -> assert false

  let detect msg = List.find_opt (fun (_, asn) -> Cs.is_prefix asn msg) asns

  let sign ?mask ~hash ~key msg =
    sig_encode ?mask ~key Cs.(asn_of_hash hash <+> Hash.digest_or ~hash msg)

  let verify ~hashp ~key ~signature msg =
    let open Option in
    ( sig_decode ~key signature >>= fun cs -> detect cs >>| fun (hash, asn) ->
        hashp hash && Cs.(ct_eq (asn <+> Hash.digest_or ~hash msg)) cs )
    |> get ~def:false

  let min_key hash =
    (len (asn_of_hash hash) + Hash.digest_size hash + min_pad + 2) * 8 + 1
end

module MGF1 (H : Hash.S) = struct

  let repr = Numeric.Int32.to_cstruct_be ~size:4

  (* Assumes len < 2^32 * H.digest_size. *)
  let mgf ~seed len =
    let rec go acc c = function
      | 0 -> Cstruct.sub (cat (List.rev acc)) 0 len
      | n -> let h = H.digesti (iter2 seed (repr c)) in
             go (h :: acc) Int32.(succ c) (pred n) in
    go [] 0l (len // H.digest_size)

  let mask ~seed cs = Cs.xor (mgf ~seed (Cstruct.len cs)) cs
end

module OAEP (H : Hash.S) = struct

  open Cstruct

  module MGF = MGF1 (H)

  let hlen = H.digest_size

  let max_msg_bytes k = k - 2 * hlen - 2

  let eme_oaep_encode ?g ?(label = Cs.empty) k msg =
    let seed  = Rng.generate ?g hlen
    and pad   = Cs.create (max_msg_bytes k - len msg) in
    let db    = cat [ H.digest label ; pad ; bx01 ; msg ] in
    let mdb   = MGF.mask ~seed db in
    let mseed = MGF.mask ~seed:mdb seed in
    cat [ bx00 ; mseed ; mdb ]

  let eme_oaep_decode ?(label = Cs.empty) msg =
    let (b0, ms, mdb) = Cs.split3 msg 1 hlen in
    let db = MGF.mask ~seed:(MGF.mask ~seed:mdb ms) mdb in
    let i  = Cs.ct_find_uint8 ~off:hlen ~f:((<>) 0x00) db |> Option.get ~def:0
    in
    let c1 = Cs.ct_eq (sub db 0 hlen) H.(digest label)
    and c2 = get_uint8 b0 0 = 0x00
    and c3 = get_uint8 db i = 0x01 in
    if c1 && c2 && c3 then Some (shift db (i + 1)) else None

  let encrypt ?g ?label ~key msg =
    let k = pub_bits key // 8 in
    if len msg > max_msg_bytes k then raise Insufficient_key
    else encrypt ~key @@ eme_oaep_encode ?g ?label k msg

  let decrypt ?mask ?label ~key em =
    let k = priv_bits key // 8 in
    if len em <> k || max_msg_bytes k < 0 then None else
      try eme_oaep_decode ?label @@ decrypt ?mask ~key em
      with Insufficient_key -> None

  (* XXX Review rfc3447 7.1.2 and
   * http://archiv.infsec.ethz.ch/education/fs08/secsem/Manger01.pdf
   * again for timing properties. *)

  (* XXX expose seed for deterministic testing? *)
end

module PSS (H: Hash.S) = struct

  open Cstruct

  module MGF = MGF1 (H)
  module H1  = Hash.Digest_or (H)

  let hlen = H.digest_size

  let bxbc = b 0xbc

  let b0mask embits = 0xff lsr ((8 - embits mod 8) mod 8)

  let zero_8 = Cs.create 8

  let digest ~salt msg = H.digesti @@ iter3 zero_8 (H1.digest_or msg) salt

  let emsa_pss_encode ?g slen emlen msg =
    let n    = emlen // 8
    and salt = Rng.generate ?g slen in
    let h    = digest ~salt msg in
    let db   = cat [ Cs.create (n - slen - hlen - 2) ; bx01 ; salt ] in
    let mdb  = MGF.mask ~seed:h db in
    set_uint8 mdb 0 @@ get_uint8 mdb 0 land b0mask emlen ;
    cat [ mdb ; h ; bxbc ]

  let emsa_pss_verify slen emlen em msg =
    let (mdb, h, bxx) = Cs.split3 em (em.len - hlen - 1) hlen in
    let db   = MGF.mask ~seed:h mdb in
    set_uint8 db 0 (get_uint8 db 0 land b0mask emlen) ;
    let salt = shift db (len db - slen) in
    let h'   = digest ~salt msg
    and i    = Cs.ct_find_uint8 ~f:((<>) 0x00) db |> Option.get ~def:0 in
    let c1 = lnot (b0mask emlen) land get_uint8 mdb 0 = 0x00
    and c2 = i = em.len - hlen - slen - 2
    and c3 = get_uint8 db  i = 0x01
    and c4 = get_uint8 bxx 0 = 0xbc
    and c5 = Cs.ct_eq h h' in
    c1 && c2 && c3 && c4 && c5

  let sufficient_key ~slen kbits =
    hlen + slen + 2 <= kbits / 8 (* 8 * (hlen + slen + 1) + 2 <= kbits *)

  let sign ?g ?mask ?(slen = hlen) ~key msg =
    let b = priv_bits key in
    if not (sufficient_key ~slen b) then raise Insufficient_key
    else decrypt ?mask ~key @@ emsa_pss_encode ?g (imax 0 slen) (b - 1) msg

  let verify ?(slen = hlen) ~key ~signature msg =
    let b = pub_bits key
    and s = len signature in
    s = b // 8 && sufficient_key ~slen b && try
      let em = encrypt ~key signature in
      emsa_pss_verify (imax 0 slen) (b - 1) (shift em (s - (b - 1) // 8)) msg
    with Insufficient_key -> false

end
