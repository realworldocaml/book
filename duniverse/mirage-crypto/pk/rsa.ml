open Mirage_crypto.Uncommon
open Sexplib.Conv
open Rresult

open Common

let two = Z.(~$2)
and three = Z.(~$3)

(* A constant-time [find_uint8] with a default value. *)
let ct_find_uint8 ~default ?off ~f cs =
  let res = Eqaf_cstruct.find_uint8 ?off ~f cs in
  Eqaf.select_int (res + 1) default res

let (&.) f g = fun h -> f (g h)

module Hash = Mirage_crypto.Hash

type 'a or_digest = [ `Message of 'a | `Digest of Hash.digest ]

module Digest_or (H : Hash.S) = struct
  let digest_or = function
    | `Message msg   -> H.digest msg
    | `Digest digest ->
        let n = digest.Cstruct.len and m = H.digest_size in
        if n = m then digest else
          invalid_arg "(`Digest _): %d bytes, expecting %d" n m
end

let digest_or ~hash =
  let module H = (val (Hash.module_of hash)) in
  let module D = Digest_or (H) in
  D.digest_or

exception Insufficient_key

type pub = { e : Z_sexp.t ; n : Z_sexp.t } [@@deriving sexp]

(* due to PKCS1 *)
let minimum_octets = 12
let minimum_bits = 8 * minimum_octets - 7

let pub ~e ~n =
  (* We cannot verify a public key being good (this would require to verify "n"
     being the multiplication of two prime numbers - figuring out which primes
     were used is the security property of RSA).

     but we validate to ensure our usage of powm_sec does not lead to
     exceptions, and we avoid tiny public keys where PKCS1 / PSS would lead to
     infinite loops or not work due to insufficient space for the header. *)
  guard Z.(n > zero && is_odd n && numbits n >= minimum_bits)
    (`Msg "invalid modulus") >>= fun () ->
  guard Z.(one < e && e < n) (`Msg "invalid exponent") >>= fun () ->
  (* NOTE that we could check for e being odd, or a prime, or 2^16+1, but
          these are not requirements, neither for RSA nor for powm_sec *)
  Ok { e ; n }

let pub_of_sexp s =
  let p = pub_of_sexp s in
  match pub ~e:p.e ~n:p.n with
  | Ok p -> p
  | Error (`Msg m) -> failwith "bad public key: %s" m

type priv = {
  e : Z_sexp.t ; d : Z_sexp.t ; n  : Z_sexp.t ;
  p : Z_sexp.t ; q : Z_sexp.t ; dp : Z_sexp.t ; dq : Z_sexp.t ; q' : Z_sexp.t
} [@@deriving sexp]

let valid_prime name p =
  guard Z.(p > zero && is_odd p && Z_extra.pseudoprime p)
    (R.msgf "invalid prime %s" name)

let rprime a b = Z.(gcd a b = one)

let valid_e ~e ~p ~q =
  guard (rprime e (Z.pred p) && rprime e (Z.pred q))
    (`Msg "e is not coprime of p and q") >>= fun () ->
  guard (Z_extra.pseudoprime e) (`Msg "exponent e is not a pseudoprime")

let priv ~e ~d ~n ~p ~q ~dp ~dq ~q' =
  pub ~e ~n >>= fun _pub ->
  valid_prime "p" p >>= fun () ->
  valid_prime "q" q >>= fun () ->
  guard (p <> q) (`Msg "p and q are the same number") >>= fun () ->
  valid_e ~e ~p ~q >>= fun () ->
  (* p and q are prime, and not equal -> multiplicative inverse exists *)
  guard Z.(q' = invert q p) (`Msg "q' <> q ^ -1 mod p") >>= fun () ->
  guard Z.(n = p * q) (`Msg "modulus is not the product of p and q") >>= fun () ->
  guard Z.(one < d && d < n) (`Msg "invalid private exponent") >>= fun () ->
  guard Z.(dp = d mod (pred p)) (`Msg "dp <> d mod (p - 1)") >>= fun () ->
  guard Z.(dq = d mod (pred q)) (`Msg "dq <> d mod (q - 1)") >>= fun () ->
  (* e has been checked (valid_e) to be coprime to p-1 and q-1 ->
     muliplicative inverse exists *)
  guard Z.(d = invert e (pred p * pred q))
    (`Msg "d <> e ^ -1 mod (p - 1) * (q - 1)") >>= fun () ->
  Ok { e ; d ; n ; p ; q ; dp ; dq ; q' }

let priv_of_sexp s =
  let p = priv_of_sexp s in
  match priv ~e:p.e ~d:p.d ~n:p.n ~p:p.p ~q:p.q ~dp:p.dp ~dq:p.dq ~q':p.q' with
  | Error (`Msg m) -> failwith "invalid private key %s" m
  | Ok p -> p

let priv_of_primes ~e ~p ~q =
  valid_prime "p" p >>= fun () ->
  valid_prime "q" q >>= fun () ->
  guard (p <> q) (`Msg "p and q are the same prime") >>= fun () ->
  valid_e ~e ~p ~q >>= fun () ->
  let n  = Z.(p * q) in
  pub ~e ~n >>= fun _pub ->
  (* valid_e checks e coprime to p-1 and q-1, a multiplicative inverse exists *)
  let d = Z.(invert e (pred p * pred q)) in
  let dp = Z.(d mod (pred p))
  and dq = Z.(d mod (pred q))
  in
  (* above we checked that p and q both are primes and not equal -> there
     should be a multiplicate inverse *)
  let q' = Z.invert q p in
  (* does not need to check valid_priv, since it is valid by construction *)
  Ok { e; d; n; p; q; dp; dq; q' }

(* Handbook of applied cryptography, 8.2.2 (i). *)
let priv_of_exp ?g ?(attempts=100) ~e ~d ~n () =
  pub ~e ~n >>= fun _ ->
  guard Z.(one < d && d < n) (`Msg "invalid private exponent") >>= fun () ->
  let rec doit ~attempts =
    let factor s t =
      let rec go ax = function
        | 0 -> None
        | i' ->
          let ax2 = Z.(ax * ax mod n) in
          if Z.(ax <> one && ax <> pred n && ax2 = one) then
            Some ax
          else
            go ax2 (i' - 1)
      in
      Option.map Z.(gcd n &. pred) (go Z.(powm (Z_extra.gen ?g n) t n) s)
    in
    if attempts > 0 then
      Z_extra.strip_factor ~f:two Z.(e * d |> pred) >>= function
      | (0, _) -> R.error_msgf "invalid factor 0"
      | (s, t) -> match factor s t with
        | None -> doit ~attempts:(attempts - 1)
        | Some p ->
          let q = Z.(div n p) in
          priv_of_primes ~e ~p:(max p q) ~q:(min p q)
    else Error (`Msg "attempts exceeded")
  in
  doit ~attempts

let rec generate ?g ?(e = Z.(~$0x10001)) ~bits () =
  if bits < minimum_bits || e < three ||
     (bits <= Z.numbits e || not (Z_extra.pseudoprime e))
  then
    invalid_arg "Rsa.generate: e: %a, bits: %d" Z.pp_print e bits;
  let (pb, qb) = (bits / 2, bits - bits / 2) in
  let (p, q) = Z_extra.(prime ?g ~msb:2 pb, prime ?g ~msb:2 qb) in
  match priv_of_primes ~e ~p:(max p q) ~q:(min p q) with
  | Error _ -> generate ?g ~e ~bits ()
  | Ok priv -> priv

let pub_of_priv ({ e; n; _ } : priv) = { e ; n }

let pub_bits  ({ n; _ } : pub)  = Z.numbits n
and priv_bits ({ n; _ } : priv) = Z.numbits n

type mask = [ `No | `Yes | `Yes_with of Mirage_crypto_rng.g ]

let encrypt_unsafe ~key: ({ e; n } : pub) msg = Z.(powm msg e n)

let decrypt_unsafe ~crt_hardening ~key:({ e; d; n; p; q; dp; dq; q'} : priv) c =
  let m1 = Z.(powm_sec c dp p)
  and m2 = Z.(powm_sec c dq q) in
  (* NOTE: neither erem, nor the multiplications (addition, subtraction) are
     guaranteed to be constant time by gmp *)
  let h  = Z.(erem (q' * (m1 - m2)) p) in
  let m  = Z.(h * q + m2) in
  (* counter Arjen Lenstra's CRT attack by verifying the signature. Since the
     public exponent is small, this is not very expensive. Mentioned again
     "Factoring RSA keys with TLS Perfect Forward Secrecy" (Weimer, 2015). *)
  if not crt_hardening || Z.(powm_sec m e n) = c then
    m
  else
    Z.(powm_sec c d n)

let decrypt_blinded_unsafe ~crt_hardening ?g ~key:({ e; n; _} as key : priv) c =
  let r  = until (rprime n) (fun _ -> Z_extra.gen_r ?g two n) in
  (* since r and n are coprime, there must be a multiplicative inverse *)
  let r' = Z.(invert r n) in
  let c' = Z.(powm_sec r e n * c mod n) in
  let x  = decrypt_unsafe ~crt_hardening ~key c' in
  Z.(r' * x mod n)

let (encrypt_z, decrypt_z) =
  let check_params n msg =
    if msg < two then invalid_arg "Rsa: message: %a" Z.pp_print msg;
    if n <= msg then raise Insufficient_key in
  (fun ~(key : pub) msg -> check_params key.n msg ; encrypt_unsafe ~key msg),
  (fun ~crt_hardening ~mask ~(key : priv) msg ->
    check_params key.n msg ;
    match mask with
    | `No         -> decrypt_unsafe ~crt_hardening ~key msg
    | `Yes        -> decrypt_blinded_unsafe ~crt_hardening ~key msg
    | `Yes_with g -> decrypt_blinded_unsafe ~crt_hardening ~g ~key msg )

let reformat out f msg =
  Z_extra.(of_cstruct_be msg |> f |> to_cstruct_be ~size:(out // 8))

let encrypt ~key              = reformat (pub_bits key)  (encrypt_z ~key)

let decrypt ?(crt_hardening=false) ?(mask=`Yes) ~key =
  reformat (priv_bits key) (decrypt_z ~crt_hardening ~mask ~key)

let b   = Cs.b
let cat = Cstruct.concat

let (bx00, bx01) = (b 0x00, b 0x01)

module PKCS1 = struct

  let min_pad = 8

  open Cstruct

  (* XXX Generalize this into `Rng.samplev` or something. *)
  let generate_with ?g ~f n =
    let cs = create n
    and k  = let b = Mirage_crypto_rng.block g in (n // b * b) in
    let rec go nonce i j =
      if i = n then cs else
      if j = k then go Mirage_crypto_rng.(generate ?g k) i 0 else
      match get_uint8 nonce j with
      | b when f b -> set_uint8 cs i b ; go nonce (succ i) (succ j)
      | _          -> go nonce i (succ j) in
    go Mirage_crypto_rng.(generate ?g k) 0 0

  let pad ~mark ~padding k msg =
    let pad = padding (k - len msg - 3 |> imax min_pad) in
    cat [ bx00 ; b mark ; pad ; bx00 ; msg ]

  let unpad ~mark ~is_pad cs =
    let f = not &. is_pad in
    let i = ct_find_uint8 ~default:2 ~off:2 ~f cs in
    let c1 = get_uint8 cs 0 = 0x00
    and c2 = get_uint8 cs 1 = mark
    and c3 = get_uint8 cs i = 0x00
    and c4 = min_pad <= i - 2 in
    if c1 && c2 && c3 && c4 then
      Some (sub cs (i + 1) (len cs - i - 1))
    else None

  let pad_01    =
    let padding size =
      let buf = Cstruct.create size in
      Cstruct.memset buf 0xff;
      buf
    in
    pad ~mark:0x01 ~padding
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

  let sig_encode ?(crt_hardening = true) ?mask ~key msg =
    padded pad_01 (decrypt ~crt_hardening ?mask ~key) (priv_bits key) msg

  let sig_decode ~key msg =
    unpadded unpad_01 (encrypt ~key) (pub_bits key) msg

  let encrypt ?g ~key msg =
    padded (pad_02 ?g) (encrypt ~key) (pub_bits key) msg

  let decrypt ?(crt_hardening = false) ?mask ~key msg =
    unpadded unpad_02 (decrypt ~crt_hardening ?mask ~key) (priv_bits key) msg

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

  let sign ?(crt_hardening = true) ?mask ~hash ~key msg =
    let msg' = Cs.(asn_of_hash hash <+> digest_or ~hash msg) in
    sig_encode ~crt_hardening ?mask ~key msg'

  let verify ~hashp ~key ~signature msg =
    let (>>=) = Option.bind
    and (>>|) = Fun.flip Option.map
    in
    Option.value
      (sig_decode ~key signature >>= fun cs ->
       detect cs >>| fun (hash, asn) ->
       hashp hash && Eqaf_cstruct.equal Cs.(asn <+> digest_or ~hash msg) cs)
      ~default:false

  let min_key hash =
    (len (asn_of_hash hash) + Hash.digest_size hash + min_pad + 2) * 8 + 1
end

module MGF1 (H : Hash.S) = struct

  let repr n =
    let cs = Cstruct.create_unsafe 4 in
    Cstruct.BE.set_uint32 cs 0 n;
    cs

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

  let eme_oaep_encode ?g ?(label = Cstruct.empty) k msg =
    let seed  = Mirage_crypto_rng.generate ?g hlen
    and pad   = Cstruct.create (max_msg_bytes k - len msg) in
    let db    = cat [ H.digest label ; pad ; bx01 ; msg ] in
    let mdb   = MGF.mask ~seed db in
    let mseed = MGF.mask ~seed:mdb seed in
    cat [ bx00 ; mseed ; mdb ]

  let eme_oaep_decode ?(label = Cstruct.empty) msg =
    let (b0, ms, mdb) = Cs.split3 msg 1 hlen in
    let db = MGF.mask ~seed:(MGF.mask ~seed:mdb ms) mdb in
    let i  = ct_find_uint8 ~default:0 ~off:hlen ~f:((<>) 0x00) db in
    let c1 = Eqaf_cstruct.equal (sub db 0 hlen) H.(digest label)
    and c2 = get_uint8 b0 0 = 0x00
    and c3 = get_uint8 db i = 0x01 in
    if c1 && c2 && c3 then Some (shift db (i + 1)) else None

  let encrypt ?g ?label ~key msg =
    let k = pub_bits key // 8 in
    if len msg > max_msg_bytes k then raise Insufficient_key
    else encrypt ~key @@ eme_oaep_encode ?g ?label k msg

  let decrypt ?(crt_hardening = false) ?mask ?label ~key em =
    let k = priv_bits key // 8 in
    if len em <> k || max_msg_bytes k < 0 then None else
      try eme_oaep_decode ?label @@ decrypt ~crt_hardening ?mask ~key em
      with Insufficient_key -> None

  (* XXX Review rfc3447 7.1.2 and
   * http://archiv.infsec.ethz.ch/education/fs08/secsem/Manger01.pdf
   * again for timing properties. *)

  (* XXX expose seed for deterministic testing? *)
end

module PSS (H: Hash.S) = struct

  open Cstruct

  module MGF = MGF1 (H)
  module H1  = Digest_or (H)

  let hlen = H.digest_size

  let bxbc = b 0xbc

  let b0mask embits = 0xff lsr ((8 - embits mod 8) mod 8)

  let zero_8 = Cstruct.create 8

  let digest ~salt msg = H.digesti @@ iter3 zero_8 (H1.digest_or msg) salt

  let emsa_pss_encode ?g slen emlen msg =
    let n    = emlen // 8
    and salt = Mirage_crypto_rng.generate ?g slen in
    let h    = digest ~salt msg in
    let db   = cat [ Cstruct.create (n - slen - hlen - 2) ; bx01 ; salt ] in
    let mdb  = MGF.mask ~seed:h db in
    set_uint8 mdb 0 @@ get_uint8 mdb 0 land b0mask emlen ;
    cat [ mdb ; h ; bxbc ]

  let emsa_pss_verify slen emlen em msg =
    let (mdb, h, bxx) = Cs.split3 em (em.len - hlen - 1) hlen in
    let db   = MGF.mask ~seed:h mdb in
    set_uint8 db 0 (get_uint8 db 0 land b0mask emlen) ;
    let salt = shift db (len db - slen) in
    let h'   = digest ~salt msg
    and i    = ct_find_uint8 ~default:0 ~f:((<>) 0x00) db in
    let c1 = lnot (b0mask emlen) land get_uint8 mdb 0 = 0x00
    and c2 = i = em.len - hlen - slen - 2
    and c3 = get_uint8 db  i = 0x01
    and c4 = get_uint8 bxx 0 = 0xbc
    and c5 = Eqaf_cstruct.equal h h' in
    c1 && c2 && c3 && c4 && c5

  let sufficient_key ~slen kbits =
    hlen + slen + 2 <= kbits / 8 (* 8 * (hlen + slen + 1) + 2 <= kbits *)

  let sign ?g ?(crt_hardening = false) ?mask ?(slen = hlen) ~key msg =
    let b = priv_bits key in
    if not (sufficient_key ~slen b) then raise Insufficient_key
    else
      let msg' = emsa_pss_encode ?g (imax 0 slen) (b - 1) msg in
      decrypt ~crt_hardening ?mask ~key msg'

  let verify ?(slen = hlen) ~key ~signature msg =
    let b = pub_bits key
    and s = len signature in
    s = b // 8 && sufficient_key ~slen b && try
      let em = encrypt ~key signature in
      emsa_pss_verify (imax 0 slen) (b - 1) (shift em (s - (b - 1) // 8)) msg
    with Insufficient_key -> false

end
