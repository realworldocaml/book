let (<+>) = Utils.Cs.(<+>)

let cdiv (x : int) (y : int) =
  if x > 0 && y > 0 then (x + y - 1) / y
  else if x < 0 && y < 0 then (x + y + 1) / y
  else x / y

let left_pad_dh group msg =
  let bytes = cdiv (Mirage_crypto_pk.Dh.modulus_size group) 8 in
  let padding = Cstruct.create (bytes - Cstruct.len msg) in
  padding <+> msg

let not_all_zero = function
  | None -> None
  | Some cs ->
    let all_zero = Cstruct.create (Cstruct.len cs) in
    if Cstruct.equal all_zero cs then None else Some cs

let share_appropriate_length group share =
  match Core.group_to_impl group with
  | `Hacl `X25519 -> Ok () (* already checked by hacl_x25519.key_exchange *)
  | `Fiat `P256 -> Ok () (* already checked by fiat_p256.key_exchange *)
  | `Mirage_crypto group ->
    let bits = Mirage_crypto_pk.Dh.modulus_size group in
    if Cstruct.len share = cdiv bits 8 then
      Ok ()
    else (* truncated share, better reject this *)
      Error (`Fatal `InvalidDH)

let dh_shared group secret share =
  (* RFC 8556, Section 7.4.1 - we need zero-padding on the left *)
  not_all_zero
    (match Core.group_to_impl group, secret with
     | `Mirage_crypto mc_group, `Mirage_crypto secret ->
       begin match Mirage_crypto_pk.Dh.shared secret share with
         | None -> None
         | Some shared -> Some (left_pad_dh mc_group shared)
       end
     | `Hacl `X25519, `Hacl priv ->
       begin match Hacl_x25519.key_exchange priv share with
         | Error _ -> None
         | Ok shared -> Some shared
       end
     | `Fiat `P256, `Fiat priv ->
       begin match Fiat_p256.key_exchange priv share with
         | Error _ -> None
         | Ok shared -> Some shared
       end
     | _ -> None)

let dh_gen_key group =
  (* RFC 8556, Section 4.2.8.1 - we need zero-padding on the left *)
  match Core.group_to_impl group with
  | `Mirage_crypto mc_group ->
    let sec, shared = Mirage_crypto_pk.Dh.gen_key mc_group in
    `Mirage_crypto sec, left_pad_dh mc_group shared
  | `Hacl `X25519 ->
    let secret, shared = Hacl_x25519.gen_key ~rng:Mirage_crypto_rng.generate in
    `Hacl secret, shared
  | `Fiat `P256 ->
    let secret, shared = Fiat_p256.gen_key ~rng:Mirage_crypto_rng.generate in
    `Fiat secret, shared

let trace tag cs = Tracing.cs ~tag:("crypto " ^ tag) cs

let pp_hash_k_n ciphersuite =
  let open Ciphersuite in
  let pp = privprot13 ciphersuite
  and hash = hash13 ciphersuite
  in
  let k, n = kn_13 pp in
  (pp, hash, k, n)

let hkdflabel label context length =
  let len =
    let b = Cstruct.create 2 in
    Cstruct.BE.set_uint16 b 0 length ;
    b
  and label =
    let lbl = Cstruct.of_string ("tls13 " ^ label) in
    let l = Cstruct.create 1 in
    Cstruct.set_uint8 l 0 (Cstruct.len lbl) ;
    l <+> lbl
  and context =
    let l = Cstruct.create 1 in
    Cstruct.set_uint8 l 0 (Cstruct.len context) ;
    l <+> context
  in
  let lbl = len <+> label <+> context in
  trace "hkdflabel" lbl ;
  lbl

let derive_secret_no_hash hash prk ?length ?(ctx = Cstruct.empty) label =
  let length = match length with
    | None -> Mirage_crypto.Hash.digest_size hash
    | Some x -> x
  in
  let info = hkdflabel label ctx length in
  trace "prk" prk ;
  let key = Hkdf.expand ~hash ~prk ~info length in
  trace ("derive_secret: " ^ label) key ;
  key

let derive_secret t label log =
  let ctx = Mirage_crypto.Hash.digest t.State.hash log in
  trace "derive secret ctx" ctx ;
  derive_secret_no_hash t.State.hash t.State.secret ~ctx label

let empty cipher = {
  State.secret = Cstruct.empty ;
  cipher ;
  hash = Ciphersuite.hash13 cipher
}

let derive t secret_ikm =
  let salt =
    if Cstruct.equal t.State.secret Cstruct.empty then
      Cstruct.empty
    else
      derive_secret t "derived" Cstruct.empty
  in
  trace "derive: secret_ikm" secret_ikm ;
  trace "derive: salt" salt ;
  let secret = Hkdf.extract ~hash:t.State.hash ~salt secret_ikm in
  trace "derive (extracted secret)" secret ;
  { t with State.secret }

let traffic_key cipher prk =
  let _, hash, key_len, iv_len = pp_hash_k_n cipher in
  let key_info = hkdflabel "key" Cstruct.empty key_len in
  let key = Hkdf.expand ~hash ~prk ~info:key_info key_len in
  let iv_info = hkdflabel "iv" Cstruct.empty iv_len in
  let iv = Hkdf.expand ~hash ~prk ~info:iv_info iv_len in
  (key, iv)

let ctx t label secret =
  let secret, nonce = traffic_key t.State.cipher secret in
  trace (label ^ " secret") secret ;
  trace (label ^ " nonce") nonce ;
  let pp = Ciphersuite.privprot13 t.State.cipher in
  { State.sequence = 0L ; cipher_st = Crypto.Ciphers.get_aead ~secret ~nonce pp }

let early_traffic t log =
  let secret = derive_secret t "c e traffic" log in
  (secret, ctx t "client early traffic" secret)

let hs_ctx t log =
  Tracing.cs ~tag:"hs ctx with sec" t.State.secret ;
  Tracing.cs ~tag:"log is" log ;
  let server_handshake_traffic_secret = derive_secret t "s hs traffic" log
  and client_handshake_traffic_secret = derive_secret t "c hs traffic" log
  in
  (server_handshake_traffic_secret,
   ctx t "server handshake traffic" server_handshake_traffic_secret,
   client_handshake_traffic_secret,
   ctx t "client handshake traffic" client_handshake_traffic_secret)

let app_ctx t log =
  let server_application_traffic_secret = derive_secret t "s ap traffic" log
  and client_application_traffic_secret = derive_secret t "c ap traffic" log
  in
  (server_application_traffic_secret,
   ctx t "server application traffic" server_application_traffic_secret,
   client_application_traffic_secret,
   ctx t "client application traffic" client_application_traffic_secret)

let app_secret_n_1 t app_secret =
  let secret = derive_secret_no_hash t.State.hash app_secret "traffic upd" in
  secret, ctx t "traffic update" secret

let exporter t log = derive_secret t "exp master" log
let resumption t log = derive_secret t "res master" log

let res_secret hash secret nonce =
  derive_secret_no_hash hash secret ~ctx:nonce "resumption"

let finished hash secret data =
  let key = derive_secret_no_hash hash secret "finished" in
  Mirage_crypto.Hash.mac hash ~key (Mirage_crypto.Hash.digest hash data)
