let ( let* ) = Result.bind

type ecdsa = [
  | `P224 of Mirage_crypto_ec.P224.Dsa.pub
  | `P256 of Mirage_crypto_ec.P256.Dsa.pub
  | `P384 of Mirage_crypto_ec.P384.Dsa.pub
  | `P521 of Mirage_crypto_ec.P521.Dsa.pub
]

type t = [
  | ecdsa
  | `RSA of Mirage_crypto_pk.Rsa.pub
  | `ED25519 of Mirage_crypto_ec.Ed25519.pub
]

module Asn_oid = Asn.OID

module Asn = struct
  open Asn_grammars
  open Asn.S
  open Mirage_crypto_pk

  let rsa_public_key =
    let f (n, e) = match Rsa.pub ~e ~n with
      | Ok p -> p
      | Error (`Msg m) -> parse_error "bad RSA public key %s" m
    and g ({ Rsa.n; e } : Rsa.pub) = (n, e) in
    map f g @@
    sequence2
      (required ~label:"modulus"        integer)
      (required ~label:"publicExponent" integer)

  let (rsa_public_of_cstruct, rsa_public_to_cstruct) =
    projections_of Asn.der rsa_public_key

  let rsa_pub_of_cs, rsa_pub_to_cs = project_exn rsa_public_key

  let to_err = function
    | Ok r -> r
    | Error e ->
      parse_error "failed to decode public EC key %a"
        Mirage_crypto_ec.pp_error e

  let reparse_pk =
    let open Mirage_crypto_ec in
    let open Algorithm in
    function
    | (RSA      , cs) -> `RSA (rsa_pub_of_cs cs)
    | (ED25519  , cs) -> `ED25519 (to_err (Ed25519.pub_of_cstruct cs))
    | (EC_pub `SECP224R1, cs) -> `P224 (to_err (P224.Dsa.pub_of_cstruct cs))
    | (EC_pub `SECP256R1, cs) -> `P256 (to_err (P256.Dsa.pub_of_cstruct cs))
    | (EC_pub `SECP384R1, cs) -> `P384 (to_err (P384.Dsa.pub_of_cstruct cs))
    | (EC_pub `SECP521R1, cs) -> `P521 (to_err (P521.Dsa.pub_of_cstruct cs))
    | _ -> parse_error "unknown public key algorithm"

  let unparse_pk =
    let open Mirage_crypto_ec in
    let open Algorithm in
    function
    | `RSA pk    -> (RSA, rsa_pub_to_cs pk)
    | `ED25519 pk -> (ED25519, Ed25519.pub_to_cstruct pk)
    | `P224 pk -> (EC_pub `SECP224R1, P224.Dsa.pub_to_cstruct pk)
    | `P256 pk -> (EC_pub `SECP256R1, P256.Dsa.pub_to_cstruct pk)
    | `P384 pk -> (EC_pub `SECP384R1, P384.Dsa.pub_to_cstruct pk)
    | `P521 pk -> (EC_pub `SECP521R1, P521.Dsa.pub_to_cstruct pk)

  let pk_info_der =
    map reparse_pk unparse_pk @@
    sequence2
      (required ~label:"algorithm" Algorithm.identifier)
      (required ~label:"subjectPK" bit_string_cs)

  let (pub_info_of_cstruct, pub_info_to_cstruct) =
    projections_of Asn.der pk_info_der
end

let id k =
  let data = match k with
    | `RSA p -> Asn.rsa_public_to_cstruct p
    | `ED25519 pk -> Mirage_crypto_ec.Ed25519.pub_to_cstruct pk
    | `P224 pk -> Mirage_crypto_ec.P224.Dsa.pub_to_cstruct pk
    | `P256 pk -> Mirage_crypto_ec.P256.Dsa.pub_to_cstruct pk
    | `P384 pk -> Mirage_crypto_ec.P384.Dsa.pub_to_cstruct pk
    | `P521 pk -> Mirage_crypto_ec.P521.Dsa.pub_to_cstruct pk
  in
  Mirage_crypto.Hash.digest `SHA1 data

let fingerprint ?(hash = `SHA256) pub =
  Mirage_crypto.Hash.digest hash (Asn.pub_info_to_cstruct pub)

let key_type = function
  | `RSA _ -> `RSA
  | `ED25519 _ -> `ED25519
  | `P224 _ -> `P224
  | `P256 _ -> `P256
  | `P384 _ -> `P384
  | `P521 _ -> `P521

let sig_alg = function
  | #ecdsa -> `ECDSA
  | `RSA _ -> `RSA
  | `ED25519 _ -> `ED25519

let pp ppf k =
  Fmt.string ppf (Key_type.to_string (key_type k));
  Fmt.sp ppf ();
  Cstruct.hexdump_pp ppf (fingerprint k)

let hashed hash data =
  match data with
  | `Message msg -> Ok (Mirage_crypto.Hash.digest hash msg)
  | `Digest d ->
    let n = Cstruct.length d and m = Mirage_crypto.Hash.digest_size hash in
    if n = m then Ok d else Error (`Msg "digested data of invalid size")

let trunc len data =
  if Cstruct.length data > len then
    Cstruct.sub data 0 len
  else
    data

let verify hash ?scheme ~signature key data =
  let open Mirage_crypto_ec in
  let ok_if_true p = if p then Ok () else Error (`Msg "bad signature") in
  let ecdsa_of_cs cs =
    Result.map_error (function `Parse s -> `Msg s)
      (Algorithm.ecdsa_sig_of_cstruct cs)
  in
  let scheme = Key_type.opt_signature_scheme ?scheme (key_type key) in
  match key, scheme with
  | `RSA key, `RSA_PSS ->
    let module H = (val (Mirage_crypto.Hash.module_of hash)) in
    let module PSS = Mirage_crypto_pk.Rsa.PSS(H) in
    let* d = hashed hash data in
    ok_if_true (PSS.verify ~key ~signature (`Digest d))
  | `RSA key, `RSA_PKCS1 ->
    let hashp x = x = hash in
    let* d = hashed hash data in
    ok_if_true (Mirage_crypto_pk.Rsa.PKCS1.verify ~hashp ~key ~signature (`Digest d))
  | `ED25519 key, `ED25519 ->
    begin match data with
      | `Message msg -> ok_if_true (Ed25519.verify ~key signature ~msg)
      | `Digest _ -> Error (`Msg "Ed25519 only suitable with raw message")
    end
  | #ecdsa as key, `ECDSA ->
    let* d = hashed hash data in
    let* s = ecdsa_of_cs signature in
    ok_if_true
      (match key with
       | `P224 key -> P224.Dsa.verify ~key s (trunc P224.Dsa.byte_length d)
       | `P256 key -> P256.Dsa.verify ~key s (trunc P256.Dsa.byte_length d)
       | `P384 key -> P384.Dsa.verify ~key s (trunc P384.Dsa.byte_length d)
       | `P521 key -> P521.Dsa.verify ~key s (trunc P521.Dsa.byte_length d))
  | _ -> Error (`Msg "invalid key and signature scheme combination")

let encode_der = Asn.pub_info_to_cstruct

let decode_der cs = Asn_grammars.err_to_msg (Asn.pub_info_of_cstruct cs)

let decode_pem cs =
  let* data = Pem.parse cs in
  let pks = List.filter (fun (t, _) -> String.equal "PUBLIC KEY" t) data in
  let* keys = Pem.foldM (fun (_, k) -> decode_der k) pks in
  Pem.exactly_one ~what:"public key" keys

let encode_pem v =
  Pem.unparse ~tag:"PUBLIC KEY" (encode_der v)
