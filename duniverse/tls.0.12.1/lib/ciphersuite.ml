(** Ciphersuite definitions and some helper functions. *)

(** sum type of all possible key exchange methods *)
type key_exchange_algorithm13 = [ `DHE_RSA ] [@@deriving sexp]
type key_exchange_algorithm = [ key_exchange_algorithm13 | `RSA ] [@@deriving sexp]

(** [required_keytype_and_usage kex] is [(keytype, usage)] which a certificate must have if it is used in the given [kex] method *)
let required_keytype_and_usage = function
  | `RSA     -> (`RSA, `Key_encipherment)
  | `DHE_RSA -> (`RSA, `Digital_signature) (* signing with the signature scheme and hash algorithm that will be employed in the server key exchange message. *)

type block_cipher =
  | TRIPLE_DES_EDE_CBC
  | AES_128_CBC
  | AES_256_CBC
  [@@deriving sexp]

type aead_cipher =
  | AES_128_CCM
  | AES_256_CCM
  | AES_128_GCM
  | AES_256_GCM
  [@@deriving sexp]

module H = struct
  type t = Mirage_crypto.Hash.hash

  let hs =
    [ (`MD5, "md5") ; (`SHA1, "sha1") ; (`SHA224, "sha224") ;
      (`SHA256, "sha256") ; (`SHA384, "sha384") ; (`SHA512, "sha512") ]

  let sexp_of_t h = Sexplib.Sexp.Atom (List.assoc h hs)

  let inv_hs = List.map (fun (a, b) -> (b, a)) hs

  let t_of_sexp = function
    | Sexplib.Sexp.Atom h -> List.assoc (String.lowercase_ascii h) inv_hs
    | _ -> failwith "can't convert sexp to hash"
end

type payload_protection13 = [ `AEAD of aead_cipher ] [@@deriving sexp]

type payload_protection =  [
  payload_protection13
  | `Block of block_cipher * H.t
  ] [@@deriving sexp]

(* this is K_LEN, max 8 N_MIN from RFC5116 sections 5.1 & 5.2 -- as defined in TLS1.3 RFC 8446 Section 5.3 *)
let kn_13 = function
  | AES_128_GCM -> (16, 12)
  | AES_256_GCM -> (32, 12)
  | AES_128_CCM -> (16, 12)
  | AES_256_CCM -> (32, 12)

(** [key_length iv payload_protection] is [(key size, IV size, mac size)] where key IV, and mac sizes are the required bytes for the given [payload_protection] *)
(* NB only used for <= TLS 1.2, IV length for AEAD defined in RFC 5288 Section 3 (for GCM), salt[4] for CCM in RFC 6655 Section 3 *)
let key_length iv pp =
  let mac_size = Mirage_crypto.Hash.digest_size in
  match pp with
  | `AEAD AES_128_CCM                -> (16, 4 , 0)
  | `AEAD AES_256_CCM                -> (32, 4 , 0)
  | `AEAD AES_128_GCM                -> (16, 4 , 0)
  | `AEAD AES_256_GCM                -> (32, 4 , 0)
  | `Block (bc, mac) ->
     let keylen, ivlen = match bc with
       | TRIPLE_DES_EDE_CBC -> (24, 8)
       | AES_128_CBC        -> (16, 16)
       | AES_256_CBC        -> (32, 16)
     and maclen = mac_size mac
     in
     match iv with
     | None -> (keylen, 0, maclen)
     | Some () -> (keylen, ivlen, maclen)

type ciphersuite13 = [
  | `AES_128_GCM_SHA256
  | `AES_256_GCM_SHA384
  (* | `CHACHA20_POLY1305_SHA256 *)
  | `AES_128_CCM_SHA256
] [@@deriving sexp]

let privprot13 = function
  | `AES_128_GCM_SHA256 -> AES_128_GCM
  | `AES_256_GCM_SHA384 -> AES_256_GCM
  (*  | `CHACHA20_POLY1305_SHA256 -> CHACHA20_POLY1305 *)
  | `AES_128_CCM_SHA256 -> AES_128_CCM

let hash13 = function
  | `AES_128_GCM_SHA256 -> `SHA256
  | `AES_256_GCM_SHA384 -> `SHA384
  (*  | `CHACHA20_POLY1305_SHA256 -> `SHA256 *)
  | `AES_128_CCM_SHA256 -> `SHA256

let any_ciphersuite_to_ciphersuite13 = function
  | Packet.TLS_AES_128_GCM_SHA256 -> Some `AES_128_GCM_SHA256
  | Packet.TLS_AES_256_GCM_SHA384 -> Some `AES_256_GCM_SHA384
  (*  | Packet.TLS_CHACHA20_POLY1305_SHA256 -> Some `CHACHA20_POLY1305_SHA256 *)
  | Packet.TLS_AES_128_CCM_SHA256 -> Some `AES_128_CCM_SHA256
  | _                                          -> None

type ciphersuite = [
  ciphersuite13
  | `DHE_RSA_WITH_AES_128_GCM_SHA256
  | `DHE_RSA_WITH_AES_256_GCM_SHA384
  | `DHE_RSA_WITH_AES_256_CCM
  | `DHE_RSA_WITH_AES_128_CCM
  | `DHE_RSA_WITH_AES_256_CBC_SHA256
  | `DHE_RSA_WITH_AES_128_CBC_SHA256
  | `DHE_RSA_WITH_AES_256_CBC_SHA
  | `DHE_RSA_WITH_AES_128_CBC_SHA
  | `DHE_RSA_WITH_3DES_EDE_CBC_SHA
  | `ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA256
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA
  | `ECDHE_RSA_WITH_3DES_EDE_CBC_SHA
  | `RSA_WITH_AES_256_CBC_SHA256
  | `RSA_WITH_AES_128_CBC_SHA256
  | `RSA_WITH_AES_256_CBC_SHA
  | `RSA_WITH_AES_128_CBC_SHA
  | `RSA_WITH_3DES_EDE_CBC_SHA
  | `RSA_WITH_AES_128_GCM_SHA256
  | `RSA_WITH_AES_256_GCM_SHA384
  | `RSA_WITH_AES_256_CCM
  | `RSA_WITH_AES_128_CCM
]  [@@deriving sexp]

let ciphersuite_to_ciphersuite13 : ciphersuite -> ciphersuite13 option = function
  | #ciphersuite13 as cs -> Some cs
  | _ -> None

let any_ciphersuite_to_ciphersuite = function
  | Packet.TLS_DHE_RSA_WITH_AES_256_CBC_SHA256 -> Some `DHE_RSA_WITH_AES_256_CBC_SHA256
  | Packet.TLS_DHE_RSA_WITH_AES_128_CBC_SHA256 -> Some `DHE_RSA_WITH_AES_128_CBC_SHA256
  | Packet.TLS_DHE_RSA_WITH_AES_256_CBC_SHA    -> Some `DHE_RSA_WITH_AES_256_CBC_SHA
  | Packet.TLS_DHE_RSA_WITH_AES_128_CBC_SHA    -> Some `DHE_RSA_WITH_AES_128_CBC_SHA
  | Packet.TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA   -> Some `DHE_RSA_WITH_3DES_EDE_CBC_SHA
  | Packet.TLS_RSA_WITH_AES_256_CBC_SHA256     -> Some `RSA_WITH_AES_256_CBC_SHA256
  | Packet.TLS_RSA_WITH_AES_128_CBC_SHA256     -> Some `RSA_WITH_AES_128_CBC_SHA256
  | Packet.TLS_RSA_WITH_AES_256_CBC_SHA        -> Some `RSA_WITH_AES_256_CBC_SHA
  | Packet.TLS_RSA_WITH_AES_128_CBC_SHA        -> Some `RSA_WITH_AES_128_CBC_SHA
  | Packet.TLS_RSA_WITH_3DES_EDE_CBC_SHA       -> Some `RSA_WITH_3DES_EDE_CBC_SHA
  | Packet.TLS_RSA_WITH_AES_128_CCM            -> Some `RSA_WITH_AES_128_CCM
  | Packet.TLS_RSA_WITH_AES_256_CCM            -> Some `RSA_WITH_AES_256_CCM
  | Packet.TLS_DHE_RSA_WITH_AES_128_CCM        -> Some `DHE_RSA_WITH_AES_128_CCM
  | Packet.TLS_DHE_RSA_WITH_AES_256_CCM        -> Some `DHE_RSA_WITH_AES_256_CCM
  | Packet.TLS_RSA_WITH_AES_128_GCM_SHA256     -> Some `RSA_WITH_AES_128_GCM_SHA256
  | Packet.TLS_RSA_WITH_AES_256_GCM_SHA384     -> Some `RSA_WITH_AES_256_GCM_SHA384
  | Packet.TLS_DHE_RSA_WITH_AES_128_GCM_SHA256 -> Some `DHE_RSA_WITH_AES_128_GCM_SHA256
  | Packet.TLS_DHE_RSA_WITH_AES_256_GCM_SHA384 -> Some `DHE_RSA_WITH_AES_256_GCM_SHA384
  | Packet.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> Some `ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | Packet.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> Some `ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | Packet.TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384 -> Some `ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | Packet.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256 -> Some `ECDHE_RSA_WITH_AES_128_CBC_SHA256
  | Packet.TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA  -> Some `ECDHE_RSA_WITH_AES_256_CBC_SHA
  | Packet.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA  -> Some `ECDHE_RSA_WITH_AES_128_CBC_SHA
  | Packet.TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA -> Some `ECDHE_RSA_WITH_3DES_EDE_CBC_SHA
  | x -> any_ciphersuite_to_ciphersuite13 x

let ciphersuite_to_any_ciphersuite = function
  | `DHE_RSA_WITH_AES_256_CBC_SHA256 -> Packet.TLS_DHE_RSA_WITH_AES_256_CBC_SHA256
  | `DHE_RSA_WITH_AES_128_CBC_SHA256 -> Packet.TLS_DHE_RSA_WITH_AES_128_CBC_SHA256
  | `DHE_RSA_WITH_AES_256_CBC_SHA    -> Packet.TLS_DHE_RSA_WITH_AES_256_CBC_SHA
  | `DHE_RSA_WITH_AES_128_CBC_SHA    -> Packet.TLS_DHE_RSA_WITH_AES_128_CBC_SHA
  | `DHE_RSA_WITH_3DES_EDE_CBC_SHA   -> Packet.TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA
  | `RSA_WITH_AES_256_CBC_SHA256     -> Packet.TLS_RSA_WITH_AES_256_CBC_SHA256
  | `RSA_WITH_AES_128_CBC_SHA256     -> Packet.TLS_RSA_WITH_AES_128_CBC_SHA256
  | `RSA_WITH_AES_256_CBC_SHA        -> Packet.TLS_RSA_WITH_AES_256_CBC_SHA
  | `RSA_WITH_AES_128_CBC_SHA        -> Packet.TLS_RSA_WITH_AES_128_CBC_SHA
  | `RSA_WITH_3DES_EDE_CBC_SHA       -> Packet.TLS_RSA_WITH_3DES_EDE_CBC_SHA
  | `RSA_WITH_AES_128_CCM            -> Packet.TLS_RSA_WITH_AES_128_CCM
  | `RSA_WITH_AES_256_CCM            -> Packet.TLS_RSA_WITH_AES_256_CCM
  | `DHE_RSA_WITH_AES_128_CCM        -> Packet.TLS_DHE_RSA_WITH_AES_128_CCM
  | `DHE_RSA_WITH_AES_256_CCM        -> Packet.TLS_DHE_RSA_WITH_AES_256_CCM
  | `RSA_WITH_AES_128_GCM_SHA256     -> Packet.TLS_RSA_WITH_AES_128_GCM_SHA256
  | `RSA_WITH_AES_256_GCM_SHA384     -> Packet.TLS_RSA_WITH_AES_256_GCM_SHA384
  | `DHE_RSA_WITH_AES_128_GCM_SHA256 -> Packet.TLS_DHE_RSA_WITH_AES_128_GCM_SHA256
  | `DHE_RSA_WITH_AES_256_GCM_SHA384 -> Packet.TLS_DHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> Packet.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> Packet.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384 -> Packet.TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA256 -> Packet.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA  -> Packet.TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA  -> Packet.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA
  | `ECDHE_RSA_WITH_3DES_EDE_CBC_SHA -> Packet.TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA
  | `AES_128_GCM_SHA256 -> Packet.TLS_AES_128_GCM_SHA256
  | `AES_256_GCM_SHA384 -> Packet.TLS_AES_256_GCM_SHA384
  (*  | `CHACHA20_POLY1305_SHA256 -> Packet.TLS_CHACHA20_POLY1305_SHA256 *)
  | `AES_128_CCM_SHA256 -> Packet.TLS_AES_128_CCM_SHA256

let ciphersuite_to_string x = Packet.any_ciphersuite_to_string (ciphersuite_to_any_ciphersuite x)

(** [get_kex_privprot ciphersuite] is [(kex, privacy_protection)] where it dissects the [ciphersuite] into a pair containing the key exchange method [kex], and its [privacy_protection] *)
let get_kex_privprot = function
  | `RSA_WITH_3DES_EDE_CBC_SHA       -> (`RSA    , `Block (TRIPLE_DES_EDE_CBC, `SHA1))
  | `DHE_RSA_WITH_3DES_EDE_CBC_SHA   -> (`DHE_RSA, `Block (TRIPLE_DES_EDE_CBC, `SHA1))
  | `RSA_WITH_AES_128_CBC_SHA        -> (`RSA    , `Block (AES_128_CBC, `SHA1))
  | `DHE_RSA_WITH_AES_128_CBC_SHA    -> (`DHE_RSA, `Block (AES_128_CBC, `SHA1))
  | `RSA_WITH_AES_256_CBC_SHA        -> (`RSA    , `Block (AES_256_CBC, `SHA1))
  | `DHE_RSA_WITH_AES_256_CBC_SHA    -> (`DHE_RSA, `Block (AES_256_CBC, `SHA1))
  | `RSA_WITH_AES_128_CBC_SHA256     -> (`RSA    , `Block (AES_128_CBC, `SHA256))
  | `RSA_WITH_AES_256_CBC_SHA256     -> (`RSA    , `Block (AES_256_CBC, `SHA256))
  | `DHE_RSA_WITH_AES_128_CBC_SHA256 -> (`DHE_RSA, `Block (AES_128_CBC, `SHA256))
  | `DHE_RSA_WITH_AES_256_CBC_SHA256 -> (`DHE_RSA, `Block (AES_256_CBC, `SHA256))
  | `RSA_WITH_AES_128_CCM            -> (`RSA    , `AEAD AES_128_CCM)
  | `RSA_WITH_AES_256_CCM            -> (`RSA    , `AEAD AES_256_CCM)
  | `DHE_RSA_WITH_AES_128_CCM        -> (`DHE_RSA, `AEAD AES_128_CCM)
  | `DHE_RSA_WITH_AES_256_CCM        -> (`DHE_RSA, `AEAD AES_256_CCM)
  | `RSA_WITH_AES_128_GCM_SHA256     -> (`RSA    , `AEAD AES_128_GCM)
  | `RSA_WITH_AES_256_GCM_SHA384     -> (`RSA    , `AEAD AES_256_GCM)
  | `DHE_RSA_WITH_AES_128_GCM_SHA256 -> (`DHE_RSA, `AEAD AES_128_GCM)
  | `DHE_RSA_WITH_AES_256_GCM_SHA384 -> (`DHE_RSA, `AEAD AES_256_GCM)
  | `ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> (`DHE_RSA, `AEAD AES_128_GCM)
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> (`DHE_RSA, `AEAD AES_256_GCM)
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384 -> (`DHE_RSA, `Block (AES_256_CBC, `SHA384))
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA256 -> (`DHE_RSA, `Block (AES_128_CBC, `SHA256))
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA -> (`DHE_RSA, `Block (AES_256_CBC, `SHA1))
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA -> (`DHE_RSA, `Block (AES_128_CBC, `SHA1))
  | `ECDHE_RSA_WITH_3DES_EDE_CBC_SHA -> (`DHE_RSA, `Block (TRIPLE_DES_EDE_CBC, `SHA1))
  | #ciphersuite13 as cs13 -> (`DHE_RSA, `AEAD (privprot13 cs13))

(** [ciphersuite_kex ciphersuite] is [kex], first projection of [get_kex_privprot] *)
let ciphersuite_kex c = fst (get_kex_privprot c)

(** [ciphersuite_privprot ciphersuite] is [privprot], second projection of [get_kex_privprot] *)
let ciphersuite_privprot c = snd (get_kex_privprot c)

let ciphersuite_fs cs =
  match ciphersuite_kex cs with
  | `DHE_RSA -> true
  | `RSA -> false

let ecc = function
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA
  | `ECDHE_RSA_WITH_3DES_EDE_CBC_SHA
  | `ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA256 -> true
  | _ -> false

let ciphersuite_tls12_only = function
  | `DHE_RSA_WITH_AES_256_CBC_SHA256
  | `DHE_RSA_WITH_AES_128_CBC_SHA256
  | `RSA_WITH_AES_256_CBC_SHA256
  | `RSA_WITH_AES_128_CBC_SHA256
  | `RSA_WITH_AES_128_CCM
  | `RSA_WITH_AES_256_CCM
  | `DHE_RSA_WITH_AES_128_CCM
  | `DHE_RSA_WITH_AES_256_CCM
  | `RSA_WITH_AES_128_GCM_SHA256
  | `RSA_WITH_AES_256_GCM_SHA384
  | `DHE_RSA_WITH_AES_128_GCM_SHA256
  | `DHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_RSA_WITH_AES_128_CBC_SHA256 -> true
  | _ -> false

let ciphersuite_tls13 = function
  | `AES_128_GCM_SHA256
  | `AES_256_GCM_SHA384
  (*  | `CHACHA20_POLY1305_SHA256 *)
  | `AES_128_CCM_SHA256 -> true
  | _ -> false
