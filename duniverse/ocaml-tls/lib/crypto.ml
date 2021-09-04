open Mirage_crypto

open Ciphersuite

let (<+>) = Cstruct.append

(* on-the-wire dh_params <-> (group, pub_message) *)
let dh_params_pack { Mirage_crypto_pk.Dh.p; gg ; _ } message =
  let cs_of_z = Mirage_crypto_pk.Z_extra.to_cstruct_be ?size:None in
  { Core.dh_p = cs_of_z p ; dh_g = cs_of_z gg ; dh_Ys = message }

and dh_params_unpack { Core.dh_p ; dh_g ; dh_Ys } =
  let z_of_cs = Mirage_crypto_pk.Z_extra.of_cstruct_be ?bits:None in
  match Mirage_crypto_pk.Dh.group ~p:(z_of_cs dh_p) ~gg:(z_of_cs dh_g) () with
  | Ok dh -> Ok (dh, dh_Ys)
  | Error _ as e -> e

module Ciphers = struct

  (* I'm not sure how to get rid of this type, but would welcome a solution *)
  (* only used as result of get_block, which is called by get_cipher below *)
  type keyed = | K_CBC : 'k State.cbc_cipher * (Cstruct.t -> 'k) -> keyed

  let get_block = function
    | TRIPLE_DES_EDE_CBC ->
        let open Cipher_block.DES in
        K_CBC ( (module CBC : Cipher_block.S.CBC with type key = CBC.key),
                CBC.of_secret )

    | AES_128_CBC ->
        let open Cipher_block.AES in
        K_CBC ( (module CBC : Cipher_block.S.CBC with type key = CBC.key),
                CBC.of_secret )

    | AES_256_CBC ->
        let open Cipher_block.AES in
        K_CBC ( (module CBC : Cipher_block.S.CBC with type key = CBC.key),
                CBC.of_secret )

  let get_aead ~secret ~nonce =
    let open Cipher_block.AES in
    function
    | AES_128_CCM | AES_256_CCM ->
       let cipher = (module CCM : Cipher_block.S.CCM with type key = CCM.key) in
       (* TODO the 16 should either be input or extracted from ciphersuite name *)
       let cipher_secret = CCM.of_secret ~maclen:16 secret in
       State.(AEAD { cipher = CCM cipher ; cipher_secret ; nonce })
    | AES_128_GCM | AES_256_GCM ->
       let cipher = (module GCM : Cipher_block.S.GCM with type key = GCM.key) in
       let cipher_secret = GCM.of_secret secret in
       State.(AEAD { cipher = GCM cipher ; cipher_secret ; nonce })
    | CHACHA20_POLY1305 ->
      let cipher = (module Chacha20 : AEAD with type key = Chacha20.key) in
      let cipher_secret = Chacha20.of_secret secret in
      State.(AEAD { cipher = ChaCha20_Poly1305 cipher ; cipher_secret ; nonce })

  let get_cipher ~secret ~hmac_secret ~iv_mode ~nonce = function
    | `Block (cipher, hmac) ->
       ( match get_block cipher with
         | K_CBC (cipher, sec) ->
            let cipher_secret = sec secret in
            State.(CBC { cipher ; cipher_secret ; iv_mode ; hmac ; hmac_secret })
       )

    | `AEAD cipher -> get_aead ~secret ~nonce cipher
end

let sequence_buf seq =
  let open Cstruct in
  let buf = create 8 in
  BE.set_uint64 buf 0 seq ;
  buf

let aead_nonce nonce seq =
  let s =
    let l = Cstruct.length nonce in
    let s = sequence_buf seq in
    let pad = Cstruct.create (l - 8) in
    pad <+> s
  in
  Uncommon.Cs.xor nonce s

let adata_1_3 len =
  (* additional data in TLS 1.3 is using the header (RFC 8446 Section 5.2):
     - APPLICATION_TYPE
     - 0x03 0x03 (for TLS version 1.2 -- binary representation is 0x03 0x03)
     - <length in 16 bit>
  *)
  let buf = Cstruct.create 5 in
  Cstruct.set_uint8 buf 0 (Packet.content_type_to_int Packet.APPLICATION_DATA) ;
  Cstruct.set_uint8 buf 1 3;
  Cstruct.set_uint8 buf 2 3;
  Cstruct.BE.set_uint16 buf 3 len ;
  buf

let pseudo_header seq ty (v_major, v_minor) v_length =
  let open Cstruct in
  let prefix = create 5 in
  set_uint8 prefix 0 (Packet.content_type_to_int ty);
  set_uint8 prefix 1 v_major;
  set_uint8 prefix 2 v_minor;
  BE.set_uint16 prefix 3 v_length;
  sequence_buf seq <+> prefix

(* MAC used in TLS *)
let mac hash key pseudo_hdr data =
  Hash.mac hash ~key (pseudo_hdr <+> data)

let cbc_block (type a) cipher =
  let module C = (val cipher : Cipher_block.S.CBC with type key = a) in C.block_size

(* crazy CBC padding and unpadding for TLS *)
let cbc_pad block data =
  let open Cstruct in

  (* 1 is the padding length, encoded as 8 bit at the end of the fragment *)
  let len = 1 + length data in
  (* we might want to add additional blocks of padding *)
  let padding_length = block - (len mod block) in
  (* 1 is again padding length field *)
  let cstruct_len = padding_length + 1 in
  let pad = create_unsafe cstruct_len in
  memset pad padding_length;
  pad

let cbc_unpad data =
  let open Cstruct in

  let len = length data in
  let padlen = get_uint8 data (pred len) in
  let (res, pad) = split data (len - padlen - 1) in

  let rec check = function
    | i when i > padlen -> true
    | i -> (get_uint8 pad i = padlen) && check (succ i) in

  try
    if check 0 then Some res else None
  with Invalid_argument _ -> None

let tag_len (type a) = function
  | State.CCM cipher ->
    let module C = (val cipher : Cipher_block.S.CCM with type key = a) in
    (* TODO this is wrong (but works since "16" is always passed in above,
       which indeed is the AES128/256 block size). There should be a
       C.tag_size (in CCM this needs to depend on the key though (due to
       different possible mac sizes), in contrast to GCM where we always have
       a static one) - maybe mirage-crypto CCM should take mac len as functor
       argument? *)
    C.block_size
  | State.GCM cipher ->
    let module C = (val cipher : Cipher_block.S.GCM with type key = a) in
    C.tag_size
  | State.ChaCha20_Poly1305 _ ->
    Poly1305.mac_size

let encrypt_aead (type a) ~cipher ~key ~nonce ?adata data =
  match cipher with
  | State.CCM cipher ->
    let module C = (val cipher : Cipher_block.S.CCM with type key = a) in
    C.authenticate_encrypt ~key ~nonce ?adata data
  | State.GCM cipher ->
    let module C = (val cipher : Cipher_block.S.GCM with type key = a) in
    C.authenticate_encrypt ~key ~nonce ?adata data
  | State.ChaCha20_Poly1305 cipher ->
    let module C = (val cipher : AEAD with type key = a) in
    C.authenticate_encrypt ~key ~nonce ?adata data

let decrypt_aead (type a) ~cipher ~key ~nonce ?adata data =
  match cipher with
  | State.CCM cipher ->
     let module C = (val cipher : Cipher_block.S.CCM with type key = a) in
     C.authenticate_decrypt ~key ~nonce ?adata data
  | State.GCM cipher ->
     let module C = (val cipher : Cipher_block.S.GCM with type key = a) in
     C.authenticate_decrypt ~key ~nonce ?adata data
  | State.ChaCha20_Poly1305 cipher ->
    let module C = (val cipher : AEAD with type key = a) in
    C.authenticate_decrypt ~key ~nonce ?adata data

let encrypt_cbc (type a) ~cipher ~key ~iv data =
  let module C = (val cipher : Cipher_block.S.CBC with type key = a) in
  let message = C.encrypt ~key ~iv (data <+> cbc_pad C.block_size data) in
  (message, C.next_iv ~iv message)

let decrypt_cbc (type a) ~cipher ~key ~iv data =
  let module C = (val cipher : Cipher_block.S.CBC with type key = a) in
  try
    let message = C.decrypt ~key ~iv data in
    match cbc_unpad message with
    | Some res -> Some (res, C.next_iv ~iv data)
    | None     -> None
  with
  (* This bails out immediately on mis-alignment, making it very timeable.
   * However, decryption belongs to the outermost level and this operation's
   * timing does not leak information ala padding oracle and friends. *)
  | Invalid_argument _ -> None
