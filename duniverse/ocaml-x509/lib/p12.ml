(* partial PKCS12 implementation, as defined in RFC 7292
   - no public/private key mode, only password privacy and integrity
   - algorithmidentifier those I need for openssl interop (looking at the p12 I have on my disk)
   - require version being 3

   some definitions from PKCS7 (RFC 2315) are implemented as well, as needed
*)

type content_info = Asn.oid * Cstruct.t

type digest_info = Algorithm.t * Cstruct.t

type mac_data = digest_info * Cstruct.t * int

type t = Cstruct.t * mac_data

module Asn = struct
  open Asn_grammars
  open Asn.S
  open Registry

  let encrypted_content_info =
    let f (oid, algo, content) =
      if Asn.OID.equal PKCS7.data oid then
        (algo, content)
      else
        parse_error "expected OID PKCS7 data"
    and g (algo, content) =
      (PKCS7.data, algo, content)
    in
    Asn.S.map f g @@
    sequence3
      (required ~label:"content type" oid) (* here we assume data!? *)
      (required ~label:"content encryption algorithm" Algorithm.identifier)
      (optional ~label:"encrypted content" (implicit 0 octet_string))

  let encrypted_data =
    let f (v, eci) =
      if v = 0 then eci else parse_error "unknown encrypted data version"
    and g eci = 0, eci
    in
    map f g @@
    sequence2
      (required ~label:"version" int)
      (required ~label:"encrypted content info" encrypted_content_info)

  let content_info =
    let f (oid, data) =
      match data with
      | None -> parse_error "found no value for content info"
      | Some `C1 data when Asn.OID.equal PKCS7.data oid -> `Data data
      | Some `C2 eci when Asn.OID.equal PKCS7.encrypted_data oid -> `Encrypted eci
      | _ -> parse_error "couldn't match PKCS7 oid with choice"
    and g = function
      | `Data data -> PKCS7.data, Some (`C1 data)
      | `Encrypted eci -> PKCS7.encrypted_data, Some (`C2 eci)
    in
    map f g @@
    sequence2
      (required ~label:"content type" oid)
      (optional ~label:"content" (explicit 0
                                    (choice2 octet_string encrypted_data)))

  let digest_info =
    sequence2
      (required ~label:"digest algorithm" Algorithm.identifier)
      (required ~label:"digest" octet_string)

  let mac_data =
    sequence3
      (required ~label:"mac" digest_info)
      (required ~label:"mac salt" octet_string)
      (required ~label:"iterations" int)

  let pfx =
    let f (version, content_info, mac_data) =
      if version = 3 then
        match content_info, mac_data with
        | `Data data, Some md -> data, md
        | _, None -> parse_error "missing mac_data"
        | _, _ -> parse_error "unsupported content_info"
      else
        parse_error "unsupported pfx version"
    and g (content, mac_data) =
      (3, `Data content, Some mac_data)
    in
    map f g @@
    sequence3
      (required ~label:"version" int)
      (required ~label:"auth safe" content_info)
      (* contentType is signedData in public-key integrity mode and data in
         password integrity mode *)
      (optional ~label:"mac data" mac_data) (* not present if public keys used *)

  let pfx_of_cs, pfx_to_cs = projections_of Asn.der pfx

  (* payload is a sequence of content_info *)
  let authenticated_safe = sequence_of content_info

  let auth_safe_of_cs, auth_safe_to_cs =
    projections_of Asn.der authenticated_safe

  let pkcs12_attribute =
    sequence2
      (required ~label:"attribute id" oid)
      (required ~label:"attribute value" (set_of octet_string))

  (* here:
     key_bag = PKCS8 private key
     pkcs8_shrouded_key_bag = encrypted private key info ==
       sequence2 Algorithm octet_string
     cert_bag =
      sequence2
        cert_id (PKCS9 cert_types <| 1 (X509) or 2 (SDSI))
        expl 0 cert_value (DER-encoded certificate)
     crl_bag = sequence2 crl_id (PKCS9 crl_types <| 1) (expl 0 crl (DER-encoded))

^^^---^^^ those we plan to support

     secret_bag = sequence2 secret_type (expl 0 secret_value)
     safe_contents_bag = (any of the above) safe_contents (recursive!)
  *)
  (* since asn1 does not yet support ANY defined BY, we develop a rather
     complex grammar covering all supported bags *)
  let safe_bag =
    let cert_oid, crl_oid =
      Asn.OID.(PKCS9.cert_types <| 1, PKCS9.crl_types <| 1)
    in
    let f (oid, (a, algo, data), attrs) =
      match a, algo, data with
      | `C1 v, Some a, `C1 data when Asn.OID.equal oid PKCS12.key_bag ->
        let key = Private_key.Asn.reparse_private (v, a, data) in
        `Private_key key, attrs
      | `C2 id, None, `C2 data ->
        if Asn.OID.equal oid PKCS12.cert_bag && Asn.OID.equal id cert_oid then
          match Certificate.decode_der data with
          | Error (`Msg e) -> error (`Parse e)
          | Ok cert -> `Certificate cert, attrs
        else if Asn.OID.equal oid PKCS12.crl_bag && Asn.OID.equal id crl_oid then
          match Crl.decode_der data with
          | Error (`Msg e) -> error (`Parse e)
          | Ok crl -> `Crl crl, attrs
        else
          parse_error "crl bag with non-standard crl"
      | `C3 algo, None, `C1 data when Asn.OID.equal oid PKCS12.pkcs8_shrouded_key_bag ->
        `Encrypted_private_key (algo, data), attrs
      | _ -> parse_error "safe bag OID not supported"
    and g (v, attrs) =
      let oid, d = match v with
        | `Encrypted_private_key (algo, data) ->
          PKCS12.pkcs8_shrouded_key_bag, (`C3 algo, None, `C1 data)
        | `Private_key pk ->
          let v, algo, data = Private_key.Asn.unparse_private pk in
          PKCS12.key_bag, (`C1 v, Some algo, `C1 data)
        | `Certificate cert -> PKCS12.cert_bag, (`C2 cert_oid, None, `C2 (Certificate.encode_der cert))
        | `Crl crl -> PKCS12.crl_bag, (`C2 crl_oid, None, `C2 (Crl.encode_der crl))
      in
      (oid, d, attrs)
    in
    map f g @@
    sequence3
      (required ~label:"bag id" oid)
      (required ~label:"bag value"
         (explicit 0
            (sequence3
               (required ~label:"fst" (choice3 int oid Algorithm.identifier))
               (optional ~label:"algorithm" Algorithm.identifier)
               (required ~label:"data" (choice2 octet_string (explicit 0 octet_string))))))
      (* (explicit 0 (* encrypted private key *)
         (sequence2
            (required ~label:"encryption algorithm" Algorithm.identifier)
            (required ~label:"encrypted data" octet_string))) *)
      (* (explicit 0 (* private key ] *)
            (sequence3
              (required ~label:"version"             int)
              (required ~label:"privateKeyAlgorithm" Algorithm.identifier)
              (required ~label:"privateKey"          octet_string)))  *)
      (* (explicit 0 (* cert / crl *)
            (sequence2
               (required ~label:"oid" oid)
               (required ~label:"data" (explicit 0 octet_string)))) *)
      (optional ~label:"bag attributes" (set_of pkcs12_attribute))

  let safe_contents = sequence_of safe_bag

  let safe_contents_of_cs, safe_contents_to_cs =
    projections_of Asn.der safe_contents
end

let prepare_pw str =
  let l = String.length str in
  let cs = Cstruct.create ((succ l) * 2) in
  for i = 0 to pred l do
    Cstruct.set_char cs (succ (i * 2)) (String.get str i)
  done;
  cs

let id len purpose =
  let b = Cstruct.create len in
  let id = match purpose with
    | `Encryption -> 1
    | `Iv -> 2
    | `Hmac -> 3
  in
  Cstruct.memset b id;
  b

let v = function
  | `MD5 | `SHA1 | `SHA224 | `SHA256 -> 512 / 8
  | `SHA384 | `SHA512 -> 1024 / 8

let fill ~data ~out =
  let len = Cstruct.len out
  and l = Cstruct.len data
  in
  let rec c off =
    if off < len then begin
      Cstruct.blit data 0 out off (min (len - off) l);
      c (off + l)
    end
  in
  c 0

let fill_or_empty size data =
  let l = Cstruct.len data in
  if l = 0 then data
  else
    let len = size * ((l + size - 1) / size) in
    let buf = Cstruct.create len in
    fill ~data ~out:buf;
    buf

let pbes algorithm purpose password salt iterations n =
  let pw = prepare_pw password
  and v = v algorithm
  and u = Mirage_crypto.Hash.digest_size algorithm
  in
  let diversifier = id v purpose in
  let salt = fill_or_empty v salt in
  let pass = fill_or_empty v pw in
  let out = Cstruct.create n in
  let rec one off i =
    let ai = ref (Mirage_crypto.Hash.digest algorithm (Cstruct.append diversifier i)) in
    for _j = 1 to pred iterations do
      ai := Mirage_crypto.Hash.digest algorithm !ai;
    done;
    Cstruct.blit !ai 0 out off (min (n - off) u);
    if u >= n - off then () else
      (* 6B *)
      let b = Cstruct.create v in
      fill ~data:!ai ~out:b;
      (* 6C *)
      let i' = Cstruct.create (Cstruct.len i) in
      for j = 0 to pred (Cstruct.len i / v) do
        let c = ref 1 in
        for k = pred v downto 0 do
          let idx = j * v + k in
          c := (!c + Cstruct.get_uint8 i idx + Cstruct.get_uint8 b k) land 0xFFFF;
          Cstruct.set_uint8 i' idx (!c land 0xFF);
          c := !c lsr 8;
        done;
      done;
      one (off + u) i'
  in
  let i = Cstruct.append salt pass in
  one 0 i;
  out

(* TODO PKCS5/7 padding is "k - (l mod k)" i.e. always > 0!
   (and rc4 being a stream cipher has no padding!) *)
let unpad x =
  (* TODO can there be bad padding in this scheme? *)
  let l = Cstruct.len x in
  if l > 0 then
    let amount = Cstruct.get_uint8 x (pred l) in
    let split_point = if l > amount then l - amount else l in
    let data, pad = Cstruct.split x split_point in
    let good = ref true in
    for i = 0 to pred amount do
      if Cstruct.get_uint8 pad i <> amount then good := false
    done;
    if !good then data else x
  else
    x

let pad bs x =
  let l = Cstruct.len x in
  let to_pad = bs - (l mod bs) in
  let amount = Cstruct.create to_pad in
  Cstruct.memset amount to_pad;
  Cstruct.append x amount

(* there are 3 possibilities to encrypt / decrypt things:
   - PKCS12 KDF (see above), with RC2/RC4/DES
   - PKCS5 v1 (PBES, PBKDF1) -- not (yet?) supported
   - PKCS5 v2 (PBES2, PBKDF2)
*)
let pkcs12_decrypt algo password data =
  let open Algorithm in
  let open Rresult.R.Infix in
  let hash = `SHA1 in
  (match algo with
   | SHA_RC4_128 (s, i) -> Ok (s, i, 16, 0)
   | SHA_RC4_40 (s, i) -> Ok (s, i, 5, 0)
   | SHA_3DES_CBC (s, i) -> Ok (s, i, 24, 8)
   | SHA_2DES_CBC (s, i) -> Ok (s, i, 16, 8) (* TODO 2des -> 3des keys (if relevant)*)
   | SHA_RC2_128_CBC (s, i) -> Ok (s, i, 16, 8)
   | SHA_RC2_40_CBC (s, i) -> Ok (s, i, 5, 8)
   | _ -> Error (`Msg "unsupported algorithm")) >>= fun (salt, count, key_len, iv_len) ->
  let key = pbes hash `Encryption password salt count key_len
  and iv = pbes hash `Iv password salt count iv_len
  in
  (match algo with
   | SHA_RC2_40_CBC _ | SHA_RC2_128_CBC _ ->
     Ok (Rc2.decrypt_cbc ~effective:(key_len * 8) ~key ~iv data)
   | SHA_RC4_40 _ | SHA_RC4_128 _ ->
     let open Mirage_crypto.Cipher_stream in
     let key = ARC4.of_secret key in
     let { ARC4.message ; _ } = ARC4.decrypt ~key data in
     Ok message
   | SHA_3DES_CBC _ ->
     let open Mirage_crypto.Cipher_block in
     let key = DES.CBC.of_secret key in
     Ok (DES.CBC.decrypt ~key ~iv data)
   | _ -> Error (`Msg "encryption algorithm not supported")) >>| fun data ->
  unpad data

let pkcs5_2_decrypt kdf enc password data =
  let open Rresult.R.Infix in
  (match enc with
   | Algorithm.AES128_CBC iv -> Ok (16l, iv)
   | Algorithm.AES192_CBC iv -> Ok (24l, iv)
   | Algorithm.AES256_CBC iv -> Ok (32l, iv)
   | _ -> Error (`Msg "unsupported encryption algorithm")) >>= fun (dk_len, iv) ->
  (match kdf with
   | Algorithm.PBKDF2 (salt, iterations, _ (* todo handle keylength *), prf) ->
     (match Algorithm.to_hmac prf with
      | Some prf -> Ok prf
      | None -> Error (`Msg "unsupported PRF")) >>| fun prf ->
     (salt, iterations, prf)
   | _ -> Error (`Msg "expected kdf being pbkdf2")) >>| fun (salt, count, prf) ->
  let password = Cstruct.of_string password in
  let key = Pbkdf.pbkdf2 ~prf ~password ~salt ~count ~dk_len in
  let key = Mirage_crypto.Cipher_block.AES.CBC.of_secret key in
  let msg = Mirage_crypto.Cipher_block.AES.CBC.decrypt ~key ~iv data in
  unpad msg

let pkcs5_2_encrypt (mac : [ `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512 ]) count algo password data =
  let bs = Mirage_crypto.Cipher_block.AES.CBC.block_size in
  let iv = Mirage_crypto_rng.generate bs in
  let enc, dk_len =
    match algo with
    | `AES128_CBC -> Algorithm.AES128_CBC iv, 16l
    | `AES192_CBC -> Algorithm.AES192_CBC iv, 24l
    | `AES256_CBC -> Algorithm.AES256_CBC iv, 32l
  in
  let password = Cstruct.of_string password in
  let salt = Mirage_crypto_rng.generate (Mirage_crypto.Hash.digest_size mac) in
  let key = Pbkdf.pbkdf2 ~prf:(mac :> Mirage_crypto.Hash.hash) ~password ~salt ~count ~dk_len in
  let key = Mirage_crypto.Cipher_block.AES.CBC.of_secret key in
  let padded_data = pad bs data in
  let enc_data =
    Mirage_crypto.Cipher_block.AES.CBC.encrypt ~key ~iv padded_data
  in
  let kdf = Algorithm.PBKDF2 (salt, count, None, Algorithm.of_hmac mac) in
  Algorithm.PBES2 (kdf, enc), enc_data

let decrypt algo password data =
  let open Algorithm in
  match algo with
  | SHA_RC4_128 _ | SHA_RC4_40 _
  | SHA_3DES_CBC _ | SHA_2DES_CBC _
  | SHA_RC2_128_CBC _ | SHA_RC2_40_CBC _ -> pkcs12_decrypt algo password data
  | PBES2 (kdf, enc) -> pkcs5_2_decrypt kdf enc password data
  | _ -> Error (`Msg "unsupported encryption algorithm")

let password_decrypt password (algo, data) =
  match data with
  | None -> Error (`Msg "no data to decrypt")
  | Some data -> decrypt algo password data

let verify password (data, ((algorithm, digest), salt, iterations)) =
  let open Rresult.R.Infix in
  (match Algorithm.to_hash algorithm with
   | Some hash -> Ok hash
   | None -> Error (`Msg "unsupported hash algorithm")) >>= fun hash ->
  let key =
    pbes hash `Hmac password salt iterations (Mirage_crypto.Hash.digest_size hash)
  in
  let computed = Mirage_crypto.Hash.mac hash ~key data in
  if Cstruct.equal computed digest then begin
    Asn_grammars.err_to_msg (Asn.auth_safe_of_cs data) >>= fun content ->
    List.fold_left (fun acc c ->
        acc >>= fun acc ->
        match c with
        | `Data data -> Ok (data :: acc)
        | `Encrypted data ->
          password_decrypt password data >>| fun data ->
          (data :: acc))
      (Ok []) content >>= fun safe_contents ->
    List.fold_left (fun acc cs ->
        acc >>= fun acc ->
        Asn_grammars.err_to_msg (Asn.safe_contents_of_cs cs) >>= fun bags ->
        List.fold_left (fun acc bag ->
            acc >>= fun acc ->
            match bag with
            | `Certificate c, _ -> Ok (`Certificate c :: acc)
            | `Crl c, _ -> Ok (`Crl c :: acc)
            | `Private_key p, _ -> Ok (`Private_key p :: acc)
            | `Encrypted_private_key (algo, enc_data), _ ->
              decrypt algo password enc_data >>= fun data ->
              Asn_grammars.err_to_msg (Private_key.Asn.private_of_cstruct data) >>= fun p ->
              Ok (`Decrypted_private_key p :: acc))
          (Ok acc) bags)
      (Ok []) safe_contents
  end else
    Error (`Msg "invalid signature")

let create ?(mac = `SHA256) ?(algorithm = `AES256_CBC) ?(iterations = 2048) password certificates private_key =
  let cert_sc =
    Asn.safe_contents_to_cs (List.map (fun c -> `Certificate c, None) certificates)
  and priv_sc =
    let data = Private_key.Asn.private_to_cstruct private_key in
    let algo, data = pkcs5_2_encrypt mac iterations algorithm password data in
    Asn.safe_contents_to_cs [ `Encrypted_private_key (algo, data), None ]
  in
  let cert_sc_enc =
    let algo, data = pkcs5_2_encrypt mac iterations algorithm password cert_sc in
    algo, Some data
  in
  let auth_data =
    Asn.auth_safe_to_cs [ `Encrypted cert_sc_enc ; `Data priv_sc ]
  in
  let mac_size = Mirage_crypto.Hash.digest_size mac in
  let salt = Mirage_crypto_rng.generate mac_size in
  let key = pbes mac `Hmac password salt iterations mac_size in
  let digest = Mirage_crypto.Hash.mac mac ~key auth_data in
  auth_data, ((Algorithm.of_hash mac, digest), salt, iterations)

let decode_der cs = Asn_grammars.err_to_msg (Asn.pfx_of_cs cs)

let encode_der = Asn.pfx_to_cs
