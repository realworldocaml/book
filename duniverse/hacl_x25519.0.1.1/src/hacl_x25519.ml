let key_length_bytes = 32

let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

let ( >>| ) r f =
  r >>= fun x ->
  Ok (f x)

type error = [ `Invalid_length | `Low_order ]

let pp_error ppf = function
  | `Invalid_length -> Format.fprintf ppf "Invalid key size"
  | `Low_order -> Format.fprintf ppf "Public key with low order"

type secret = [ `Checked of Cstruct.t ]

let key_of_cstruct cs =
  if Cstruct.len cs = key_length_bytes then Ok (`Checked cs)
  else Error `Invalid_length

(* pk -> sk -> basepoint -> unit *)
external scalarmult_raw :
  Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit
  = "ml_Hacl_Curve25519_crypto_scalarmult"
  [@@noalloc]

let checked_buffer (`Checked cs) = Cstruct.to_bigarray cs

let key_exchange_buffer ~priv ~checked_pub =
  let cs = Cstruct.create key_length_bytes in
  let result = `Checked cs in
  scalarmult_raw (checked_buffer result) (checked_buffer priv) checked_pub;
  cs

let all_zeroes = String.make key_length_bytes '\x00'

let is_zero_output cs =
  let s = Cstruct.to_string cs in
  Eqaf.equal s all_zeroes

let key_exchange priv pub =
  key_of_cstruct pub >>| checked_buffer >>= fun checked_pub ->
  let shared = key_exchange_buffer ~priv ~checked_pub in
  if is_zero_output shared then Error `Low_order else Ok shared

let basepoint =
  let cs = Cstruct.create key_length_bytes in
  Cstruct.set_uint8 cs 0 9;
  Cstruct.to_bigarray cs

let public priv = key_exchange_buffer ~priv ~checked_pub:basepoint

let gen_key ~rng =
  let secret_cstruct = rng key_length_bytes in
  let secret =
    match key_of_cstruct secret_cstruct with
    | Ok k -> k
    | Error `Invalid_length ->
        failwith "Hacl_x25519.gen_key: generator returned an invalid length"
  in
  let pub_key = public secret in
  (secret, pub_key)
