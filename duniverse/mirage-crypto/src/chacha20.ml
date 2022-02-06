(* Based on https://github.com/abeaumont/ocaml-chacha.git *)

open Uncommon

let block = 64

type key = Cstruct.t

let of_secret a = a

let chacha20_block state idx key_stream =
  Native.Chacha.round 10 state.Cstruct.buffer 0 key_stream.Cstruct.buffer idx

let init ctr ~key ~nonce =
  let ctr_off = 48 in
  let set_ctr32 b v = Cstruct.LE.set_uint32 b ctr_off v
  and set_ctr64 b v = Cstruct.LE.set_uint64 b ctr_off v
  in
  let inc32 b = set_ctr32 b (Int32.add (Cstruct.LE.get_uint32 b ctr_off) 1l)
  and inc64 b = set_ctr64 b (Int64.add (Cstruct.LE.get_uint64 b ctr_off) 1L)
  in
  let s, key, init_ctr, nonce_off, inc =
    match Cstruct.length key, Cstruct.length nonce, Int64.shift_right ctr 32 = 0L with
    | 32, 12, true ->
      let ctr = Int64.to_int32 ctr in
      "expand 32-byte k", key, (fun b -> set_ctr32 b ctr), 52, inc32
    | 32, 12, false ->
      invalid_arg "Counter too big for IETF mode (32 bit counter)"
    | 32, 8, _ ->
      "expand 32-byte k", key, (fun b -> set_ctr64 b ctr), 56, inc64
    | 16, 8, _ ->
      let k = Cstruct.append key key in
      "expand 16-byte k", k, (fun b -> set_ctr64 b ctr), 56, inc64
    | _ -> invalid_arg "Valid parameters are nonce 12 bytes and key 32 bytes \
                        (counter 32 bit), or nonce 8 byte and key 16 or 32 \
                        bytes (counter 64 bit)."
  in
  let state = Cstruct.create block in
  Cstruct.blit_from_string s 0 state 0 16 ;
  Cstruct.blit key 0 state 16 32 ;
  init_ctr state ;
  Cstruct.blit nonce 0 state nonce_off (Cstruct.length nonce) ;
  state, inc

let crypt ~key ~nonce ?(ctr = 0L) data =
  let state, inc = init ctr ~key ~nonce in
  let l = Cstruct.length data in
  let block_count = l // block in
  let len = block * block_count in
  let last_len =
    let last = l mod block in
    if last = 0 then block else last
  in
  let key_stream = Cstruct.create_unsafe len in
  let rec loop i = function
    | 1 ->
      chacha20_block state i key_stream ;
      Native.xor_into data.buffer (data.off + i) key_stream.buffer i last_len
    | n ->
      chacha20_block state i key_stream ;
      Native.xor_into data.buffer (data.off + i) key_stream.buffer i block ;
      inc state;
      loop (i + block) (n - 1)
  in
  loop 0 block_count ;
  Cstruct.sub key_stream 0 l

module P = Poly1305.It

let generate_poly1305_key ~key ~nonce =
  crypt ~key ~nonce (Cstruct.create 32)

let mac ~key ~adata ciphertext =
  let pad16 b =
    let len = Cstruct.length b mod 16 in
    if len = 0 then Cstruct.empty else Cstruct.create (16 - len)
  and len =
    let data = Cstruct.create 16 in
    Cstruct.LE.set_uint64 data 0 (Int64.of_int (Cstruct.length adata));
    Cstruct.LE.set_uint64 data 8 (Int64.of_int (Cstruct.length ciphertext));
    data
  in
  let ctx = P.empty ~key in
  let ctx = P.feed ctx adata in
  let ctx = P.feed ctx (pad16 adata) in
  let ctx = P.feed ctx ciphertext in
  let ctx = P.feed ctx (pad16 ciphertext) in
  let ctx = P.feed ctx len in
  P.get ctx

let authenticate_encrypt ~key ~nonce ?(adata = Cstruct.empty) data =
  let poly1305_key = generate_poly1305_key ~key ~nonce in
  let ciphertext = crypt ~key ~nonce ~ctr:1L data in
  let mac = mac ~key:poly1305_key ~adata ciphertext in
  Cstruct.append ciphertext mac

let authenticate_decrypt ~key ~nonce ?(adata = Cstruct.empty) data =
  if Cstruct.length data < P.mac_size then
    None
  else
    let cipher, tag = Cstruct.split data (Cstruct.length data - P.mac_size) in
    let poly1305_key = generate_poly1305_key ~key ~nonce in
    let ctag = mac ~key:poly1305_key ~adata cipher in
    let plain = crypt ~key ~nonce ~ctr:1L cipher in
    if Eqaf_cstruct.equal tag ctag then Some plain else None
