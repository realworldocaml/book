open Core.Std
open Cryptokit

let md5 s = hash_string (Hash.md5 ()) s
let hex s = String.uppercase (transform_string (Hexa.encode ()) s)

let evp_byte_to_key password tlen =
  let rec aux acc v =
    match String.length acc < tlen with
    |true ->
      let v = md5 (v ^ password) in
      aux (acc^v) v
    |false -> acc
  in
  let v = md5 password in
  hex (aux v v)

let evp_byte_to_key password tlen =
  let o = Hexa.encode () in
  let v = ref (md5 password) in
  o#put_string !v;
  while o#available_output/2 < tlen do
    let n = md5 (!v ^ password) in
    o#put_string n;
    v := n;
  done;
  String.uppercase o#get_string
 
let _ =
  let secret = "ocaml" in
  let key_len = 16 * 2 in
  let iv_len = 16 * 2 in
  let x = evp_byte_to_key secret (key_len+iv_len) in
  let key = String.sub x ~pos:0 ~len:key_len in
  let iv = String.sub x ~pos:key_len ~len:iv_len in
  Printf.printf "key=%s\niv =%s\n%!" key iv
