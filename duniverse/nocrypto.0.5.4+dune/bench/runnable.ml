
open Lwt
open Nocrypto
open Nocrypto.Uncommon


let time f =
  let t1 = Sys.time () in
  let r  = f () in
  let t2 = Sys.time () in
  Printf.printf "[time] %.04f sec\n%!" (t2 -. t1) ;
  r

let rec replicate f = function
  | 0 -> []
  | n -> let x = f () in x :: replicate f (pred n)

let rec forever f = ( ignore @@ f () ; forever f )

(*

let random_string () =
  let len = Random.int 1024 in
  let str = String.create len in
  for i = 0 to len - 1 do
    str.[i] <- char_of_int (Random.int 256)
  done;
  str

let head_to_head () =
  let key = random_string ()
  and msg = random_string () in
  let h1 =
    Cstruct.of_string
      Cryptokit.(hash_string (MAC.hmac_sha1 key) msg)
  and h2 =
    Hmac.sha1 ~key:(Cstruct.of_string key)
                   (Cstruct.of_string msg) in
  h1 = h2

let () =
  let cs  = Cstruct.of_string "desu"
  and key = Cstruct.of_string "sekrit" in
  Cstruct.hexdump (Hash.sha1 cs);
  Cstruct.hexdump (Hash.md5 cs);
  Cstruct.hexdump (Hmac.sha1 ~key cs);
  Cstruct.hexdump (Hmac.md5 ~key cs);

*)

let cs_of_s = Cstruct.of_string


let on_stdin fn =
  lwt input = Lwt_io.(read stdin) in
  let res = fn (Cstruct.of_string input) in
  let b   = Buffer.create 16 in
  Cstruct.hexdump_to_buffer b res;
  Lwt_io.printl (Buffer.contents b)

(* let aes_key str = Block.AES.of_secret (Cstruct.of_string str) *)

let main () =
  match List.tl (Array.to_list Sys.argv) with

  | ["md5"]       -> on_stdin Hash.MD5.digest

  | ["sha1"]      -> on_stdin Hash.SHA1.digest

  | ["hmac_md5" ; k] -> on_stdin @@ Hash.MD5.hmac ~key:(cs_of_s k)

  | ["hmac_sha1"; k] -> on_stdin @@ Hash.SHA1.hmac ~key:(cs_of_s k)

(*   | ["aes_ecb"; "encrypt"; k] -> on_stdin Block.AES.(encrypt_ecb ~key:(aes_key k))

  | ["aes_ecb"; "decrypt"; k] -> on_stdin Block.AES.(decrypt_ecb ~key:(aes_key k))

  | ["aes_cbc"; "encrypt"; k; iv] -> on_stdin @@ fun i ->
      snd Block.AES.(encrypt_cbc ~key:(aes_key k) ~iv:(cs_of_s iv) i)

  | ["aes_cbc"; "decrypt"; k; iv] -> on_stdin @@ fun i ->
      snd Block.AES.(decrypt_cbc ~key:(aes_key k) ~iv:(cs_of_s iv) i) *)

  | _ ->
      Printf.eprintf
        "%s: [ md5 | sha1 | hmac_md5 <key> | hmac_sha1 <key> ]\n%!"
        Sys.argv.(0);
      exit 1

(* let () = Lwt_main.run @@ main () *)

let time_fortuna_generation () =
  let g = Fortuna.create () in
  Fortuna.reseed g (Cstruct.of_string "\001\002\003\004");
  let _ = time @@ fun () ->
    for i = 1 to 10 do
      ignore @@ Fortuna.generate g (int_of_float @@ 10. *. (2.**20.))
    done in
  ()

let time_intgen_with_misses () =
  Rng.reseed (Cstruct.of_string "\001\002\003\004");
  let _ = time @@ fun () ->
    for i = 1 to 1000000 do
      ignore @@ Rng.Int.gen 0x2000000000000001
    done in
  ()

let time_z_of_bits () =
  Rng.reseed (Cstruct.of_string "\001\002\003\004");
  let items = 10000000 in
  let cs    = time @@ fun () -> Rng.generate (items * 8) in
  time @@ fun () ->
    let rec loop cs = function
      | 0 -> ()
      | n ->
          ignore (Numeric.Z.of_cstruct_be ~bits:(7 * 8 + 3) cs);
          loop (Cstruct.shift cs 8) (pred n) in
    loop cs items

let time_rsa_generate () =
  Rng.reseed (Cstruct.of_string "\001\002\003\004");
  let items = 100 in
  time @@ fun () ->
    for i = 1 to items do
      ignore @@ Rsa.generate 2048
    done


let rsa_feedback bits =
  let open Cstruct in
  let open Rsa in

  let def_e   = Z.of_int 0x10001 in

  let m = Rng.generate (bits / 8 - 1) in
  Cstruct.(set_uint8 m 0 (max 1 (get_uint8 m 0)));
  hexdump m ;

  let e = if Z.(pow ~$2 bits < def_e) then Z.of_int 3 else def_e in
  let key =
    Printf.printf "+ generating...\n%!";
    generate ~e bits in
  Printf.printf "%s\n%!"
    (Sexplib.Sexp.to_string_hum (sexp_of_priv key));

  let c =
    Printf.printf "+ encrypt...\n%!";
    encrypt ~key:(pub_of_priv key) m in
  hexdump c ;

  let d =
    Printf.printf "+ decrypt...\n%!";
    decrypt ~key c in
  hexdump d ;

  assert (Cstruct.equal m d) ;
  Printf.printf "* \n%!"


let dh_feedback bits =
  let p = Dh.gen_group bits in
(*   let p = Dh.Group.rfc_5114_3 in *)

  let (s1, m1) = Dh.gen_secret p
  and (s2, m2) = Dh.gen_secret p in

  let sh1 = Dh.shared p s1 m2
  and sh2 = Dh.shared p s2 m1 in

  assert (Cstruct.equal sh1 sh2);
  Cstruct.hexdump sh1

(* let _ =
  Rng.reseed (Cstruct.of_string "\001\002\003\004");
  forever (fun () -> dh_feedback 1024) *)
(*   forever (fun () -> rsa_feedback 2048) *)


(*
 * ECB ctypes parith: 2.53
 * ECB ctypes to_bytestring: 4.05
 * ECB stubs: 3.11
 *
 * CBC ctypes parith: 2.72
 * CBC ctypes to_bytestring: 4.13
 * CBC stubs: 3.30
 *)

(* let _ =
  Rng.reseed (Cstruct.of_string "\000");
  let cs  = Rng.generate (16 * 1000000)
  and iv  = Cstruct.of_string "abcdABCDefghEFGH"
  and key = Block.AES.CBC.of_secret (Cstruct.of_string "desu1234desu1234") in
  time @@ fun () ->
    for x = 1 to 10 do
      ignore @@ Block.AES.CBC.encrypt ~key ~iv cs
    done *)


(*
 * SHA1 ctypes parith: 1.5967
 * SHA1 ctypes to_bytestring: 4.6133
 * SHA1 stubs: 2.54
 *)

(* let _ =
  let rngs size n =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (Rng.generate size :: acc) (pred n) in
    loop [] n
  in
  Rng.reseed (Cstruct.of_string "\000");
  let css = rngs 16 1000000 in
  time @@ fun () ->
    for x = 1 to 5 do
      ignore @@ Hash.SHA1.digestv css
    done *)


(* let _ =
  Rng.reseed (Cstruct.of_string "\001\002\003\004");
  let x = Rng.generate 10000000
  and y = Rng.generate 10000000 in
  time @@ fun () ->
    for i = 1 to 100 do ignore @@ Common.Cs.xor_into x y 10000000 done *)

