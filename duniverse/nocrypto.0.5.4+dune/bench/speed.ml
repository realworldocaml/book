open Nocrypto

open Uncommon
open Cipher_block
open Hash


module Time = struct

  let time ~n f a =
    let t1 = Sys.time () in
    for _ = 1 to n do ignore (f a) done ;
    let t2 = Sys.time () in
    (t2 -. t1)

  let warmup () =
    let x = ref 0 in
    let rec go start =
      if Sys.time () -. start < 1. then begin
        for i = 0 to 10000 do x := !x + i done ;
        go start
      end in
    go (Sys.time ())

end


let _ = Rng.reseed (Cstruct.of_string "abcd")

let burn_period = 2.0

let sizes = [16; 64; 256; 1024; 8192]
(* let sizes = [16] *)

let burn f n =
  let cs = Rng.generate n in
  let (t1, i1) =
    let rec loop it =
      let t = Time.time ~n:it f cs in
      if t > 0.2 then (t, it) else loop (it * 10) in
    loop 10 in
  let iters = int_of_float (float i1 *. burn_period /. t1) in
  let time  = Time.time ~n:iters f cs in
  (iters, time, float (n * iters) /. time)

let mb = 1024. *. 1024.

let throughput title f =
  Printf.printf "\n* [%s]\n%!" title ;
  sizes |> List.iter @@ fun size ->
    Gc.full_major () ;
    let (iters, time, bw) = burn f size in
    Printf.printf "    % 5d:  %04f MB/s  (%d iters in %.03f s)\n%!"
      size (bw /. mb) iters time

let bm name f = (name, fun () -> f name)

let benchmarks = [

  bm "aes-128-ecb" (fun name ->
    let key = AES.ECB.of_secret (Rng.generate 16) in
    throughput name (fun cs -> AES.ECB.encrypt ~key cs)) ;

  bm "aes-128-cbc-e" (fun name ->
    let key = AES.CBC.of_secret (Rng.generate 16)
    and iv  = Rng.generate 16 in
    throughput name (fun cs -> AES.CBC.encrypt ~key ~iv cs)) ;

  bm "aes-128-cbc-d" (fun name ->
    let key = AES.CBC.of_secret (Rng.generate 16)
    and iv  = Rng.generate 16 in
    throughput name (fun cs -> AES.CBC.decrypt ~key ~iv cs)) ;

  bm "aes-128-ctr" (fun name ->
    let key = Rng.generate 16 |> AES.CTR.of_secret
    and ctr = Rng.generate 16 |> AES.CTR.ctr_of_cstruct in
    throughput name (fun cs -> AES.CTR.encrypt ~key ~ctr cs)) ;

  bm "aes-128-gcm" (fun name ->
    let key = AES.GCM.of_secret (Rng.generate 16)
    and iv  = Rng.generate 12 in
    throughput name (fun cs -> AES.GCM.encrypt ~key ~iv cs));

  bm "aes-128-ghash" (fun name ->
    let key = AES.GCM.of_secret (Rng.generate 16)
    and iv  = Rng.generate 12 in
    throughput name (fun cs -> AES.GCM.encrypt ~key ~iv ~adata:cs Cs.empty));

  bm "aes-128-ccm" (fun name ->
    let key   = AES.CCM.of_secret ~maclen:16 (Rng.generate 16)
    and nonce = Rng.generate 10 in
    throughput name (fun cs -> AES.CCM.encrypt ~key ~nonce cs));

  bm "aes-192-ecb" (fun name ->
    let key = AES.ECB.of_secret (Rng.generate 24) in
    throughput name (fun cs -> AES.ECB.encrypt ~key cs)) ;

  bm "aes-256-ecb" (fun name ->
    let key = AES.ECB.of_secret (Rng.generate 32) in
    throughput name (fun cs -> AES.ECB.encrypt ~key cs)) ;

  bm "d3des-ecb" (fun name ->
    let key = DES.ECB.of_secret (Rng.generate 24) in
    throughput name (fun cs -> DES.ECB.encrypt ~key cs)) ;

  bm "fortuna" (fun name ->
    let open Rng.Generators.Fortuna in
    let g = create () in
    reseed ~g (Cstruct.of_string "abcd") ;
    throughput name (fun cs -> generate ~g (Cstruct.len cs))) ;

  bm "md5"    (fun name -> throughput name MD5.digest) ;
  bm "sha1"   (fun name -> throughput name SHA1.digest) ;
  bm "sha256" (fun name -> throughput name SHA256.digest) ;
  bm "sha512" (fun name -> throughput name SHA512.digest) ;
]

let help () =
  Printf.printf "available benchmarks:\n  ";
  List.iter (fun (n, _) -> Printf.printf "%s  " n) benchmarks ;
  Printf.printf "\n%!"

let runv fs =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    Nocrypto.Cipher_block.accelerated;
  Time.warmup () ;
  List.iter (fun f -> f ()) fs


let _ =
  match Array.to_list Sys.argv with
  | _::(_::_ as args) -> begin
      try
        let fs =
          args |> List.map @@ fun n ->
            snd (benchmarks |> List.find @@ fun (n1, _) -> n = n1) in
        runv fs
      with Not_found -> help ()
    end
  | _ -> help ()
