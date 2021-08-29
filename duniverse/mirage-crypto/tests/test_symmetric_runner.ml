open OUnit2

(* Gather quantum uncertainty. *)
(* let () = *)
(*   let t  = Unix.gettimeofday () in *)
(*   let cs = Cstruct.create 8 in *)
(*   Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ; *)
(*   Mirage_crypto_rng.reseed cs *)

let () =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    Mirage_crypto.Cipher_block.accelerated

let suite =
  "All" >::: [
    "Basic" >::: Test_base.suite;
    "Hash" >::: Test_hash.suite;
    "Hmac" >::: Test_hmac.suite;
    "Cipher" >::: Test_cipher.suite;
  ]

let () =
  run_test_tt_main suite
