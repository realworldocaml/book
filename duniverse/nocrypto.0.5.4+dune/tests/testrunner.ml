
open OUnit2

(* Gather quantum uncertainty. *)
(* let () = *)
(*   let t  = Unix.gettimeofday () in *)
(*   let cs = Cstruct.create 8 in *)
(*   Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ; *)
(*   Nocrypto.Rng.reseed cs *)

let () = Nocrypto_entropy_unix.initialize ()

let () =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    Nocrypto.Cipher_block.accelerated

let () =
(*   Nocrypto.Rng.reseed @@ Cstruct.of_string "\001\002\003\004" ; *)
  run_test_tt_main Testlib.suite
