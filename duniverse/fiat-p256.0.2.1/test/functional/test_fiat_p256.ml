module Testable = struct
  let cstruct =
    let pp fmt t = Hex.pp fmt (Hex.of_cstruct t) in
    Alcotest.testable pp Cstruct.equal
end

let list_init l f =
  let rec go acc i = if i = l then List.rev acc else go (f i :: acc) (i + 1) in
  go [] 0

let int32_to_hex i = Printf.sprintf "%08Lx" i

let prng len =
  let i32_count = (len / 4) + 1 in
  let i32s = list_init i32_count (fun _ -> Random.int64 0x100000000L) in
  let as_hex = String.concat "" (List.map int32_to_hex i32s) in
  Cstruct.of_hex (String.sub as_hex 0 (len * 2))

let whole_key_exchange =
  let open Rresult.R.Infix in
  let test_name = "whole_key_exchange" in
  let test_fun () =
    let res =
      let secret1, public1 = Fiat_p256.gen_key ~rng:prng in
      let secret2, public2 = Fiat_p256.gen_key ~rng:prng in
      Fiat_p256.key_exchange secret1 public2 >>= fun shared1 ->
      Fiat_p256.key_exchange secret2 public1 >>= fun shared2 ->
      Ok (shared1, shared2)
    in
    match res with
    | Ok (s1, s2) -> Alcotest.check Testable.cstruct test_name s1 s2
    | Error e ->
        Alcotest.failf "Key exchange failed with error %a" Fiat_p256.pp_error e
  in
  [ (test_name, `Quick, test_fun) ]

let () =
  Random.self_init ();
  Alcotest.run "Fiat_p256" [ ("Functional", whole_key_exchange) ]
