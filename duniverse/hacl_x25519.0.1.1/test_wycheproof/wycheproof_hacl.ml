open Wycheproof

let check pp equal name expected got =
  if equal expected got then Format.printf "%s - ok\n" name
  else
    Format.printf "%s - error\nexpected:\n%a\ngot:\n%a\n" name pp expected pp
      got

let get_ok = function Ok x -> x | Error _ -> assert false

let key_pair_of_cstruct data = Hacl_x25519.gen_key ~rng:(fun _ -> data)

let priv_of_string s = key_pair_of_cstruct (Cstruct.of_string s) |> fst

let key_exchange ~priv ~pub =
  match
    Hacl_x25519.key_exchange (priv_of_string priv) (Cstruct.of_string pub)
  with
  | Ok cs -> Ok (Cstruct.to_string cs)
  | Error `Low_order -> Ok (String.make 32 '\x00')
  | Error _ as e -> e

let run_test { tcId; comment; private_; public; shared = expected; _ } =
  let name = Printf.sprintf "%d - %s" tcId comment in
  let got = get_ok (key_exchange ~priv:private_ ~pub:public) in
  check Wycheproof.pp_hex Wycheproof.equal_hex name expected got

let () =
  List.iter (fun group -> List.iter run_test group.tests) x25519.testGroups
