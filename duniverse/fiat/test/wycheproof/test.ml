open Wycheproof

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

let parse_asn1 s =
  let cs = Cstruct.of_string s in
  let seq2 a b = Asn.S.(sequence2 (required a) (required b)) in
  let term = Asn.S.(seq2 (seq2 oid oid) bit_string_cs) in
  let ec_public_key = Asn.OID.(base 1 2 <|| [ 840; 10045; 2; 1 ]) in
  let prime256v1 = Asn.OID.(base 1 2 <|| [ 840; 10045; 3; 1; 7 ]) in
  match Asn.decode (Asn.codec Asn.ber term) cs with
  | Error _ -> Error "ASN1 parse error"
  | Ok (((oid1, oid2), data), rest) ->
      if Cstruct.len rest <> 0 then Error "ASN1 leftover"
      else if not (Asn.OID.equal oid1 ec_public_key) then
        Error "ASN1: wrong oid 1"
      else if not (Asn.OID.equal oid2 prime256v1) then
        Error "ASN1: wrong oid 2"
      else Ok (Cstruct.to_string data)

let ( >>= ) xr f = match xr with Error _ as e -> e | Ok x -> f x

let parse_point p =
  parse_asn1 p >>= fun h ->
  Ok Hex.(to_cstruct (of_string h))

let to_string_result ~pp_error = function
  | Ok _ as ok -> ok
  | Error e ->
      let msg = Format.asprintf "%a" pp_error e in
      Error msg

let strip_leading_zeroes cs =
  let first_nonzero_index = ref None in
  let cs_len = Cstruct.len cs in
  for i = cs_len - 1 downto 0 do
    if Cstruct.get_uint8 cs i <> 0 then first_nonzero_index := Some i
  done;
  match !first_nonzero_index with
  | None -> Cstruct.empty
  | Some i ->
      let off = i in
      let len = cs_len - i in
      Cstruct.sub cs off len

let pad ~total_len cs =
  match total_len - Cstruct.len cs with
  | 0 -> Ok cs
  | n when n < 0 -> Error "input is too long"
  | pad_len -> Ok (Cstruct.append cs (Cstruct.create pad_len))

let parse_secret s =
  let stripped = strip_leading_zeroes (Cstruct.of_string s) in
  pad ~total_len:32 (Cstruct.rev stripped) >>= fun cs ->
  Ok (Cstruct.rev cs)

type test = {
  public_key : Cstruct.t;
  raw_private_key : Cstruct.t;
  expected : string;
}

let perform_key_exchange ~public_key ~raw_private_key =
  let open Fiat_p256 in
  let gen _ = raw_private_key in
  let secret, _ = gen_key ~rng:gen in
  to_string_result ~pp_error (key_exchange secret public_key)

let interpret_test ~tcId { public_key; raw_private_key; expected } () =
  match perform_key_exchange ~public_key ~raw_private_key with
  | Ok cs ->
      let got = Cstruct.to_string cs in
      Alcotest.check hex __LOC__ expected got
  | Error err -> Printf.ksprintf Alcotest.fail "While parsing %d: %s" tcId err

type invalid_test = { public : string; private_ : string }

let is_ok = function Ok _ -> true | Error _ -> false

let interpret_invalid_test { public; private_ } () =
  let result =
    parse_point public >>= fun public_key ->
    parse_secret private_ >>= fun raw_private_key ->
    perform_key_exchange ~public_key ~raw_private_key
  in
  Alcotest.check Alcotest.bool __LOC__ false (is_ok result)

type strategy = Test of test | Invalid_test of invalid_test | Skip

let make_test test =
  let ignored_flags = [ "CompressedPoint"; "UnnamedCurve" ] in
  match test.result with
  | _ when has_ignored_flag test ~ignored_flags -> Ok Skip
  | Invalid ->
      Ok (Invalid_test { public = test.public; private_ = test.private_ })
  | Acceptable -> Ok Skip
  | Valid ->
      parse_point test.public >>= fun public_key ->
      parse_secret test.private_ >>= fun raw_private_key ->
      Ok (Test { public_key; raw_private_key; expected = test.shared })

let concat_map f l = List.map f l |> List.concat

let to_tests x =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  match make_test x with
  | Ok (Test t) -> [ (name, `Quick, interpret_test ~tcId:x.tcId t) ]
  | Ok (Invalid_test t) -> [ (name, `Quick, interpret_invalid_test t) ]
  | Ok Skip -> []
  | Error e -> Printf.ksprintf failwith "While parsing %d: %s" x.tcId e

let tests =
  let data = load_file_exn "ecdh_secp256r1_test.json" in
  concat_map (fun group -> concat_map to_tests group.tests) data.testGroups

let () = Alcotest.run "Wycheproof-hacl-p256" [ ("test vectors", tests) ]
