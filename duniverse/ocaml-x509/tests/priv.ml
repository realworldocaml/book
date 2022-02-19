open X509

let pk_equal a b =
  Cstruct.equal
    (Mirage_crypto.Hash.SHA256.digest (Private_key.encode_der a))
    (Mirage_crypto.Hash.SHA256.digest (Private_key.encode_der b))

let generate_rsa () =
  let seed = "Test1234" in
  let pk = Private_key.generate ~seed:(Cstruct.of_string seed) `RSA in
  let pk' = Result.get_ok (Private_key.of_string `RSA seed) in
  let pk'' = Result.get_ok (Private_key.of_string ~seed_or_data:`Seed `RSA seed) in
  Alcotest.(check bool "generate and of_string" true (pk_equal pk pk'));
  Alcotest.(check bool "generate and of_string ~seed" true (pk_equal pk pk''));
  match Private_key.of_string ~seed_or_data:`Data `RSA seed with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected failure (of_string `Data `RSA)"

let b64_dec s = Base64.decode_exn s |> Cstruct.of_string

let test_ec (key_type, data) () =
  let pk = Result.get_ok (Private_key.of_cstruct (b64_dec data) key_type) in
  let pk' = Result.get_ok (Private_key.of_string key_type data) in
  let pk'' = Result.get_ok (Private_key.of_string ~seed_or_data:`Data key_type data) in
  Alcotest.(check bool "generate and of_string" true (pk_equal pk pk'));
  Alcotest.(check bool "generate and of_string ~data" true (pk_equal pk pk''));
  match Private_key.of_string ~seed_or_data:`Seed key_type data with
  | Error _ -> Alcotest.fail "expected ok (of_string `Seed)"
  | Ok pk''' -> Alcotest.(check bool "generate and of_String ~seed" false (pk_equal pk pk'''))

let ec_data = [
  `ED25519, "W0p4c4tBHtSaTj4zij4oARCjhFbIi8voYg+65bl7wLU=" ;
  `P224, "Wjy6Nf4/xJSaaR/eeoQBUxJMA3PDP/c+8VkuPA==" ;
  `P256, "arvDmHpdTdzbc0uo+KCXoArmrmAs2GAvfk14D8gi6gM=" ;
  `P384, "UEZz/xVx2f3s7W8/cFy/w38LkjAq0xfMYJiXamdwgW9zwSK18+vrhKzgE23sFnyq" ;
  `P521, "AVb4DIpMO5hzyfX1n4qi4xtj/JBDCTCwyOLasKnnVS6FHW2hEZbGwd1c2J4rwpNKZqTKNsKu3dVJAmlp3EFhqv5T" ;
]

let tests =
  ("Generate RSA", `Quick, generate_rsa) ::
  List.map (fun d -> Key_type.to_string (fst d), `Quick, test_ec d) ec_data
