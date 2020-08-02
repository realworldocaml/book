module Testable = struct
  let fiat_error = Alcotest.testable Fiat_p256.pp_error ( = )

  let ok_or_error = Alcotest.result Alcotest.unit fiat_error
end

let key_pair_of_hex h = Fiat_p256.gen_key ~rng:(fun _ -> Hex.to_cstruct h)

let scalar_of_hex h = fst (key_pair_of_hex h)

let pp_hex_le fmt cs =
  let n = Cstruct.len cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done

let pp_result ppf = function
  | Ok cs -> pp_hex_le ppf cs
  | Error e -> Format.fprintf ppf "%a" Fiat_p256.pp_error e

let key_exchange =
  let test ~name d p ~expected =
    ( name,
      `Quick,
      fun () ->
        Fiat_p256.key_exchange d p
        |> Format.asprintf "%a" pp_result
        |> Alcotest.check Alcotest.string __LOC__ expected )
  in
  let d_a, p_a =
    key_pair_of_hex
      (`Hex "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let d_b, p_b =
    key_pair_of_hex
      (`Hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  [ test ~name:"b*A" d_b p_a
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"a*B" d_a p_b
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"a*A" d_a p_a
      ~expected:
        "2ea4e810837da217a5bfd05f01d12459eeda830b6e0dec7f8afa425c5b55c507";
    test ~name:"b*B" d_b p_b
      ~expected:
        "a7666bcc3818472194460f7df22d80a5886da0e1679eac930175ce1ff733c7ca"
  ]

let scalar_mult =
  let test ~n ~scalar ~point ~expected =
    let scalar = scalar_of_hex scalar in
    let point = Hex.to_cstruct point in
    ( Printf.sprintf "Scalar mult (#%d)" n,
      `Quick,
      fun () ->
        Fiat_p256.key_exchange scalar point
        |> Format.asprintf "%a" pp_result
        |> Alcotest.check Alcotest.string __LOC__ expected )
  in
  let point =
    `Hex
      "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
  in
  [ test ~n:0
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000001")
      ~point
      ~expected:
        "96c298d84539a1f4a033eb2d817d0377f240a463e5e6bcf847422ce1f2d1176b";
    test ~n:1
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000002")
      ~point
      ~expected:
        "78996647fc480ba6351bf277e26989c0c31ab5040338528a7e4f038d187bf27c";
    test ~n:2
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000004")
      ~point
      ~expected:
        "5208036b44029350ef965578dbe21f03d02be69e65de2da0bb8fd032354a53e2";
    test ~n:3
      ~scalar:
        (`Hex
          "0612465c89a023ab17855b0a6bcebfd3febb53aef84138647b5352e02c10c346")
      ~point:
        (`Hex
          "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf")
      ~expected:
        "854271e19508bc935ab22b95cd2be13a0e78265f528b658b3219028b900d0253";
    test ~n:4
      ~scalar:
        (`Hex
          "0a0d622a47e48f6bc1038ace438c6f528aa00ad2bd1da5f13ee46bf5f633d71a")
      ~point:
        (`Hex
          "043cbc1b31b43f17dc200dd70c2944c04c6cb1b082820c234a300b05b7763844c74fde0a4ef93887469793270eb2ff148287da9265b0334f9e2609aac16e8ad503")
      ~expected:
        "ffffffffffffffffffffffffffffffff3022cfeeffffffffffffffffffffff7f";
    test ~n:5
      ~scalar:
        (`Hex
          "55d55f11bb8da1ea318bca7266f0376662441ea87270aa2077f1b770c4854a48")
      ~point:
        (`Hex
          "04000000000000000000000000000000000000000000000000000000000000000066485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4")
      ~expected:
        "48e82c9b82c88cb9fc2a5cff9e7c41bc4255ff6bd3814538c9b130877c07e4cf"
  ]

let to_ok_or_error = function Ok _ -> Ok () | Error _ as e -> e

let point_validation =
  let test ~name ~x ~y ~expected =
    let scalar =
      scalar_of_hex
        (`Hex
          "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
    in
    let point =
      Cstruct.concat [ Cstruct.of_hex "04"; Hex.to_cstruct x; Hex.to_cstruct y ]
    in
    ( name,
      `Quick,
      fun () ->
        Fiat_p256.key_exchange scalar point
        |> to_ok_or_error
        |> Alcotest.check Testable.ok_or_error __LOC__ expected )
  in
  let zero = `Hex (String.make 64 '0') in
  let sb =
    `Hex "66485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4"
  in
  [ test ~name:"Ok"
      ~x:
        (`Hex
          "62d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26")
      ~y:
        (`Hex
          "ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf")
      ~expected:(Ok ());
    test ~name:"P=0"
      ~x:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000000")
      ~y:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000000")
      ~expected:(Error `Not_on_curve);
    test ~name:"(0, sqrt(b))" ~x:zero ~y:sb ~expected:(Ok ());
    test ~name:"out of range"
      ~x:
        (`Hex
          "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF")
      ~y:sb
      ~expected:(Error `Invalid_range)
  ]

let scalar_validation =
  let test_scalar_validation ~name ~scalar ~expected =
    let safe =
      Cstruct.of_hex
        "0000000000000000000000000000000000000000000000000000000000000001"
    in
    let ncalls = ref 0 in
    let return_value = ref (Some (Hex.to_cstruct scalar)) in
    let rng _ =
      incr ncalls;
      match !return_value with
      | None -> safe
      | Some rv ->
          return_value := None;
          rv
    in
    ( name,
      `Quick,
      fun () ->
        let _, _ = Fiat_p256.gen_key ~rng in
        let got = !ncalls in
        Alcotest.check Alcotest.int __LOC__ expected got )
  in
  [ test_scalar_validation ~name:"0"
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000000")
      ~expected:2;
    test_scalar_validation ~name:"1"
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000001")
      ~expected:1;
    test_scalar_validation ~name:"n-1"
      ~scalar:
        (`Hex
          "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550")
      ~expected:1;
    test_scalar_validation ~name:"n"
      ~scalar:
        (`Hex
          "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551")
      ~expected:2
  ]

let () =
  Alcotest.run "Fiat_p256"
    [ ("Key exchange", key_exchange);
      ("Low level scalar mult", scalar_mult);
      ("Point validation", point_validation);
      ("Scalar validation when generating", scalar_validation)
    ]
