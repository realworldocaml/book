open Mirage_crypto_ec

module Testable = struct
  let ok_or_error =
    Alcotest.result Alcotest.unit (Alcotest.testable pp_error ( = ))
end

let pp_hex_le fmt cs =
  let n = Cstruct.length cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done

let pp_result ppf = function
  | Ok cs -> pp_hex_le ppf cs
  | Error e -> Format.fprintf ppf "%a" pp_error e

let key_exchange =
  let test ~name d p ~expected =
    ( name,
      `Quick,
      fun () ->
        P256.Dh.key_exchange d p
        |> Format.asprintf "%a" pp_result
        |> Alcotest.check Alcotest.string __LOC__ expected )
  in
  let kp_of_cs data =
    match P256.Dh.secret_of_cs data with
    | Ok (p, s) -> p, s
    | Error _ -> assert false
  in
  let d_a, p_a =
    kp_of_cs (Cstruct.of_hex "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  and d_b, p_b =
    kp_of_cs (Cstruct.of_hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  and d_b', p_b' =
    kp_of_cs (Cstruct.shift (Cstruct.of_hex "00000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f") 1)
  in
  [
    test ~name:"b*A" d_b p_a
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"a*B" d_a p_b
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"b'*A" d_b' p_a
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"a*B'" d_a p_b'
      ~expected:
        "2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b";
    test ~name:"a*A" d_a p_a
      ~expected:
        "2ea4e810837da217a5bfd05f01d12459eeda830b6e0dec7f8afa425c5b55c507";
    test ~name:"b*B" d_b p_b
      ~expected:
        "a7666bcc3818472194460f7df22d80a5886da0e1679eac930175ce1ff733c7ca";
  ]

let scalar_mult =
  let test ~n ~scalar ~point ~expected =
    let scalar =
      match P256.Dh.secret_of_cs scalar with
      | Ok (p, _) -> p
      | Error _ -> assert false
    in
    let point = Hex.to_cstruct point in
    ( Printf.sprintf "Scalar mult (#%d)" n,
      `Quick,
      fun () ->
        P256.Dh.key_exchange scalar point
        |> Format.asprintf "%a" pp_result
        |> Alcotest.check Alcotest.string __LOC__ expected )
  in
  let point =
    `Hex
      "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
  in
  [
    test ~n:0
      ~scalar:
        (Cstruct.of_hex
           "0000000000000000000000000000000000000000000000000000000000000001")
      ~point
      ~expected:
        "96c298d84539a1f4a033eb2d817d0377f240a463e5e6bcf847422ce1f2d1176b";
    test ~n:1
      ~scalar:
        (Cstruct.of_hex
           "0000000000000000000000000000000000000000000000000000000000000002")
      ~point
      ~expected:
        "78996647fc480ba6351bf277e26989c0c31ab5040338528a7e4f038d187bf27c";
    test ~n:2
      ~scalar:
        (Cstruct.of_hex
           "0000000000000000000000000000000000000000000000000000000000000004")
      ~point
      ~expected:
        "5208036b44029350ef965578dbe21f03d02be69e65de2da0bb8fd032354a53e2";
    test ~n:3
      ~scalar:
        (Cstruct.of_hex
           "0612465c89a023ab17855b0a6bcebfd3febb53aef84138647b5352e02c10c346")
      ~point:
        (`Hex
          "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf")
      ~expected:
        "854271e19508bc935ab22b95cd2be13a0e78265f528b658b3219028b900d0253";
    test ~n:4
      ~scalar:
        (Cstruct.of_hex
           "0a0d622a47e48f6bc1038ace438c6f528aa00ad2bd1da5f13ee46bf5f633d71a")
      ~point:
        (`Hex
          "043cbc1b31b43f17dc200dd70c2944c04c6cb1b082820c234a300b05b7763844c74fde0a4ef93887469793270eb2ff148287da9265b0334f9e2609aac16e8ad503")
      ~expected:
        "ffffffffffffffffffffffffffffffff3022cfeeffffffffffffffffffffff7f";
    test ~n:5
      ~scalar:
        (Cstruct.of_hex
           "55d55f11bb8da1ea318bca7266f0376662441ea87270aa2077f1b770c4854a48")
      ~point:
        (`Hex
          "04000000000000000000000000000000000000000000000000000000000000000066485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4")
      ~expected:
        "48e82c9b82c88cb9fc2a5cff9e7c41bc4255ff6bd3814538c9b130877c07e4cf";
  ]

let to_ok_or_error = function Ok _ -> Ok () | Error _ as e -> e

let point_validation =
  let test ~name ~x ~y ~expected =
    let scalar =
      match P256.Dh.secret_of_cs (Cstruct.of_hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f") with
      | Ok (p, _) -> p
      | _ -> assert false
    in
    let point =
      Cstruct.concat [ Cstruct.of_hex "04"; Hex.to_cstruct x; Hex.to_cstruct y ]
    in
    ( name,
      `Quick,
      fun () ->
        P256.Dh.key_exchange scalar point
        |> to_ok_or_error
        |> Alcotest.check Testable.ok_or_error __LOC__ expected )
  in
  let zero = `Hex (String.make 64 '0') in
  let sb =
    `Hex "66485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4"
  in
  [
    test ~name:"Ok"
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
      ~expected:(Error `Invalid_range);
  ]

let scalar_validation =
  let ign_sec hex =
    match P256.Dh.secret_of_cs (Cstruct.of_hex hex) with
    | Ok _ -> Ok ()
    | Error _ as e -> e
  in
  [ ("0", `Quick, fun () ->
        Alcotest.check Testable.ok_or_error __LOC__
          (Error `Invalid_range)
          (ign_sec "0000000000000000000000000000000000000000000000000000000000000000")) ;
    ("1", `Quick, fun () ->
        Alcotest.check Testable.ok_or_error __LOC__
          (Ok ())
          (ign_sec "0000000000000000000000000000000000000000000000000000000000000001")) ;
    ("n-1", `Quick, fun () ->
        Alcotest.check Testable.ok_or_error __LOC__
          (Ok ())
          (ign_sec "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550")) ;
    ("n", `Quick, fun () ->
        Alcotest.check Testable.ok_or_error __LOC__
          (Error `Invalid_range)
          (ign_sec "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551")) ;
  ]

let ecdsa_gen () =
  let d = Cstruct.of_hex "C477F9F6 5C22CCE2 0657FAA5 B2D1D812 2336F851 A508A1ED 04E479C3 4985BF96" in
  let p = match
      P256.Dsa.pub_of_cstruct
        (Cstruct.of_hex {|04
                        B7E08AFD FE94BAD3 F1DC8C73 4798BA1C 62B3A0AD 1E9EA2A3 8201CD08 89BC7A19
                        3603F747 959DBF7A 4BB226E4 19287290 63ADC7AE 43529E61 B563BBC6 06CC5E09|})
    with
    | Ok a -> a
    | Error _ -> assert false
  in
  let pub = match P256.Dsa.priv_of_cstruct d with
    | Ok p -> P256.Dsa.pub_of_priv p
    | Error _ -> Alcotest.fail "couldn't decode private key"
  in
  let pub_eq a b =
    Cstruct.equal (P256.Dsa.pub_to_cstruct a) (P256.Dsa.pub_to_cstruct b)
  in
  Alcotest.(check bool __LOC__ true (pub_eq pub p))

let ecdsa_sign () =
  let d = Cstruct.of_hex "C477F9F6 5C22CCE2 0657FAA5 B2D1D812 2336F851 A508A1ED 04E479C3 4985BF96"
  and k = Cstruct.of_hex "7A1A7E52 797FC8CA AA435D2A 4DACE391 58504BF2 04FBE19F 14DBB427 FAEE50AE"
  and e = Cstruct.of_hex "A41A41A1 2A799548 211C410C 65D8133A FDE34D28 BDD542E4 B680CF28 99C8A8C4"
  in
  let r = Cstruct.of_hex "2B42F576 D07F4165 FF65D1F3 B1500F81 E44C316F 1F0B3EF5 7325B69A CA46104F"
  and s = Cstruct.of_hex "DC42C212 2D6392CD 3E3A993A 89502A81 98C1886F E69D262C 4B329BDB 6B63FAF1"
  in
  let key = match P256.Dsa.priv_of_cstruct d with
    | Ok p -> p
    | Error _ -> Alcotest.fail "couldn't decode private key"
  in
  let (r', s') = P256.Dsa.sign ~key ~k e in
  Alcotest.(check bool __LOC__ true (Cstruct.equal r r' && Cstruct.equal s s'));
  let d' = Cstruct.(shift (append (create 10) d) 10)
  and k' = Cstruct.(shift (append (create 12) k) 12)
  and e' = Cstruct.(shift (append (create 16) e) 16)
  in
  let key = match P256.Dsa.priv_of_cstruct d' with
    | Ok p -> p
    | Error _ -> Alcotest.fail "couldn't decode private key"
  in
  let (r', s') = P256.Dsa.sign ~key ~k:k' e' in
  Alcotest.(check bool __LOC__ true (Cstruct.equal r r' && Cstruct.equal s s'))

let ecdsa_verify () =
  let key =
    match P256.Dsa.pub_of_cstruct
            (Cstruct.of_hex {|04
                        B7E08AFD FE94BAD3 F1DC8C73 4798BA1C 62B3A0AD 1E9EA2A3 8201CD08 89BC7A19
                        3603F747 959DBF7A 4BB226E4 19287290 63ADC7AE 43529E61 B563BBC6 06CC5E09|})
    with
    | Ok a -> a
    | Error _ -> assert false
  and e = Cstruct.of_hex "A41A41A1 2A799548 211C410C 65D8133A FDE34D28 BDD542E4 B680CF28 99C8A8C4"
  and r = Cstruct.of_hex "2B42F576 D07F4165 FF65D1F3 B1500F81 E44C316F 1F0B3EF5 7325B69A CA46104F"
  and s = Cstruct.of_hex "DC42C212 2D6392CD 3E3A993A 89502A81 98C1886F E69D262C 4B329BDB 6B63FAF1"
  in
  Alcotest.(check bool __LOC__ true (P256.Dsa.verify ~key (r, s) e));
  let key =
    match P256.Dsa.pub_of_cstruct
            Cstruct.(shift (of_hex {|0000000004
                        B7E08AFD FE94BAD3 F1DC8C73 4798BA1C 62B3A0AD 1E9EA2A3 8201CD08 89BC7A19
                        3603F747 959DBF7A 4BB226E4 19287290 63ADC7AE 43529E61 B563BBC6 06CC5E09|}) 4)
    with
    | Ok a -> a
    | Error _ -> assert false
  in
  let e' = Cstruct.(shift (append (create 3) e) 3)
  and r' = Cstruct.(shift (append (create 6) r) 6)
  and s' = Cstruct.(shift (append (create 9) s) 9)
  in
  Alcotest.(check bool __LOC__ true (P256.Dsa.verify ~key (r', s') e'))

let ecdsa = [
  (* from https://csrc.nist.rip/groups/ST/toolkit/documents/Examples/ECDSA_Prime.pdf *)
  "ECDSA gen", `Quick, ecdsa_gen ;
  "ECDSA sign", `Quick, ecdsa_sign ;
  "ECDSA verify", `Quick, ecdsa_verify ;
]

let ecdsa_rfc6979_p224 =
  (* A.2.4 - P 224 *)
  let priv, pub =
    let data = Cstruct.of_hex "F220266E1105BFE3083E03EC7A3A654651F45E37167E88600BF257C1" in
    match P224.Dsa.priv_of_cstruct data with
    | Ok p -> p, P224.Dsa.pub_of_priv p
    | Error _ -> assert false
  in
  let pub_rfc () =
    let fst = Cstruct.create 1 in
    Cstruct.set_uint8 fst 0 4;
    let ux = Cstruct.of_hex "00CF08DA5AD719E42707FA431292DEA11244D64FC51610D94B130D6C"
    and uy = Cstruct.of_hex "EEAB6F3DEBE455E3DBF85416F7030CBD94F34F2D6F232C69F3C1385A"
    in
    match P224.Dsa.pub_of_cstruct (Cstruct.concat [ fst ; ux ; uy ]) with
    | Ok p ->
      let pub_eq =
        Cstruct.equal (P224.Dsa.pub_to_cstruct pub) (P224.Dsa.pub_to_cstruct p)
      in
      Alcotest.(check bool __LOC__ true pub_eq)
    | Error _ -> Alcotest.fail "bad public key"
  in
  let case hash ~message ~k ~r ~s () =
    let msg =
      let h = Mirage_crypto.Hash.digest hash (Cstruct.of_string message) in
      Cstruct.sub h 0 (min (Cstruct.length h) 28)
    and k = Cstruct.of_hex k
    in
    let k' =
      let module H = (val (Mirage_crypto.Hash.module_of hash)) in
      let module K = P224.Dsa.K_gen (H) in
      K.generate ~key:priv msg
    in
    Alcotest.(check bool __LOC__ true (Cstruct.equal k k'));
    let sig_eq (r', s') =
      Cstruct.equal (Cstruct.of_hex r) r' && Cstruct.equal (Cstruct.of_hex s) s'
    in
    let sig' = P224.Dsa.sign ~key:priv ~k msg in
    Alcotest.(check bool __LOC__ true (sig_eq sig'))
  in
  let cases = [

   case `SHA1 ~message:"sample"
   ~k:"7EEFADD91110D8DE6C2C470831387C50D3357F7F4D477054B8B426BC"
   ~r:"22226F9D40A96E19C4A301CE5B74B115303C0F3A4FD30FC257FB57AC"
   ~s:"66D1CDD83E3AF75605DD6E2FEFF196D30AA7ED7A2EDF7AF475403D69";

   case `SHA224 ~message:"sample"
   ~k:"C1D1F2F10881088301880506805FEB4825FE09ACB6816C36991AA06D"
   ~r:"1CDFE6662DDE1E4A1EC4CDEDF6A1F5A2FB7FBD9145C12113E6ABFD3E"
   ~s:"A6694FD7718A21053F225D3F46197CA699D45006C06F871808F43EBC";

   case `SHA256 ~message:"sample"
   ~k:"AD3029E0278F80643DE33917CE6908C70A8FF50A411F06E41DEDFCDC"
   ~r:"61AA3DA010E8E8406C656BC477A7A7189895E7E840CDFE8FF42307BA"
   ~s:"BC814050DAB5D23770879494F9E0A680DC1AF7161991BDE692B10101";

   case `SHA384 ~message:"sample"
   ~k:"52B40F5A9D3D13040F494E83D3906C6079F29981035C7BD51E5CAC40"
   ~r:"0B115E5E36F0F9EC81F1325A5952878D745E19D7BB3EABFABA77E953"
   ~s:"830F34CCDFE826CCFDC81EB4129772E20E122348A2BBD889A1B1AF1D";

   case `SHA512 ~message:"sample"
   ~k:"9DB103FFEDEDF9CFDBA05184F925400C1653B8501BAB89CEA0FBEC14"
   ~r:"074BD1D979D5F32BF958DDC61E4FB4872ADCAFEB2256497CDAC30397"
   ~s:"A4CECA196C3D5A1FF31027B33185DC8EE43F288B21AB342E5D8EB084";

   case `SHA1 ~message:"test"
   ~k:"2519178F82C3F0E4F87ED5883A4E114E5B7A6E374043D8EFD329C253"
   ~r:"DEAA646EC2AF2EA8AD53ED66B2E2DDAA49A12EFD8356561451F3E21C"
   ~s:"95987796F6CF2062AB8135271DE56AE55366C045F6D9593F53787BD2";

   case `SHA224 ~message:"test"
   ~k:"DF8B38D40DCA3E077D0AC520BF56B6D565134D9B5F2EAE0D34900524"
   ~r:"C441CE8E261DED634E4CF84910E4C5D1D22C5CF3B732BB204DBEF019"
   ~s:"902F42847A63BDC5F6046ADA114953120F99442D76510150F372A3F4";

   case `SHA256 ~message:"test"
   ~k:"FF86F57924DA248D6E44E8154EB69F0AE2AEBAEE9931D0B5A969F904"
   ~r:"AD04DDE87B84747A243A631EA47A1BA6D1FAA059149AD2440DE6FBA6"
   ~s:"178D49B1AE90E3D8B629BE3DB5683915F4E8C99FDF6E666CF37ADCFD";

   case `SHA384 ~message:"test"
   ~k:"7046742B839478C1B5BD31DB2E862AD868E1A45C863585B5F22BDC2D"
   ~r:"389B92682E399B26518A95506B52C03BC9379A9DADF3391A21FB0EA4"
   ~s:"414A718ED3249FF6DBC5B50C27F71F01F070944DA22AB1F78F559AAB";

   case `SHA512 ~message:"test"
   ~k:"E39C2AA4EA6BE2306C72126D40ED77BF9739BB4D6EF2BBB1DCB6169D"
   ~r:"049F050477C5ADD858CAC56208394B5A55BAEBBE887FDF765047C17C"
   ~s:"077EB13E7005929CEFA3CD0403C7CDCC077ADF4E44F3C41B2F60ECFF";

  ] in
  ("public key matches", `Quick, pub_rfc) ::
  List.mapi (fun i c -> "RFC 6979 A.2.4 " ^ string_of_int i, `Quick, c) cases

let ecdsa_rfc6979_p256 =
  (* A.2.5 - P 256 *)
  let priv, pub =
    let data = Cstruct.of_hex "C9AFA9D845BA75166B5C215767B1D6934E50C3DB36E89B127B8A622B120F6721" in
    match P256.Dsa.priv_of_cstruct data with
    | Ok p -> p, P256.Dsa.pub_of_priv p
    | Error _ -> assert false
  in
  let pub_rfc () =
    let fst = Cstruct.create 1 in
    Cstruct.set_uint8 fst 0 4;
    let ux = Cstruct.of_hex "60FED4BA255A9D31C961EB74C6356D68C049B8923B61FA6CE669622E60F29FB6"
    and uy = Cstruct.of_hex "7903FE1008B8BC99A41AE9E95628BC64F2F1B20C2D7E9F5177A3C294D4462299"
    in
    match P256.Dsa.pub_of_cstruct (Cstruct.concat [ fst ; ux ; uy ]) with
    | Ok p ->
      let pub_eq =
        Cstruct.equal (P256.Dsa.pub_to_cstruct pub) (P256.Dsa.pub_to_cstruct p)
      in
      Alcotest.(check bool __LOC__ true pub_eq)
    | Error _ -> Alcotest.fail "bad public key"
  in
  let pub_key_compression () =
    let _, pub = P256.Dsa.generate () in
    let compressed = P256.Dsa.pub_to_cstruct ~compress:true pub in
    let decompressed = P256.Dsa.pub_of_cstruct compressed in
    let comparison = match decompressed with
      | Ok decompressed ->
        let cstruct1 = (P256.Dsa.pub_to_cstruct pub) in
        let cstruct2 = (P256.Dsa.pub_to_cstruct decompressed) in
        Cstruct.equal cstruct1 cstruct2
      | Error _ -> false in
    Alcotest.(check bool __LOC__ true comparison)
  in
  let case hash ~message ~k ~r ~s () =
    let msg =
      let h = Mirage_crypto.Hash.digest hash (Cstruct.of_string message) in
      Cstruct.sub h 0 (min (Cstruct.length h) 32)
    and k = Cstruct.of_hex k
    in
    let k' =
      let module H = (val (Mirage_crypto.Hash.module_of hash)) in
      let module K = P256.Dsa.K_gen (H) in
      K.generate ~key:priv msg
    in
    Alcotest.(check bool __LOC__ true (Cstruct.equal k k'));
    let sig_eq (r', s') =
      Cstruct.equal (Cstruct.of_hex r) r' && Cstruct.equal (Cstruct.of_hex s) s'
    in
    let sig' = P256.Dsa.sign ~key:priv ~k msg in
    Alcotest.(check bool __LOC__ true (sig_eq sig'))
  in
  let cases = [
    case `SHA1  ~message:"sample"
      ~k:"882905F1227FD620FBF2ABF21244F0BA83D0DC3A9103DBBEE43A1FB858109DB4"
      ~r:"61340C88C3AAEBEB4F6D667F672CA9759A6CCAA9FA8811313039EE4A35471D32"
      ~s:"6D7F147DAC089441BB2E2FE8F7A3FA264B9C475098FDCF6E00D7C996E1B8B7EB" ;
    case `SHA224 ~message:"sample"
      ~k:"103F90EE9DC52E5E7FB5132B7033C63066D194321491862059967C715985D473"
      ~r:"53B2FFF5D1752B2C689DF257C04C40A587FABABB3F6FC2702F1343AF7CA9AA3F"
      ~s:"B9AFB64FDC03DC1A131C7D2386D11E349F070AA432A4ACC918BEA988BF75C74C" ;
    case `SHA256 ~message:"sample"
      ~k:"A6E3C57DD01ABE90086538398355DD4C3B17AA873382B0F24D6129493D8AAD60"
      ~r:"EFD48B2AACB6A8FD1140DD9CD45E81D69D2C877B56AAF991C34D0EA84EAF3716"
      ~s:"F7CB1C942D657C41D436C7A1B6E29F65F3E900DBB9AFF4064DC4AB2F843ACDA8" ;
    case `SHA384 ~message:"sample"
      ~k:"09F634B188CEFD98E7EC88B1AA9852D734D0BC272F7D2A47DECC6EBEB375AAD4"
      ~r:"0EAFEA039B20E9B42309FB1D89E213057CBF973DC0CFC8F129EDDDC800EF7719"
      ~s:"4861F0491E6998B9455193E34E7B0D284DDD7149A74B95B9261F13ABDE940954" ;
    case `SHA512 ~message:"sample"
      ~k:"5FA81C63109BADB88C1F367B47DA606DA28CAD69AA22C4FE6AD7DF73A7173AA5"
      ~r:"8496A60B5E9B47C825488827E0495B0E3FA109EC4568FD3F8D1097678EB97F00"
      ~s:"2362AB1ADBE2B8ADF9CB9EDAB740EA6049C028114F2460F96554F61FAE3302FE" ;
    case `SHA1 ~message:"test"
      ~k:"8C9520267C55D6B980DF741E56B4ADEE114D84FBFA2E62137954164028632A2E"
      ~r:"0CBCC86FD6ABD1D99E703E1EC50069EE5C0B4BA4B9AC60E409E8EC5910D81A89"
      ~s:"01B9D7B73DFAA60D5651EC4591A0136F87653E0FD780C3B1BC872FFDEAE479B1" ;
    case `SHA224 ~message:"test"
      ~k:"669F4426F2688B8BE0DB3A6BD1989BDAEFFF84B649EEB84F3DD26080F667FAA7"
      ~r:"C37EDB6F0AE79D47C3C27E962FA269BB4F441770357E114EE511F662EC34A692"
      ~s:"C820053A05791E521FCAAD6042D40AEA1D6B1A540138558F47D0719800E18F2D" ;
    case `SHA256 ~message:"test"
      ~k:"D16B6AE827F17175E040871A1C7EC3500192C4C92677336EC2537ACAEE0008E0"
      ~r:"F1ABB023518351CD71D881567B1EA663ED3EFCF6C5132B354F28D3B0B7D38367"
      ~s:"019F4113742A2B14BD25926B49C649155F267E60D3814B4C0CC84250E46F0083" ;
    case `SHA384 ~message:"test"
      ~k:"16AEFFA357260B04B1DD199693960740066C1A8F3E8EDD79070AA914D361B3B8"
      ~r:"83910E8B48BB0C74244EBDF7F07A1C5413D61472BD941EF3920E623FBCCEBEB6"
      ~s:"8DDBEC54CF8CD5874883841D712142A56A8D0F218F5003CB0296B6B509619F2C" ;
    case `SHA512 ~message:"test"
      ~k:"6915D11632ACA3C40D5D51C08DAF9C555933819548784480E93499000D9F0B7F"
      ~r:"461D93F31B6540894788FD206C07CFA0CC35F46FA3C91816FFF1040AD1581A04"
      ~s:"39AF9F15DE0DB8D97E72719C74820D304CE5226E32DEDAE67519E840D1194E55" ;
  ] in
  ("public key matches", `Quick, pub_rfc) ::
  ("public key compression and decompression", `Quick, pub_key_compression) ::
  List.mapi (fun i c -> "RFC 6979 A.2.5 " ^ string_of_int i, `Quick, c) cases

let ecdsa_rfc6979_p384 =
  (* A.2.6 - P 384 *)
  let priv, pub =
    let data = Cstruct.of_hex "6B9D3DAD2E1B8C1C05B19875B6659F4DE23C3B667BF297BA9AA47740787137D896D5724E4C70A825F872C9EA60D2EDF5" in
    match P384.Dsa.priv_of_cstruct data with
    | Ok p -> p, P384.Dsa.pub_of_priv p
    | Error _ -> assert false
  in
  let pub_rfc () =
    let fst = Cstruct.create 1 in
    Cstruct.set_uint8 fst 0 4;
    let ux = Cstruct.of_hex "EC3A4E415B4E19A4568618029F427FA5DA9A8BC4AE92E02E06AAE5286B300C64DEF8F0EA9055866064A254515480BC13"
    and uy = Cstruct.of_hex "8015D9B72D7D57244EA8EF9AC0C621896708A59367F9DFB9F54CA84B3F1C9DB1288B231C3AE0D4FE7344FD2533264720"
    in
    match P384.Dsa.pub_of_cstruct (Cstruct.concat [ fst ; ux ; uy ]) with
    | Ok p ->
      let pub_eq =
        Cstruct.equal (P384.Dsa.pub_to_cstruct pub) (P384.Dsa.pub_to_cstruct p)
      in
      Alcotest.(check bool __LOC__ true pub_eq)
    | Error _ -> Alcotest.fail "bad public key"
  in
  let pub_key_compression () =
    let _, pub = P384.Dsa.generate () in
    let compressed = P384.Dsa.pub_to_cstruct ~compress:true pub in
    let decompressed = P384.Dsa.pub_of_cstruct compressed in
    let comparison = match decompressed with
      | Ok decompressed ->
        let cstruct1 = (P384.Dsa.pub_to_cstruct pub) in
        let cstruct2 = (P384.Dsa.pub_to_cstruct decompressed) in
        Cstruct.equal cstruct1 cstruct2
      | Error _ -> false
    in
    Alcotest.(check bool __LOC__ true comparison)
  in
  let case hash ~message ~k ~r ~s () =
    let msg =
      let h = Mirage_crypto.Hash.digest hash (Cstruct.of_string message) in
      Cstruct.sub h 0 (min (Cstruct.length h) 48)
    and k = Cstruct.of_hex k
    in
    let k' =
      let module H = (val (Mirage_crypto.Hash.module_of hash)) in
      let module K = P384.Dsa.K_gen (H) in
      K.generate ~key:priv msg
    in
    Alcotest.(check bool __LOC__ true (Cstruct.equal k k'));
    let sig_eq (r', s') =
      Cstruct.equal (Cstruct.of_hex r) r' && Cstruct.equal (Cstruct.of_hex s) s'
    in
    let sig' = P384.Dsa.sign ~key:priv ~k msg in
    Alcotest.(check bool __LOC__ true (sig_eq sig'))
  in
  let cases = [
   case `SHA1 ~message:"sample"
   ~k:"4471EF7518BB2C7C20F62EAE1C387AD0C5E8E470995DB4ACF694466E6AB09663
       0F29E5938D25106C3C340045A2DB01A7"
   ~r:"EC748D839243D6FBEF4FC5C4859A7DFFD7F3ABDDF72014540C16D73309834FA3
       7B9BA002899F6FDA3A4A9386790D4EB2"
   ~s:"A3BCFA947BEEF4732BF247AC17F71676CB31A847B9FF0CBC9C9ED4C1A5B3FACF
       26F49CA031D4857570CCB5CA4424A443";

   case `SHA224 ~message:"sample"
   ~k:"A4E4D2F0E729EB786B31FC20AD5D849E304450E0AE8E3E341134A5C1AFA03CAB
       8083EE4E3C45B06A5899EA56C51B5879"
   ~r:"42356E76B55A6D9B4631C865445DBE54E056D3B3431766D0509244793C3F9366
       450F76EE3DE43F5A125333A6BE060122"
   ~s:"9DA0C81787064021E78DF658F2FBB0B042BF304665DB721F077A4298B095E483
       4C082C03D83028EFBF93A3C23940CA8D";

   case `SHA256 ~message:"sample"
   ~k:"180AE9F9AEC5438A44BC159A1FCB277C7BE54FA20E7CF404B490650A8ACC414E
       375572342863C899F9F2EDF9747A9B60"
   ~r:"21B13D1E013C7FA1392D03C5F99AF8B30C570C6F98D4EA8E354B63A21D3DAA33
       BDE1E888E63355D92FA2B3C36D8FB2CD"
   ~s:"F3AA443FB107745BF4BD77CB3891674632068A10CA67E3D45DB2266FA7D1FEEB
       EFDC63ECCD1AC42EC0CB8668A4FA0AB0";

   case `SHA384 ~message:"sample"
   ~k:"94ED910D1A099DAD3254E9242AE85ABDE4BA15168EAF0CA87A555FD56D10FBCA
       2907E3E83BA95368623B8C4686915CF9"
   ~r:"94EDBB92A5ECB8AAD4736E56C691916B3F88140666CE9FA73D64C4EA95AD133C
       81A648152E44ACF96E36DD1E80FABE46"
   ~s:"99EF4AEB15F178CEA1FE40DB2603138F130E740A19624526203B6351D0A3A94F
       A329C145786E679E7B82C71A38628AC8";

   case `SHA512 ~message:"sample"
   ~k:"92FC3C7183A883E24216D1141F1A8976C5B0DD797DFA597E3D7B32198BD35331
       A4E966532593A52980D0E3AAA5E10EC3"
   ~r:"ED0959D5880AB2D869AE7F6C2915C6D60F96507F9CB3E047C0046861DA4A799C
       FE30F35CC900056D7C99CD7882433709"
   ~s:"512C8CCEEE3890A84058CE1E22DBC2198F42323CE8ACA9135329F03C068E5112
       DC7CC3EF3446DEFCEB01A45C2667FDD5";

   case `SHA1 ~message:"test"
   ~k:"66CC2C8F4D303FC962E5FF6A27BD79F84EC812DDAE58CF5243B64A4AD8094D47
       EC3727F3A3C186C15054492E30698497"
   ~r:"4BC35D3A50EF4E30576F58CD96CE6BF638025EE624004A1F7789A8B8E43D0678
       ACD9D29876DAF46638645F7F404B11C7"
   ~s:"D5A6326C494ED3FF614703878961C0FDE7B2C278F9A65FD8C4B7186201A29916
       95BA1C84541327E966FA7B50F7382282";

   case `SHA224 ~message:"test"
   ~k:"18FA39DB95AA5F561F30FA3591DC59C0FA3653A80DAFFA0B48D1A4C6DFCBFF6E
       3D33BE4DC5EB8886A8ECD093F2935726"
   ~r:"E8C9D0B6EA72A0E7837FEA1D14A1A9557F29FAA45D3E7EE888FC5BF954B5E624
       64A9A817C47FF78B8C11066B24080E72"
   ~s:"07041D4A7A0379AC7232FF72E6F77B6DDB8F09B16CCE0EC3286B2BD43FA8C614
       1C53EA5ABEF0D8231077A04540A96B66";

   case `SHA256 ~message:"test"
   ~k:"0CFAC37587532347DC3389FDC98286BBA8C73807285B184C83E62E26C401C0FA
       A48DD070BA79921A3457ABFF2D630AD7"
   ~r:"6D6DEFAC9AB64DABAFE36C6BF510352A4CC27001263638E5B16D9BB51D451559
       F918EEDAF2293BE5B475CC8F0188636B"
   ~s:"2D46F3BECBCC523D5F1A1256BF0C9B024D879BA9E838144C8BA6BAEB4B53B47D
       51AB373F9845C0514EEFB14024787265";

   case `SHA384 ~message:"test"
   ~k:"015EE46A5BF88773ED9123A5AB0807962D193719503C527B031B4C2D225092AD
       A71F4A459BC0DA98ADB95837DB8312EA"
   ~r:"8203B63D3C853E8D77227FB377BCF7B7B772E97892A80F36AB775D509D7A5FEB
       0542A7F0812998DA8F1DD3CA3CF023DB"
   ~s:"DDD0760448D42D8A43AF45AF836FCE4DE8BE06B485E9B61B827C2F13173923E0
       6A739F040649A667BF3B828246BAA5A5";

   case `SHA512 ~message:"test"
   ~k:"3780C4F67CB15518B6ACAE34C9F83568D2E12E47DEAB6C50A4E4EE5319D1E8CE
       0E2CC8A136036DC4B9C00E6888F66B6C"
   ~r:"A0D5D090C9980FAF3C2CE57B7AE951D31977DD11C775D314AF55F76C676447D0
       6FB6495CD21B4B6E340FC236584FB277"
   ~s:"976984E59B4C77B0E8E4460DCA3D9F20E07B9BB1F63BEEFAF576F6B2E8B22463
       4A2092CD3792E0159AD9CEE37659C736"
  ] in
  ("public key matches", `Quick, pub_rfc) ::
  ("public key compression and decompression", `Quick, pub_key_compression) ::
  List.mapi (fun i c -> "RFC 6979 A.2.6 " ^ string_of_int i, `Quick, c) cases

let ecdsa_rfc6979_p521 =
  (* A.2.7 - P 521 *)
  let of_h b = Cstruct.of_hex ((String.make 1 '0') ^ b) in
  let priv, pub =
    let data = of_h
        "0FAD06DAA62BA3B25D2FB40133DA757205DE67F5BB0018FEE8C86E1B68C7E75C
         AA896EB32F1F47C70855836A6D16FCC1466F6D8FBEC67DB89EC0C08B0E996B83
         538"
    in
    match P521.Dsa.priv_of_cstruct data with
    | Ok p -> p, P521.Dsa.pub_of_priv p
    | Error _ -> assert false
  in
  let pub_rfc () =
    let fst = Cstruct.create 1 in
    Cstruct.set_uint8 fst 0 4;
    let ux = of_h
        "1894550D0785932E00EAA23B694F213F8C3121F86DC97A04E5A7167DB4E5BCD3
         71123D46E45DB6B5D5370A7F20FB633155D38FFA16D2BD761DCAC474B9A2F502
         3A4"
    and uy = of_h
        "0493101C962CD4D2FDDF782285E64584139C2F91B47F87FF82354D6630F746A2
         8A0DB25741B5B34A828008B22ACC23F924FAAFBD4D33F81EA66956DFEAA2BFDF
         CF5"
    in
    match P521.Dsa.pub_of_cstruct (Cstruct.concat [ fst ; ux ; uy ]) with
    | Ok p ->
      let pub_eq =
        Cstruct.equal (P521.Dsa.pub_to_cstruct pub) (P521.Dsa.pub_to_cstruct p)
      in
      Alcotest.(check bool __LOC__ true pub_eq)
    | Error _ -> Alcotest.fail "bad public key"
  in
  let pub_key_compression () =
    let _, pub = P521.Dsa.generate () in
    let compressed = P521.Dsa.pub_to_cstruct ~compress:true pub in
    let decompressed = P521.Dsa.pub_of_cstruct compressed in
    let comparison = match decompressed with
      | Ok decompressed ->
        let cstruct1 = (P521.Dsa.pub_to_cstruct pub) in
        let cstruct2 = (P521.Dsa.pub_to_cstruct decompressed) in
        Cstruct.equal cstruct1 cstruct2
      | Error _ -> false
    in
    Alcotest.(check bool __LOC__ true comparison)
  in
  let case hash ~message ~k ~r ~s () =
    let msg = Mirage_crypto.Hash.digest hash (Cstruct.of_string message)
    and k = of_h k
    in
    let k' =
      let module H = (val (Mirage_crypto.Hash.module_of hash)) in
      let module K = P521.Dsa.K_gen (H) in
      K.generate ~key:priv msg
    in
    Alcotest.(check bool __LOC__ true (Cstruct.equal k k'));
    let sig_eq (r', s') =
      Cstruct.equal (of_h r) r' && Cstruct.equal (of_h s) s'
    in
    let sig' = P521.Dsa.sign ~key:priv ~k msg in
    Alcotest.(check bool __LOC__ true (sig_eq sig'))
  in
  let _cases = [

   case `SHA1 ~message:"sample"
   ~k:"089C071B419E1C2820962321787258469511958E80582E95D8378E0C2CCDB3CB
       42BEDE42F50E3FA3C71F5A76724281D31D9C89F0F91FC1BE4918DB1C03A5838D
       0F9"
   ~r:"0343B6EC45728975EA5CBA6659BBB6062A5FF89EEA58BE3C80B619F322C87910
       FE092F7D45BB0F8EEE01ED3F20BABEC079D202AE677B243AB40B5431D497C55D
       75D"
   ~s:"0E7B0E675A9B24413D448B8CC119D2BF7B2D2DF032741C096634D6D65D0DBE3D
       5694625FB9E8104D3B842C1B0E2D0B98BEA19341E8676AEF66AE4EBA3D5475D5
       D16";

   case `SHA224 ~message:"sample"
   ~k:"121415EC2CD7726330A61F7F3FA5DE14BE9436019C4DB8CB4041F3B54CF31BE0
       493EE3F427FB906393D895A19C9523F3A1D54BB8702BD4AA9C99DAB2597B9211
       3F3"
   ~r:"1776331CFCDF927D666E032E00CF776187BC9FDD8E69D0DABB4109FFE1B5E2A3
       0715F4CC923A4A5E94D2503E9ACFED92857B7F31D7152E0F8C00C15FF3D87E2E
       D2E"
   ~s:"050CB5265417FE2320BBB5A122B8E1A32BD699089851128E360E620A30C7E17B
       A41A666AF126CE100E5799B153B60528D5300D08489CA9178FB610A2006C254B
       41F";

   case `SHA256 ~message:"sample"
   ~k:"0EDF38AFCAAECAB4383358B34D67C9F2216C8382AAEA44A3DAD5FDC9C3257576
       1793FEF24EB0FC276DFC4F6E3EC476752F043CF01415387470BCBD8678ED2C7E
       1A0"
   ~r:"1511BB4D675114FE266FC4372B87682BAECC01D3CC62CF2303C92B3526012659
       D16876E25C7C1E57648F23B73564D67F61C6F14D527D54972810421E7D87589E
       1A7"
   ~s:"04A171143A83163D6DF460AAF61522695F207A58B95C0644D87E52AA1A347916
       E4F7A72930B1BC06DBE22CE3F58264AFD23704CBB63B29B931F7DE6C9D949A7E
       CFC";

   case `SHA384 ~message:"sample"
   ~k:"1546A108BC23A15D6F21872F7DED661FA8431DDBD922D0DCDB77CC878C8553FF
       AD064C95A920A750AC9137E527390D2D92F153E66196966EA554D9ADFCB109C4
       211"
   ~r:"1EA842A0E17D2DE4F92C15315C63DDF72685C18195C2BB95E572B9C5136CA4B4
       B576AD712A52BE9730627D16054BA40CC0B8D3FF035B12AE75168397F5D50C67
       451"
   ~s:"1F21A3CEE066E1961025FB048BD5FE2B7924D0CD797BABE0A83B66F1E35EEAF5
       FDE143FA85DC394A7DEE766523393784484BDF3E00114A1C857CDE1AA203DB65
       D61";

   case `SHA512 ~message:"sample"
   ~k:"1DAE2EA071F8110DC26882D4D5EAE0621A3256FC8847FB9022E2B7D28E6F1019
       8B1574FDD03A9053C08A1854A168AA5A57470EC97DD5CE090124EF52A2F7ECBF
       FD3"
   ~r:"0C328FAFCBD79DD77850370C46325D987CB525569FB63C5D3BC53950E6D4C5F1
       74E25A1EE9017B5D450606ADD152B534931D7D4E8455CC91F9B15BF05EC36E37
       7FA"
   ~s:"0617CCE7CF5064806C467F678D3B4080D6F1CC50AF26CA209417308281B68AF2
       82623EAA63E5B5C0723D8B8C37FF0777B1A20F8CCB1DCCC43997F1EE0E44DA4A
       67A";

   case `SHA1 ~message:"test"
   ~k:"0BB9F2BF4FE1038CCF4DABD7139A56F6FD8BB1386561BD3C6A4FC818B20DF5DD
       BA80795A947107A1AB9D12DAA615B1ADE4F7A9DC05E8E6311150F47F5C57CE8B
       222"
   ~r:"13BAD9F29ABE20DE37EBEB823C252CA0F63361284015A3BF430A46AAA80B87B0
       693F0694BD88AFE4E661FC33B094CD3B7963BED5A727ED8BD6A3A202ABE009D0
       367"
   ~s:"1E9BB81FF7944CA409AD138DBBEE228E1AFCC0C890FC78EC8604639CB0DBDC90
       F717A99EAD9D272855D00162EE9527567DD6A92CBD629805C0445282BBC91679
       7FF";

   case `SHA224 ~message:"test"
   ~k:"040D09FCF3C8A5F62CF4FB223CBBB2B9937F6B0577C27020A99602C25A011369
       87E452988781484EDBBCF1C47E554E7FC901BC3085E5206D9F619CFF07E73D6F
       706"
   ~r:"1C7ED902E123E6815546065A2C4AF977B22AA8EADDB68B2C1110E7EA44D42086
       BFE4A34B67DDC0E17E96536E358219B23A706C6A6E16BA77B65E1C595D43CAE1
       7FB"
   ~s:"177336676304FCB343CE028B38E7B4FBA76C1C1B277DA18CAD2A8478B2A9A9F5
       BEC0F3BA04F35DB3E4263569EC6AADE8C92746E4C82F8299AE1B8F1739F8FD51
       9A4";

   case `SHA256 ~message:"test"
   ~k:"01DE74955EFAABC4C4F17F8E84D881D1310B5392D7700275F82F145C61E84384
       1AF09035BF7A6210F5A431A6A9E81C9323354A9E69135D44EBD2FCAA7731B909
       258"
   ~r:"00E871C4A14F993C6C7369501900C4BC1E9C7B0B4BA44E04868B30B41D807104
       2EB28C4C250411D0CE08CD197E4188EA4876F279F90B3D8D74A3C76E6F1E4656
       AA8"
   ~s:"0CD52DBAA33B063C3A6CD8058A1FB0A46A4754B034FCC644766CA14DA8CA5CA9
       FDE00E88C1AD60CCBA759025299079D7A427EC3CC5B619BFBC828E7769BCD694
       E86";

   case `SHA384 ~message:"test"
   ~k:"1F1FC4A349A7DA9A9E116BFDD055DC08E78252FF8E23AC276AC88B1770AE0B5D
       CEB1ED14A4916B769A523CE1E90BA22846AF11DF8B300C38818F713DADD85DE0
       C88"
   ~r:"14BEE21A18B6D8B3C93FAB08D43E739707953244FDBE924FA926D76669E7AC8C
       89DF62ED8975C2D8397A65A49DCC09F6B0AC62272741924D479354D74FF60755
       78C"
   ~s:"133330865C067A0EAF72362A65E2D7BC4E461E8C8995C3B6226A21BD1AA78F0E
       D94FE536A0DCA35534F0CD1510C41525D163FE9D74D134881E35141ED5E8E95B
       979";

   case `SHA512 ~message:"test"
   ~k:"16200813020EC986863BEDFC1B121F605C1215645018AEA1A7B215A564DE9EB1
       B38A67AA1128B80CE391C4FB71187654AAA3431027BFC7F395766CA988C964DC
       56D"
   ~r:"13E99020ABF5CEE7525D16B69B229652AB6BDF2AFFCAEF38773B4B7D08725F10
       CDB93482FDCC54EDCEE91ECA4166B2A7C6265EF0CE2BD7051B7CEF945BABD47E
       E6D"
   ~s:"1FBD0013C674AA79CB39849527916CE301C66EA7CE8B80682786AD60F98F7E78
       A19CA69EFF5C57400E3B3A0AD66CE0978214D13BAF4E9AC60752F7B155E2DE4D
       CE3"

  ] in
  [ ("public key matches", `Quick, pub_rfc); ("public key compression and decompression", `Quick, pub_key_compression)]
  (* TODO: our deterministic generator for bit_size mod 8 <> 0 is different from RFC 6979 *)
(* List.mapi (fun i c -> "RFC 6979 A.2.7 " ^ string_of_int i, `Quick, c) cases *)

let x25519 () =
  (* RFC 7748, 6.1 *)
  let a = Cstruct.of_hex "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
  and apub = Cstruct.of_hex "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
  and b = Cstruct.of_hex "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
  and bpub = Cstruct.of_hex "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
  and shared = Cstruct.of_hex "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
  in
  let of_cs cs = match X25519.secret_of_cs cs with
    | Ok (a, b) -> a, b
    | Error _ -> Alcotest.fail "couldn't decode secret"
  in
  let apriv, apub' = of_cs a in
  Alcotest.(check bool __LOC__ true (Cstruct.equal apub apub'));
  let bpriv, bpub' = of_cs b in
  Alcotest.(check bool __LOC__ true (Cstruct.equal bpub bpub'));
  (match X25519.key_exchange apriv bpub with
   | Ok shared' ->
     Alcotest.(check bool __LOC__ true (Cstruct.equal shared shared'))
   | Error e ->
     Alcotest.failf "X25519 key exchange apriv bpub failed %a" pp_error e);
  (match X25519.key_exchange bpriv apub with
   | Ok shared' ->
     Alcotest.(check bool __LOC__ true (Cstruct.equal shared shared'))
   | Error e ->
     Alcotest.failf "X25519 key exchange bpriv apub failed %a" pp_error e);
  let apub' = Cstruct.(shift (append (create 32) apub) 32)
  and b' = Cstruct.(shift (append (create 10) b) 10)
  in
  let bpriv', _ = of_cs b' in
  (match X25519.key_exchange bpriv' apub' with
   | Ok shared' ->
     Alcotest.(check bool __LOC__ true (Cstruct.equal shared shared'))
   | Error e ->
     Alcotest.failf "X25519 key exchange bpriv' apub' failed %a" pp_error e)

let ed25519 =
  let cs = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal in
  let test secret public msg signature =
    Alcotest.(
      check cs "public key is ok" (Ed25519.pub_to_cstruct public)
        Ed25519.(pub_to_cstruct (pub_of_priv secret)));
    Alcotest.(check cs "signature is ok" signature (Ed25519.sign ~key:secret msg));
    Alcotest.(check bool "verify is ok" true
                (Ed25519.verify ~key:public signature ~msg))
  in
  let case i ~secret ~public ~msg ~signature =
    "RFC 8032 " ^ string_of_int i, `Quick, fun () ->
      let s =
        match Ed25519.priv_of_cstruct (Cstruct.of_hex secret) with
        | Ok p ->
          Alcotest.(check cs "private key encoding is good"
                      (Cstruct.of_hex secret) (Ed25519.priv_to_cstruct p));
          p
        | Error _ -> Alcotest.fail "failed to decode private key"
      and p =
        match Ed25519.pub_of_cstruct (Cstruct.of_hex public) with
        | Ok p ->
          Alcotest.(check cs "public key encoding is good"
                      (Cstruct.of_hex public) (Ed25519.pub_to_cstruct p));
          p
        | Error _ -> Alcotest.fail "failed to decode public key"
      and m = Cstruct.of_hex msg
      and si = Cstruct.of_hex signature
      in
      test s p m si
  in
  [
    case 1
      ~secret:
        "9d61b19deffd5a60ba844af492ec2cc4 4449c5697b326919703bac031cae7f60"
      ~public:
        "d75a980182b10ab7d54bfed3c964073a 0ee172f3daa62325af021a68f707511a"
      ~msg:""
      ~signature:
        {|
        e5564300c360ac729086e2cc806e828a
        84877f1eb8e5d974d873e06522490155
        5fb8821590a33bacc61e39701cf9b46b
        d25bf5f0595bbe24655141438e7a100b
      |};
    case 2
      ~secret:
        "4ccd089b28ff96da9db6c346ec114e0f 5b8a319f35aba624da8cf6ed4fb8a6fb"
      ~public:
        "3d4017c3e843895a92b70aa74d1b7ebc 9c982ccf2ec4968cc0cd55f12af4660c"
      ~msg:"72"
      ~signature:
        {|
        92a009a9f0d4cab8720e820b5f642540
        a2b27b5416503f8fb3762223ebdb69da
        085ac1e43e15996e458f3613d0f11d8c
        387b2eaeb4302aeeb00d291612bb0c00
     |};
    case 3
      ~secret:
        "c5aa8df43f9f837bedb7442f31dcb7b1 66d38535076f094b85ce3a2e0b4458f7"
      ~public:
        "fc51cd8e6218a1a38da47ed00230f058 0816ed13ba3303ac5deb911548908025"
      ~msg:"af82"
      ~signature:
        {|
        6291d657deec24024827e69c3abe01a3
        0ce548a284743a445e3680d7db5ac3ac
        18ff9b538d16f290ae67f760984dc659
        4a7c15e9716ed28dc027beceea1ec40a
      |};
    case 4
      ~secret:
        "f5e5767cf153319517630f226876b86c 8160cc583bc013744c6bf255f5cc0ee5"
      ~public:
        "278117fc144c72340f67d0f2316e8386 ceffbf2b2428c9c51fef7c597f1d426e"
      ~msg:
        {|
   08b8b2b733424243760fe426a4b54908
   632110a66c2f6591eabd3345e3e4eb98
   fa6e264bf09efe12ee50f8f54e9f77b1
   e355f6c50544e23fb1433ddf73be84d8
   79de7c0046dc4996d9e773f4bc9efe57
   38829adb26c81b37c93a1b270b20329d
   658675fc6ea534e0810a4432826bf58c
   941efb65d57a338bbd2e26640f89ffbc
   1a858efcb8550ee3a5e1998bd177e93a
   7363c344fe6b199ee5d02e82d522c4fe
   ba15452f80288a821a579116ec6dad2b
   3b310da903401aa62100ab5d1a36553e
   06203b33890cc9b832f79ef80560ccb9
   a39ce767967ed628c6ad573cb116dbef
   efd75499da96bd68a8a97b928a8bbc10
   3b6621fcde2beca1231d206be6cd9ec7
   aff6f6c94fcd7204ed3455c68c83f4a4
   1da4af2b74ef5c53f1d8ac70bdcb7ed1
   85ce81bd84359d44254d95629e9855a9
   4a7c1958d1f8ada5d0532ed8a5aa3fb2
   d17ba70eb6248e594e1a2297acbbb39d
   502f1a8c6eb6f1ce22b3de1a1f40cc24
   554119a831a9aad6079cad88425de6bd
   e1a9187ebb6092cf67bf2b13fd65f270
   88d78b7e883c8759d2c4f5c65adb7553
   878ad575f9fad878e80a0c9ba63bcbcc
   2732e69485bbc9c90bfbd62481d9089b
   eccf80cfe2df16a2cf65bd92dd597b07
   07e0917af48bbb75fed413d238f5555a
   7a569d80c3414a8d0859dc65a46128ba
   b27af87a71314f318c782b23ebfe808b
   82b0ce26401d2e22f04d83d1255dc51a
   ddd3b75a2b1ae0784504df543af8969b
   e3ea7082ff7fc9888c144da2af58429e
   c96031dbcad3dad9af0dcbaaaf268cb8
   fcffead94f3c7ca495e056a9b47acdb7
   51fb73e666c6c655ade8297297d07ad1
   ba5e43f1bca32301651339e22904cc8c
   42f58c30c04aafdb038dda0847dd988d
   cda6f3bfd15c4b4c4525004aa06eeff8
   ca61783aacec57fb3d1f92b0fe2fd1a8
   5f6724517b65e614ad6808d6f6ee34df
   f7310fdc82aebfd904b01e1dc54b2927
   094b2db68d6f903b68401adebf5a7e08
   d78ff4ef5d63653a65040cf9bfd4aca7
   984a74d37145986780fc0b16ac451649
   de6188a7dbdf191f64b5fc5e2ab47b57
   f7f7276cd419c17a3ca8e1b939ae49e4
   88acba6b965610b5480109c8b17b80e1
   b7b750dfc7598d5d5011fd2dcc5600a3
   2ef5b52a1ecc820e308aa342721aac09
   43bf6686b64b2579376504ccc493d97e
   6aed3fb0f9cd71a43dd497f01f17c0e2
   cb3797aa2a2f256656168e6c496afc5f
   b93246f6b1116398a346f1a641f3b041
   e989f7914f90cc2c7fff357876e506b5
   0d334ba77c225bc307ba537152f3f161
   0e4eafe595f6d9d90d11faa933a15ef1
   369546868a7f3a45a96768d40fd9d034
   12c091c6315cf4fde7cb68606937380d
   b2eaaa707b4c4185c32eddcdd306705e
   4dc1ffc872eeee475a64dfac86aba41c
   0618983f8741c5ef68d3a101e8a3b8ca
   c60c905c15fc910840b94c00a0b9d0
     |}
      ~signature:
        {|
   0aab4c900501b3e24d7cdf4663326a3a
   87df5e4843b2cbdb67cbf6e460fec350
   aa5371b1508f9f4528ecea23c436d94b
   5e8fcd4f681e30a6ac00a9704a188a03
     |};
    case 5
      ~secret:
        "833fe62409237b9d62ec77587520911e 9a759cec1d19755b7da901b96dca3d42"
      ~public:
        "ec172b93ad5e563bf4932c70e1245034 c35467ef2efd4d64ebf819683467e2bf"
      ~msg:
        {|
   ddaf35a193617abacc417349ae204131
   12e6fa4e89a97ea20a9eeee64b55d39a
   2192992a274fc1a836ba3c23a3feebbd
   454d4423643ce80e2a9ac94fa54ca49f
     |}
      ~signature:
        {|
   dc2a4459e7369633a52b1bf277839a00
   201009a3efbf3ecb69bea2186c26b589
   09351fc9ac90b3ecfdfbc7c66431e030
   3dca179c138ac17ad9bef1177331a704
     |};
  ]

let ed25519_cs_with_offs () =
  let cs = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal in
  let test secret public msg signature =
    Alcotest.(
      check cs "public key is ok" (Ed25519.pub_to_cstruct public)
        Ed25519.(pub_to_cstruct (pub_of_priv secret)));
    Alcotest.(check cs "signature is ok" signature (Ed25519.sign ~key:secret msg));
    Alcotest.(check bool "verify is ok" true
                (Ed25519.verify ~key:public signature ~msg))
  in
  let case ~secret ~public ~msg ~signature =
    let s =
      match Ed25519.priv_of_cstruct Cstruct.(shift (of_hex secret) 1) with
      | Ok p ->
        Alcotest.(check cs "private key encoding is good"
                    Cstruct.(shift (of_hex secret) 1)
                    (Ed25519.priv_to_cstruct p));
        p
      | Error _ -> Alcotest.fail "failed to decode private key"
    and p =
      match Ed25519.pub_of_cstruct Cstruct.(shift (of_hex public) 2) with
      | Ok p ->
        Alcotest.(check cs "public key encoding is good"
                    Cstruct.(shift (of_hex public) 2)
                    (Ed25519.pub_to_cstruct p));
        p
      | Error _ -> Alcotest.fail "failed to decode public key"
    and m = Cstruct.(shift (of_hex msg) 3)
    and si = Cstruct.(shift (of_hex signature) 4)
    in
    test s p m si
  in
  case
    ~secret:
      "00 9d61b19deffd5a60ba844af492ec2cc4 4449c5697b326919703bac031cae7f60"
    ~public:
      "0000 d75a980182b10ab7d54bfed3c964073a 0ee172f3daa62325af021a68f707511a"
    ~msg:"000000"
    ~signature:
      {|00000000
        e5564300c360ac729086e2cc806e828a
        84877f1eb8e5d974d873e06522490155
        5fb8821590a33bacc61e39701cf9b46b
        d25bf5f0595bbe24655141438e7a100b
      |}

let p521_regression () =
  let key = Cstruct.of_hex
"04 01 e4 f8 8a 40 3d fe  2f 65 a0 20 50 01 9b 87
86 2c 30 2f 64 58 de 68  63 ab 92 72 88 04 c6 20
7b 6f 9a 52 95 2d ff c7  80 df 50 44 b1 c4 91 e3
a7 65 39 e6 9c cf ed d2  2a eb 47 84 ea 0f 3d 05
dd 25 0e 00 95 6e 19 fb  7f b7 ce 47 5a 59 01 5f
35 33 fc 85 ac 34 1a b0  7a 67 86 e8 3e 31 fe 38
35 5c bb a1 b5 74 f4 47  a3 4c 0a f0 5f 6d 68 47
85 0f e9 79 74 23 e8 75  47 6e 2b e5 ea 1b 0a 36
b9 c3 94 ca b0"
  and data = Cstruct.of_hex
"a8 98 57 b9 3f 58 02 c7  9a 37 e2 d7 89 d8 0b f4
2d 84 c2 24 7c 7f ff 5f  7b 65 c5 17 cf 79 7d 36
ff d3 9d 47 5e 68 90 57  f1 61 48 18 04 c3 fe ee
59 b2 15 2d 75 8b 9a 3c  52 60 96 5c 52 a8 55 9c"
  and sigr = Cstruct.of_hex
"3a 2c 99 0b 61 a1 da 06  20 bf 6c fe 1f d3 f8 2a
cb f1 e5 0f 78 11 61 58  22 e4 a0 5f 18 81 8d 98
f8 7a ca 8b f8 f8 cc b8  95 f7 6f 03 54 1b 66 6e
cf c5 cb f1 7b 48 82 d2  c3 0e 0e 1b b4 ad e6 a4
5c"
  and sigs = Cstruct.of_hex
"01 7b 8c 82 a5 aa 80 c5  ee 23 0f 91 55 89 a7 b0
3c 46 7f 56 ff b4 52 89  52 99 59 1e 5e b7 f2 c1
df f8 a0 4f d3 dd 1d f0  07 78 3a 2f 29 d6 61 61
55 dc 3b be 14 82 93 75  c2 0d be 7e ca 50 e4 3c
98 88"
  in
  match P521.Dsa.pub_of_cstruct key with
  | Ok key ->
    Alcotest.check Alcotest.bool "regression 1" true
      (P521.Dsa.verify ~key (sigr, sigs) data)
  | Error _ -> Alcotest.fail "regression failed"

let () =
  Mirage_crypto_rng_unix.initialize ();
  Alcotest.run "EC"
    [
      ("P256 Key exchange", key_exchange);
      ("P256 Low level scalar mult", scalar_mult);
      ("P256 Point validation", point_validation);
      ("P256 Scalar validation when generating", scalar_validation);
      ("ECDSA NIST", ecdsa);
      ("ECDSA RFC 6979 P224", ecdsa_rfc6979_p224);
      ("ECDSA RFC 6979 P256", ecdsa_rfc6979_p256);
      ("ECDSA RFC 6979 P384", ecdsa_rfc6979_p384);
      ("ECDSA RFC 6979 P521", ecdsa_rfc6979_p521);
      ("X25519", [ "RFC 7748", `Quick, x25519 ]);
      ("ED25519", ed25519);
      ("ED25519 with offsets", [ "one", `Quick, ed25519_cs_with_offs ]);
      ("ECDSA P521 regression", [ "regreesion1", `Quick, p521_regression ]);
    ]
