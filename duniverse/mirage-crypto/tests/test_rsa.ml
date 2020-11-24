open OUnit2

open Mirage_crypto.Uncommon
open Mirage_crypto
open Mirage_crypto_pk

open Test_common
open Test_common_random

let vz = Z.of_string_base 16

module Null = struct

  type g = Cstruct.t ref

  let block = 1

  let create ?time:_ () = ref Cstruct.empty

  let generate ~g n =
    try
      let (a, b) = Cstruct.split !g n in ( g := b ; a )
    with Invalid_argument _ -> raise Mirage_crypto_rng.Unseeded_generator

  let reseed ~g cs = g := Cs.(!g <+> cs)

  let seeded ~g = Cstruct.len !g > 0

  let accumulate ~g _source = `Acc (reseed ~g)

  let pools = 0
end

let random_is seed =
  Mirage_crypto_rng.create ~seed (module Null)

let gen_rsa ~bits =
  let e     = Z.(if bits < 24 then ~$3 else ~$0x10001) in
  let key   = Rsa.(generate ~e ~bits ()) in
  let key_s = Sexplib.Sexp.to_string_hum Rsa.(sexp_of_priv key) in
  assert_equal
    ~msg:Printf.(sprintf "key size not %d bits:\n%s" bits key_s)
    bits Rsa.(priv_bits key);
  (key, key_s)

let rsa_priv_of_primes_regression _ =
  let e = Z.of_string "65537"
  and p = Z.of_string "63541376186162969"
  and q = Z.of_string "31114890003960709"
  in
  match Rsa.priv_of_primes ~e ~p ~q with
  | exception _ -> assert_failure "expected an error"
  | Error _ -> () (* expected since there's no multiplicative inverse of e with p and q (e is not coprime to q-1) *)
  | Ok _ -> assert_failure "expected an error"

let rsa_selftest ~bits n =
  "selftest" >:: times ~n @@ fun _ ->
    let msg =
      let size = bits // 8 in
      let cs = Mirage_crypto_rng.generate size
      and i  = 1 + Randomconv.int ~bound:(pred size) Mirage_crypto_rng.generate in
      Cstruct.set_uint8 cs 0 0;
      Cstruct.(set_uint8 cs i (get_uint8 cs i lor 2));
      cs in
    let (key, key_s) = gen_rsa ~bits in
    let enc = Rsa.(encrypt ~key:(pub_of_priv key) msg) in
    let dec = Rsa.(decrypt ~key enc) in

    assert_cs_equal
      ~msg:Printf.(sprintf "failed decryption with:\n%s" key_s)
      msg dec

let show_key_size key =
  Printf.sprintf "(%d bits)" (Rsa.priv_bits key)

let pkcs_message_for_bits bits =
  let padding = 12 in
  let size    = bits // 8 - padding in
  assert (size >= 0) ; Mirage_crypto_rng.generate size

let rsa_pkcs1_encode_selftest ~bits n =
  "selftest" >:: times ~n @@ fun _ ->
    let (key, _) = gen_rsa ~bits
    and msg      = pkcs_message_for_bits bits in
    let sgn      = Rsa.PKCS1.sig_encode ~key msg in
    match Rsa.(PKCS1.sig_decode ~key:(pub_of_priv key) sgn) with
    | None     -> assert_failure ("unpad failure " ^ show_key_size key)
    | Some dec -> assert_cs_equal msg dec
                    ~msg:("recovery failure " ^ show_key_size key)

let rsa_pkcs1_sign_selftest n =
  let open Hash.SHA1 in
  "selftest" >:: times ~n @@ fun _ ->
    let (key, _) = gen_rsa ~bits:(Rsa.PKCS1.min_key `SHA1)
    and msg      = Mirage_crypto_rng.generate 47 in
    let pkey     = Rsa.pub_of_priv key in
    assert_bool "invert 1" Rsa.PKCS1.(
      verify ~key:pkey ~hashp:any (`Message msg)
        ~signature:(sign ~hash:`SHA1 ~key (`Digest (digest msg))) );
    assert_bool "invert 2" Rsa.PKCS1.(
      verify ~key:pkey ~hashp:any (`Digest (digest msg))
        ~signature:(sign ~hash:`SHA1 ~key (`Message msg)) )

let rsa_pkcs1_encrypt_selftest ~bits n =
  "selftest" >:: times ~n @@ fun _ ->
    let (key, _) = gen_rsa ~bits
    and msg      = pkcs_message_for_bits bits in
    let enc      = Rsa.(PKCS1.encrypt ~key:(pub_of_priv key) msg) in
    match Rsa.PKCS1.decrypt ~key enc with
    | None     -> assert_failure ("unpad failure " ^ show_key_size key)
    | Some dec -> assert_cs_equal msg dec
                    ~msg:("recovery failure " ^ show_key_size key)

let rsa_oaep_encrypt_selftest ~bits n =
  let hashes = [| `MD5; `SHA1; `SHA224; `SHA256 |] in
  "selftest" >:: times ~n @@ fun _ ->
    let module H = (val (Hash.module_of (sample hashes))) in
    let module OAEP = Rsa.OAEP (H) in
    let (key, _) = gen_rsa ~bits
    and msg      = Mirage_crypto_rng.generate (bits // 8 - 2 * H.digest_size - 2) in
    let enc      = OAEP.encrypt ~key:(Rsa.pub_of_priv key) msg in
    match OAEP.decrypt ~key enc with
    | None     -> assert_failure "unpad failure"
    | Some dec -> assert_cs_equal msg dec ~msg:"recovery failure"

let rsa_pss_sign_selftest ~bits n =
  let module Pss_sha1 = Rsa.PSS (Hash.SHA1) in
  let open Hash.SHA1 in
  "selftest" >:: times ~n @@ fun _ ->
    let (key, _) = gen_rsa ~bits
    and msg      = Mirage_crypto_rng.generate 1024 in
    let pkey     = Rsa.pub_of_priv key in
    Pss_sha1.(verify ~key:pkey (`Message msg)
                ~signature:(sign ~key (`Digest (digest msg))))
      |> assert_bool "invert 1" ;
    Pss_sha1.(verify ~key:pkey (`Digest (digest msg))
               ~signature:(Pss_sha1.sign ~key (`Message msg)))
      |> assert_bool "invert 2"

let rsa_pkcs1_cases =
  let key () =
    let n = vz "c8a2069182394a2ab7c3f4190c15589c56a2d4bc42dca675b34cc950e24663048441e8aa593b2bc59e198b8c257e882120c62336e5cc745012c7ffb063eebe53f3c6504cba6cfe51baa3b6d1074b2f398171f4b1982f4d65caf882ea4d56f32ab57d0c44e6ad4e9cf57a4339eb6962406e350c1b15397183fbf1f0353c9fc991"
    and d = vz "5dfcb111072d29565ba1db3ec48f57645d9d8804ed598a4d470268a89067a2c921dff24ba2e37a3ce834555000dc868ee6588b7493303528b1b3a94f0b71730cf1e86fca5aeedc3afa16f65c0189d810ddcd81049ebbd0391868c50edec958b3a2aaeff6a575897e2f20a3ab5455c1bfa55010ac51a7799b1ff8483644a3d425"
    and e = vz "10001"
    in
    match Rsa.priv_of_exp ~e ~d ~n () with
    | Error (`Msg m) -> invalid_arg "bad key %s" m
    | Ok key -> key, Rsa.pub_of_priv key
  in

  let case ~hash ~msg ~sgn = test_case @@ fun _ ->
    let msg = vx msg and sgn = vx sgn in
    let key, public = key () in
    Rsa.(PKCS1.sign ~hash ~key (`Message msg))
      |> assert_cs_equal ~msg:"recomputing sig:" sgn ;
    Rsa.(PKCS1.verify ~hashp:any ~key:public ~signature:sgn (`Message msg))
      |> assert_bool "sig verification" in

  "FIPS 186-2 Test Vectors (1024 bits)" >::: [

    case ~hash:`SHA1
    ~msg:"e8312742ae23c456ef28a23142c4490895832765dadce02afe5be5d31b0048fbeee2cf218b1747ad4fd81a2e17e124e6af17c3888e6d2d40c00807f423a233cad62ce9eaefb709856c94af166dba08e7a06965d7fc0d8e5cb26559c460e47bc088589d2242c9b3e62da4896fab199e144ec136db8d84ab84bcba04ca3b90c8e5"
    ~sgn:"28928e19eb86f9c00070a59edf6bf8433a45df495cd1c73613c2129840f48c4a2c24f11df79bc5c0782bcedde97dbbb2acc6e512d19f085027cd575038453d04905413e947e6e1dddbeb3535cdb3d8971fe0200506941056f21243503c83eadde053ed866c0e0250beddd927a08212aa8ac0efd61631ef89d8d049efb36bb35f"

  ; case ~hash:`SHA1
    ~msg:"4c95073dac19d0256eaadff3505910e431dd50018136afeaf690b7d18069fcc980f6f54135c30acb769bee23a7a72f6ce6d90cbc858c86dbbd64ba48a07c6d7d50c0e9746f97086ad6c68ee38a91bbeeeb2221aa2f2fb4090fd820d4c0ce5ff025ba8adf43ddef89f5f3653de15edcf3aa8038d4686960fc55b2917ec8a8f9a8"
    ~sgn:"53ab600a41c71393a271b0f32f521963087e56ebd7ad040e4ee8aa7c450ad18ac3c6a05d4ae8913e763cfe9623bd9cb1eb4bed1a38200500fa7df3d95dea485f032a0ab0c6589678f9e8391b5c2b1392997ac9f82f1d168878916aace9ac7455808056af8155231a29f42904b7ab87a5d71ed6395ee0a9d024b0ca3d01fd7150"

  ; case ~hash:`SHA1
    ~msg:"e075ad4b0f9b5b20376e467a1a35e308793ba38ed983d03887b8b82eda630e68b8618dc45b93de5555d7bcfed23756401e61f5516757de6ec3687a71755fb4a66cfaa3db0c9e69b631485b4c71c762eea229a0469c7357a440950792ba9cd7ae022a36b9a923c2ebd2aa69897f4cceba0e7aee97033d03810725a9b731833f27"
    ~sgn:"642609ce084f479271df596480252e2f892b3e7982dff95994c3eeda787f80f3f6198bbce33ec5515378d4b571d7186078b75b43aed11d342547386c5696eb3799a0b28475e54cd4ca7d036dcd8a11f5e10806f7d3b8cc4fcb3e93e857be958344a34e126809c15b3d33661cf57bf5c338f07acced60f14019335c152d86b3b2"

  ; case ~hash:`SHA224
    ~msg:"e567a39ae4e5ef9b6801ea0561b72a5d4b5f385f0532fc9fe10a7570f869ae05c0bdedd6e0e22d4542e9ce826a188cac0731ae39c8f87f9771ef02132e64e2fb27ada8ff54b330dd93ad5e3ef82e0dda646248e35994bda10cf46e5abc98aa7443c03cddeb5ee2ab82d60100b1029631897970275f119d05daa2220a4a0defba"
    ~sgn:"5aa5033381bdd0acce332dd314daf008acaa9e835f832979891d1bda2b55d5eae35c479c06cac5bf33f432c8c0a5549d1d1b29c5e2589024d27800a0c235a61532c203cbc406ac6ecf63f52ae771b97c08e4b108ec916900e5a11b1d48cca86ca5a5a799ed32e99c815cef04cf8eb55223bfd4d9c3449264b60061bc3684bc82"

  ; case ~hash:`SHA256
    ~msg:"e567a39ae4e5ef9b6801ea0561b72a5d4b5f385f0532fc9fe10a7570f869ae05c0bdedd6e0e22d4542e9ce826a188cac0731ae39c8f87f9771ef02132e64e2fb27ada8ff54b330dd93ad5e3ef82e0dda646248e35994bda10cf46e5abc98aa7443c03cddeb5ee2ab82d60100b1029631897970275f119d05daa2220a4a0defba"
    ~sgn:"0e7cdd121e40323ca6115d1ec6d1f9561738455f0e9e1cd858e8b566ae2da5e8ee63d8f15c3cdd88027e13406db609369c88ca99b34fa156c7ee62bc5a3923bb5a1edabd45c1a422aafcbb47e0947f35cfef87970b4b713162b21916cafb8c864a3e5b9ffc989401d4eae992312a32c5bc88abbb45f99ac885b54d6b8e61b6ec"

  ; case ~hash:`SHA384
    ~msg:"e567a39ae4e5ef9b6801ea0561b72a5d4b5f385f0532fc9fe10a7570f869ae05c0bdedd6e0e22d4542e9ce826a188cac0731ae39c8f87f9771ef02132e64e2fb27ada8ff54b330dd93ad5e3ef82e0dda646248e35994bda10cf46e5abc98aa7443c03cddeb5ee2ab82d60100b1029631897970275f119d05daa2220a4a0defba"
    ~sgn:"1689a8523919ac77cc997ebc59cb908872d88b2855a309ead2779b888b22b4232da9b93bb19b32c1db77ad738c6e43361e9eb6b1a37c49a8f3c7c7ae7e784d19a62138741293e49b1831c0c3617eb43c56706d83314953470636441086419ab9e6fd1ec4f9d5cc6544815d1e02ed96a3ae64c6998b2cf238e79a12164352d12a"

  ; case ~hash:`SHA512
    ~msg:"e567a39ae4e5ef9b6801ea0561b72a5d4b5f385f0532fc9fe10a7570f869ae05c0bdedd6e0e22d4542e9ce826a188cac0731ae39c8f87f9771ef02132e64e2fb27ada8ff54b330dd93ad5e3ef82e0dda646248e35994bda10cf46e5abc98aa7443c03cddeb5ee2ab82d60100b1029631897970275f119d05daa2220a4a0defba"
    ~sgn:"bf3ff2c69675f1b8ed421021801fb4ce29a757f7f8869ce436d0d75ab749efc8b903d9f9cb214686147f12f3335fa936689c192f310ae3c5d75493f44b24bc1cd3501584aaa5004b65a8716d1eda7240ad8a529d5a0cf169f4054b450e076ee0d41a0011c557aa69a84a8104c909201d60fe39c79e684347ef4d144ea18f7a4e"
  ]

let rsa_pss_cases =
  let key () =
    let n = vz "bcb47b2e0dafcba81ff2a2b5cb115ca7e757184c9d72bcdcda707a146b3b4e29989ddc660bd694865b932b71ca24a335cf4d339c719183e6222e4c9ea6875acd528a49ba21863fe08147c3a47e41990b51a03f77d22137f8d74c43a5a45f4e9e18a2d15db051dc89385db9cf8374b63a8cc88113710e6d8179075b7dc79ee76b"
    and d = vz "383a6f19e1ea27fd08c7fbc3bfa684bd6329888c0bbe4c98625e7181f411cfd0853144a3039404dda41bce2e31d588ec57c0e148146f0fa65b39008ba5835f829ba35ae2f155d61b8a12581b99c927fd2f22252c5e73cba4a610db3973e019ee0f95130d4319ed413432f2e5e20d5215cdd27c2164206b3f80edee51938a25c1"
    and e = vz "10001"
    in
    match Rsa.priv_of_exp ~e ~d ~n () with
    | Error (`Msg m) -> invalid_arg "bad key %s" m
    | Ok key -> key, Rsa.pub_of_priv key
  and salt = "6f2841166a64471d4f0b8ed0dbb7db32161da13b"
  in

  let case ~hash ~msg ~sgn = test_case @@ fun _ ->
    let module H = (val (Hash.module_of hash)) in
    let module Pss = Rsa.PSS (H) in
    let msg = vx msg and sgn = vx sgn and salt = vx salt in
    let key, public = key () in
    let slen = Cstruct.len salt in
    Pss.sign ~g:(random_is salt) ~slen ~mask:`No ~key (`Message msg)
      |> assert_cs_equal ~msg:"recomputing sig:" sgn ;
    Pss.verify ~key:public ~slen ~signature:sgn (`Message msg)
      |> assert_bool "sig verification" in

  "FIPS 186-2 Test Vectors (1024 bits)" >::: [

    case ~hash:`SHA1
    ~msg:"1248f62a4389f42f7b4bb131053d6c88a994db2075b912ccbe3ea7dc611714f14e075c104858f2f6e6cfd6abdedf015a821d03608bf4eba3169a6725ec422cd9069498b5515a9608ae7cc30e3d2ecfc1db6825f3e996ce9a5092926bc1cf61aa42d7f240e6f7aa0edb38bf81aa929d66bb5d890018088458720d72d569247b0c"
    ~sgn:"682cf53c1145d22a50caa9eb1a9ba70670c5915e0fdfde6457a765de2a8fe12de9794172a78d14e668d498acedad616504bb1764d094607070080592c3a69c343d982bd77865873d35e24822caf43443cc10249af6a1e26ef344f28b9ef6f14e09ad839748e5148bcceb0fd2aa63709cb48975cbf9c7b49abc66a1dc6cb5b31a"

  ; case ~hash:`SHA1
    ~msg:"9968809a557bb4f892039ff2b6a0efcd06523624bc3b9ad359a7cf143c4942e874c797b9d37a563d436fe19d5db1aad738caa2617f87f50fc7fcf4361fc85212e89a9465e7f4c361982f64c8c5c0aa5258b9e94f6e934e8dac2ace7cd6095c909de85fe7b973632c384d0ebb165556050d28f236aee70e16b13a432d8a94c62b"
    ~sgn:"8f5ea7037367e0db75670504085790acd6d97d96f51e76df916a0c2e4cd66e1ab51c4cd8e2c3e4ef781f638ad65dc49c8d6d7f6930f80b6ae199ea283a8924925a50edab79bb3f34861ffa8b2f96fdf9f8cad3d3f8f025478c81f316da61b0d6a7f71b9068efdfb33c21983a922f4669280d8e84f963ff885ef56dd3f50381db"

  ; case ~hash:`SHA224
    ~msg:"1248f62a4389f42f7b4bb131053d6c88a994db2075b912ccbe3ea7dc611714f14e075c104858f2f6e6cfd6abdedf015a821d03608bf4eba3169a6725ec422cd9069498b5515a9608ae7cc30e3d2ecfc1db6825f3e996ce9a5092926bc1cf61aa42d7f240e6f7aa0edb38bf81aa929d66bb5d890018088458720d72d569247b0c"
    ~sgn:"53d859c9f10abf1c00284a4b55bf2bd84d8e313b4f3c35b8dec7bc3afe39b9b8a155418ead1931895769ce2340be2091f2385bbcf10d9e92bcf5d0e2960d10e792e7d865c64e50d19ffa13e52817d7d8d8db34392c2374a2e9b69184f92a4ad9b1b8bae99ca614d204b65a438e38dbbfc8c7cc44ed5677af70ce6c4f951f0244"

  ; case ~hash:`SHA256
    ~msg:"1248f62a4389f42f7b4bb131053d6c88a994db2075b912ccbe3ea7dc611714f14e075c104858f2f6e6cfd6abdedf015a821d03608bf4eba3169a6725ec422cd9069498b5515a9608ae7cc30e3d2ecfc1db6825f3e996ce9a5092926bc1cf61aa42d7f240e6f7aa0edb38bf81aa929d66bb5d890018088458720d72d569247b0c"
    ~sgn:"7b1d37278e549898d4084e2210c4a9961edfe7b5963550cca1904248c8681513539017820f0e9bd074b9f8a067b9fefff7f1fa20bf2d0c75015ff020b2210cc7f79034fedf68e8d44a007abf4dd82c26e8b00393723aea15abfbc22941c8cf79481718c008da713fb8f54cb3fca890bde1137314334b9b0a18515bfa48e5ccd0"

  ; case ~hash:`SHA384
    ~msg:"1248f62a4389f42f7b4bb131053d6c88a994db2075b912ccbe3ea7dc611714f14e075c104858f2f6e6cfd6abdedf015a821d03608bf4eba3169a6725ec422cd9069498b5515a9608ae7cc30e3d2ecfc1db6825f3e996ce9a5092926bc1cf61aa42d7f240e6f7aa0edb38bf81aa929d66bb5d890018088458720d72d569247b0c"
    ~sgn:"8f16c807bef3ed6f74ee7ff5c360a5428c6c2f105178b58ff7d073e566dad6e7718d3129c768cd5a9666de2b6c947177b45709dc7cd0f43b0ba6fc75578e1196acc15ca3afe4a78c144cb6885c1cc815f7f98925bc04ad2ff20fc1068b045d9450e2a1dcf5a161ceabba2b0b66c7354fdb80fa1d729e5f976387f24a697a7e56"

  ; case ~hash:`SHA512
    ~msg:"1248f62a4389f42f7b4bb131053d6c88a994db2075b912ccbe3ea7dc611714f14e075c104858f2f6e6cfd6abdedf015a821d03608bf4eba3169a6725ec422cd9069498b5515a9608ae7cc30e3d2ecfc1db6825f3e996ce9a5092926bc1cf61aa42d7f240e6f7aa0edb38bf81aa929d66bb5d890018088458720d72d569247b0c"
    ~sgn:"a833ba31634f8773e4fe6ea0c69e1a23766a939d34b32fc78b774b22e46a646c25e6e1062d234ed48b1aba0f830529ff6afc296cc8dc207bbc15391623beac5f6c3db557ca49d0e42c962de95b5ff548cff970f5c73f439cfe82d3907be60240f56b6a4259cc96dfd8fe02a0bfa26e0223f68214428fff0ae40162198cc5cbd1"
  ]

let suite = [
  "RSA" >::: [
    rsa_selftest ~bits:89   100  ;
    rsa_selftest ~bits:131  100  ;
    rsa_selftest ~bits:1024 10   ;
    rsa_selftest ~bits:2048 10   ;
  ] ;

  "RSA-PKCS1-ENC" >::: [
    rsa_pkcs1_encrypt_selftest ~bits:111 1000 ;
    rsa_pkcs1_encrypt_selftest ~bits:512 10 ;
  ] ;

  "RSA-PKCS1-SIGN" >::: [
    rsa_pkcs1_encode_selftest ~bits:111 100 ;
    rsa_pkcs1_encode_selftest ~bits:512 10 ;
    rsa_pkcs1_sign_selftest 10;
    rsa_pkcs1_cases;
  ] ;

  "RSA-OAEP(SHA1)-ENC" >::: [
    rsa_oaep_encrypt_selftest ~bits:1023 15 ;
    rsa_oaep_encrypt_selftest ~bits:1024 15 ;
    rsa_oaep_encrypt_selftest ~bits:1025 15 ;
  ] ;

  "RSA-PSS(SHA1)-END" >::: [
    rsa_pss_sign_selftest ~bits:511 15 ;
    rsa_pss_sign_selftest ~bits:512 15 ;
    rsa_pss_sign_selftest ~bits:513 15 ;
    rsa_pss_cases
  ] ;

  "RSA-regression" >::: [
    test_case rsa_priv_of_primes_regression
  ] ;
]
