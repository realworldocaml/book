
let test ~hash ~ikm ?salt ?info ~l ~prk ~okm () =
  let ikm = Cstruct.of_hex ikm
  and salt = match salt with None -> None | Some x -> Some (Cstruct.of_hex x)
  and info = match info with None -> None | Some x -> Some (Cstruct.of_hex x)
  and prk = Cstruct.of_hex prk
  and okm = Cstruct.of_hex okm
  in
  (fun () ->
   let cprk = Hkdf.extract ~hash ?salt ikm in
   let prk = Cstruct.to_string prk
   and scprk = Cstruct.to_string cprk
   in
   Alcotest.check Alcotest.string "PRK matches" prk scprk ;
   let cokm = Hkdf.expand ~hash ~prk:cprk ?info l in
   let okm = Cstruct.to_string okm
   and cokm = Cstruct.to_string cokm
   in
   Alcotest.check Alcotest.string "OKM matches" okm cokm)

let test1 =
  test
    ~hash:`SHA256
    ~ikm:"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    ~salt:"000102030405060708090a0b0c"
    ~info:"f0f1f2f3f4f5f6f7f8f9"
    ~l:42
    ~prk:"077709362c2e32df0ddc3f0dc47bba63 \
          90b6c73bb50f9c3122ec844ad7c2b3e5"
    ~okm:"3cb25f25faacd57a90434f64d0362f2a \
          2d2d0a90cf1a5a4c5db02d56ecc4c5bf \
          34007208d5b887185865"
    ()

and test2 =
  test
    ~hash:`SHA256
    ~ikm:"000102030405060708090a0b0c0d0e0f \
          101112131415161718191a1b1c1d1e1f \
          202122232425262728292a2b2c2d2e2f \
          303132333435363738393a3b3c3d3e3f \
          404142434445464748494a4b4c4d4e4f"
    ~salt:"606162636465666768696a6b6c6d6e6f \
           707172737475767778797a7b7c7d7e7f \
           808182838485868788898a8b8c8d8e8f \
           909192939495969798999a9b9c9d9e9f \
           a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
    ~info:"b0b1b2b3b4b5b6b7b8b9babbbcbdbebf \
           c0c1c2c3c4c5c6c7c8c9cacbcccdcecf \
           d0d1d2d3d4d5d6d7d8d9dadbdcdddedf \
           e0e1e2e3e4e5e6e7e8e9eaebecedeeef \
           f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
    ~l:82
    ~prk:"06a6b88c5853361a06104c9ceb35b45c \
          ef760014904671014a193f40c15fc244"
    ~okm:"b11e398dc80327a1c8e7f78c596a4934 \
          4f012eda2d4efad8a050cc4c19afa97c \
          59045a99cac7827271cb41c65e590e09 \
          da3275600c2f09b8367793a9aca3db71 \
          cc30c58179ec3e87c14c01d5c1f3434f \
          1d87"
    ()

and test3 =
  test
    ~hash:`SHA256
    ~ikm:"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    ~salt:""
    (* info = (0 octets) *)
    ~l:42
    ~prk:"19ef24a32c717b167f33a91d6f648bdf \
          96596776afdb6377ac434c1c293ccb04"
    ~okm:"8da4e775a563c18f715f802a063c5a31 \
          b8a11f5c5ee1879ec3454e5f3c738d2d \
          9d201395faa4b61a96c8"
    ()

and test4 =
  test
    ~hash:`SHA1
    ~ikm:"0b0b0b0b0b0b0b0b0b0b0b"
    ~salt:"000102030405060708090a0b0c"
    ~info:"f0f1f2f3f4f5f6f7f8f9"
    ~l:42
    ~prk:"9b6c18c432a7bf8f0e71c8eb88f4b30baa2ba243"
    ~okm:"085a01ea1b10f36933068b56efa5ad81 \
          a4f14b822f5b091568a9cdd4f155fda2 \
          c22e422478d305f3f896"
    ()

and test5 =
  test
    ~hash:`SHA1
    ~ikm:"000102030405060708090a0b0c0d0e0f \
          101112131415161718191a1b1c1d1e1f \
          202122232425262728292a2b2c2d2e2f \
          303132333435363738393a3b3c3d3e3f \
          404142434445464748494a4b4c4d4e4f"
   ~salt:"606162636465666768696a6b6c6d6e6f \
          707172737475767778797a7b7c7d7e7f \
          808182838485868788898a8b8c8d8e8f \
          909192939495969798999a9b9c9d9e9f \
          a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
   ~info:"b0b1b2b3b4b5b6b7b8b9babbbcbdbebf \
          c0c1c2c3c4c5c6c7c8c9cacbcccdcecf \
          d0d1d2d3d4d5d6d7d8d9dadbdcdddedf \
          e0e1e2e3e4e5e6e7e8e9eaebecedeeef \
          f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
   ~l:82
   ~prk:"8adae09a2a307059478d309b26c4115a224cfaf6"
   ~okm:"0bd770a74d1160f7c9f12cd5912a06eb \
         ff6adcae899d92191fe4305673ba2ffe \
         8fa3f1a4e5ad79f3f334b3b202b2173c \
         486ea37ce3d397ed034c7f9dfeb15c5e \
         927336d0441f4c4300e2cff0d0900b52 \
         d3b4"
   ()

and test6 =
  test
    ~hash:`SHA1
    ~ikm:"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    ~salt:""
    (* info = (0 octets) *)
    ~l:42
    ~prk:"da8c8a73c7fa77288ec6f5e7c297786aa0d32d01"
    ~okm:"0ac1af7002b3d761d1e55298da9d0506 \
          b9ae52057220a306e07b6b87e8df21d0 \
          ea00033de03984d34918"
    ()

and test7 =
  test
    ~hash:`SHA1
    ~ikm:"0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"
    (* salt = not provided (defaults to HashLen zero octets) *)
    (* info = (0 octets) *)
    ~l:42
    ~prk:"2adccada18779e7c2077ad2eb19d3f3e731385dd"
    ~okm:"2c91117204d745f3500d636a62f64f0a \
          b3bae548aa53d423b0d1f27ebba6f5e5 \
          673a081d70cce7acfc48"
    ()

let tests = [
  "RFC 5869 Test Case 1", `Quick, test1 ;
  "RFC 5869 Test Case 2", `Quick, test2 ;
  "RFC 5869 Test Case 3", `Quick, test3 ;
  "RFC 5869 Test Case 4", `Quick, test4 ;
  "RFC 5869 Test Case 5", `Quick, test5 ;
  "RFC 5869 Test Case 6", `Quick, test6 ;
  "RFC 5869 Test Case 7", `Quick, test7 ;
]

let () = Alcotest.run "HKDF Tests" [ "RFC 5869", tests ]
