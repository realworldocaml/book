open OUnit2

let suite =
  "All" >::: [
    "Numeric" >::: Test_numeric.suite;
    "DHE" >::: Test_dh.suite;
    "DSA" >::: Test_dsa.suite;
    "RSA" >::: Test_rsa.suite;
  ]

let () =
  Mirage_crypto_rng_unix.initialize ();
  run_test_tt_main suite
