let suites =
  X509tests.x509_tests @ [
    "Regression", Regression.regression_tests ;
    "Host names", Regression.hostname_tests ;
    "Revoke", Revoke.revoke_tests ;
    "CRL", Crltests.crl_tests ;
  ]


let () =
  Printexc.record_backtrace true;
  Mirage_crypto_rng_unix.initialize ();
  Alcotest.run "X509 tests" suites
