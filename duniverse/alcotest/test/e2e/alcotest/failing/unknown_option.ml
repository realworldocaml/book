let () =
  Alcotest.run __FILE__
    [ ("alpha", [ Alcotest.test_case "1" `Quick (fun () -> ()) ]) ]
