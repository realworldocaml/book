let () =
  Alcotest.run __FILE__
    [ ("", [ Alcotest.test_case "1" `Quick (fun () -> ()) ]) ]
