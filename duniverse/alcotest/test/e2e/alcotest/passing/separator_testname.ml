let () =
  Alcotest.run __FILE__
    [
      ( "with/separator",
        [ Alcotest.test_case "First test case" `Quick (fun () -> ()) ] );
    ]
