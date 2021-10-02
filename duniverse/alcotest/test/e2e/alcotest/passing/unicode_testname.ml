let () =
  Alcotest.run
    "Suite name containing file separators / and non-ASCII characters 🔥"
    [
      ( "🔥",
        [
          Alcotest.test_case "Non ASCII unicode character" `Quick (fun () -> ());
        ] );
      ( "🔥a-b",
        [
          Alcotest.test_case "Non ASCII and ASCII characters" `Quick (fun () ->
              ());
        ] );
    ]
