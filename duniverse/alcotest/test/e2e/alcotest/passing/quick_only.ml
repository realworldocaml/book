let () =
  let open Alcotest in
  let id () = () in
  run ~argv:[| ""; "--quick" |] __FILE__
    [
      ("test-a", [ test_case "Quick" `Quick id; test_case "Slow" `Slow id ]);
      ("test-b", [ test_case "Slow" `Slow id; test_case "Quick" `Quick id ]);
    ]
