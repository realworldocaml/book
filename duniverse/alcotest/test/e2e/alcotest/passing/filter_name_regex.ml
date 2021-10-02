let () =
  let open Alcotest in
  let id () = () in
  run
    ~argv:[| ""; "test"; "basic-.*-(a|c)" |]
    __FILE__
    [
      ( "basic-run-a",
        [ test_case "Executed" `Quick id; test_case "Also executed" `Quick id ]
      );
      ("basic-run-b", [ test_case "Skipped" `Quick id ]);
      ("basic-run-c", [ test_case "Executed" `Quick id ]);
      ("complex-run-a", [ test_case "Skipped" `Quick id ]);
    ]
