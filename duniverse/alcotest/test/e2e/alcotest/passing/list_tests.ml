let () =
  let open Alcotest in
  let failtest () = invalid_arg "This test should never be run" in
  run ~argv:[| ""; "list" |] __FILE__
    [
      ( "test-a",
        [ test_case "alpha" `Quick failtest; test_case "beta" `Quick failtest ]
      );
      ( "test-b",
        [
          test_case "lorem ipsum dolor sit amet consectutor adipiscing elit"
            `Quick failtest;
        ] );
    ]
