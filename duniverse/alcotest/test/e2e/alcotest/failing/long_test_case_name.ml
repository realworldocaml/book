let () =
  let open Alcotest in
  run __FILE__
    [
      ( "test-a",
        [
          test_case "Passing" `Quick (fun () -> ());
          test_case
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
             Suspendisse mollis, orci sed venenatis efficitur, eros est \
             imperdiet purus, sit amet tincidunt massa diam ut elit."
            `Quick (fun () -> Alcotest.fail "Failed");
        ] );
    ]
