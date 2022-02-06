open Alcotest_stdlib_ext

let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "test-a",
        List.init 200 (fun i ->
            test_case (Format.sprintf "Test case #%d" i) `Quick id) );
      ("test-b", [ test_case "Third test case" `Quick id ]);
      ("test-c", [ test_case "Fourth test case" `Slow id ]);
    ]
