let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "alpha",
        [
          test_case "Output may or may not contain ANSII escape codes" `Quick id;
          test_case "according to whether or not [--color] is set." `Quick id;
          test_case "(See the corresponding [dune] file.)" `Quick id;
        ] );
    ]
