(** Reproduction for https://github.com/mirage/alcotest/issues/225, testing the
    interaction between `--verbose` and newlines in the test stdout. *)

let () =
  Alcotest.run ~verbose:true __FILE__
    [
      ( "alpha",
        [
          Alcotest.test_case "0 newlines" `Quick (fun () ->
              Format.printf "Print inside alpha");
        ] );
      ( "beta",
        (* Progressive result reporting is broken for this test, since the
           carriage return happens on the wrong line. *)
        [
          Alcotest.test_case "1 newline" `Quick (fun () ->
              Format.printf "Print inside beta\n");
        ] );
      ( "gamma",
        [
          (* Reporting is also broken here. Even worse, some of the test std.out
             is clipped by the eventual result of the test. *)
          Alcotest.test_case "1 newline + long line" `Quick (fun () ->
              Format.printf
                "Print inside gamma\n\
                 Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
                 nullam malesuada dictum tortor in venenatis.");
        ] );
      ( "delta",
        [
          Alcotest.test_case "1 newline + long check" `Quick (fun () ->
              Format.printf "Print inside delta\n";
              Alcotest.(check unit)
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
                 nullam malesuada dictum tortor in venenatis."
                () ());
        ] );
    ]
