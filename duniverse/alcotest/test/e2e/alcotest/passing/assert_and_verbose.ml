(** Test the interaction between ASSERT prints and the `--verbose` option. *)

let () =
  let open Alcotest in
  run ~verbose:true "assert-and-verbose"
    [
      ( "alpha",
        [
          Alcotest.test_case "check → stdout" `Quick (fun () ->
              Alcotest.(check unit) "alpha-0 check" () ();
              Format.printf "alpha-0 standard out\n");
          Alcotest.test_case "stdout → check" `Quick (fun () ->
              Format.printf "\nalpha-1 standard out\n";
              Alcotest.(check unit) "alpha-1 check" () ());
          Alcotest.test_case "check → stderr" `Quick (fun () ->
              Alcotest.(check unit) "alpha-2 check" () ();
              Format.eprintf "alpha-2 standard error\n");
        ] );
      ( "beta",
        [
          Alcotest.test_case "stdout → check → stderr" `Quick (fun () ->
              Format.printf "\nbeta-0 standard out\n";
              Alcotest.(check unit) "beta-0 check" () ();
              Format.eprintf "beta-0 standard error\n");
        ] );
    ]
