(** Ensures that the `--verbose` flag is passed from the CLI. *)

let () =
  let open Alcotest in
  run ~verbose:false (* CLI flag should take priority over this option *)
    "cli_verbose"
    [
      ( "alpha",
        [
          test_case "0" `Quick (fun () ->
              Format.printf "SHOULD APPEAR IN TEST OUTPUT\n");
        ] );
    ]
