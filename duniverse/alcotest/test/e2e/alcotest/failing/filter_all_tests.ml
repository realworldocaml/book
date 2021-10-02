(** Ensure that filters which eliminate all tests are rejected. *)

let () =
  Alcotest.run __FILE__
    [
      ("foo", [ Alcotest.test_case "1" `Quick (fun () -> assert false) ]);
      ("bar", [ Alcotest.test_case "2" `Quick (fun () -> assert false) ]);
    ]
