let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Results should be in JSON, since --json is passed"
       [ ("test", [ test_case "alpha" `Quick (fun _ () -> Lwt.return_unit) ]) ]
