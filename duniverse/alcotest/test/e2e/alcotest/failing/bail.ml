let () =
  let tc name f = Alcotest.test_case name `Quick f in
  Alcotest.run ~bail:true __FILE__
    [
      ("passing", [ tc "a" (fun () -> ()) ]);
      ( "failing",
        [
          tc "b" (fun () -> failwith "Expected failure");
          tc "not_run" (fun () -> assert false);
        ] );
    ]
