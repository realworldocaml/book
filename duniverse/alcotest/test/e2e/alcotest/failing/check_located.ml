let here : Lexing.position =
  { pos_fname = __FILE__; pos_lnum = 2; pos_bol = 20; pos_cnum = 50 }

let pos = __POS__

let () =
  let open Alcotest in
  let msg = "Expected failure" in
  let tc msg f = Alcotest.test_case msg `Quick f in
  run ~verbose:true __FILE__
    [
      ( "check",
        [
          tc "here" (fun () -> check ~here bool msg true false);
          tc "pos" (fun () -> check ~pos bool msg true false);
        ] );
      ( "check_raises",
        [
          tc "here" (fun () ->
              check_raises ~here msg (Failure "") (fun () -> ()));
          tc "pos" (fun () -> check_raises ~pos msg (Failure "") (fun () -> ()));
        ] );
      ( "fail",
        [
          tc "here" (fun () -> fail ~here msg);
          tc "pos" (fun () -> fail ~pos msg);
        ] );
    ]
