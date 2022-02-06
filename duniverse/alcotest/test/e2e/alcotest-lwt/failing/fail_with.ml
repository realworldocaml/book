let test_lwt _switch () = Lwt.fail_with "should fail"

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run ~record_backtrace:false __FILE__
       [
         ( "all",
           [
             test_case "one" `Quick test_lwt;
             test_case "two" `Quick (fun _ () -> Lwt.return_unit);
           ] );
       ]
