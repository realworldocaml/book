let free () =
  print_endline "freeing all resources";
  Lwt.return ()

let test_lwt switch () =
  Lwt_switch.add_hook (Some switch) free;
  Lwt.async (fun () -> failwith "All is broken");
  Lwt_unix.sleep 10.

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run __FILE__
       [
         ( "all",
           [
             test_case "one" `Quick test_lwt;
             test_case "two" `Quick (fun _ () -> Lwt.return_unit);
           ] );
       ]
