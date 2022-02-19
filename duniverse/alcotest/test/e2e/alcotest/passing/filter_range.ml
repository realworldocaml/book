open Alcotest_stdlib_ext

let () =
  let should_run i =
    (* Corresponds to ranges specified in [filter_range.opts]. *)
    List.mem i [ 0; 1; 2; 3; 5 ]
  in
  let tests =
    List.init 6 (fun i ->
        Alcotest.test_case (Printf.sprintf "test-%d" i) `Quick (fun () ->
            if not (should_run i) then failwith "Test should have been skipped"))
  in
  Alcotest.run __FILE__ [ ("main", tests) ]
