let () =
  let open Alcotest in
  let id () = () in
  run ~and_exit:false __FILE__
    [ ("test-a", [ test_case "Test case" `Quick id ]) ];
  Printf.printf "\n Program execution continued!%!"
