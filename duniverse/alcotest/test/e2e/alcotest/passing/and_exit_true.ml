let () =
  let open Alcotest in
  let id () = () in
  run ~and_exit:true __FILE__
    [ ("test-a", [ test_case "Test case" `Quick id ]) ];
  Printf.printf "\n Should never be printed!%!";
  assert false
