let test_error_output () =
  for i = 1 to 100 do
    Printf.printf "output line %i\n" i
  done;
  Alcotest.fail "Logs above should be 101 lines long."

let () =
  let open Alcotest in
  run ~tail_errors:`Unlimited __FILE__
    [ ("failing", [ test_case "test" `Quick test_error_output ]) ]
