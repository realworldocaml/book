let test_error_output () =
  for i = 1 to 100 do
    Printf.printf "output line %i\n" i
  done;
  Alcotest.fail
    "Logs should be 10 lines long, including this line (omitting 92)."

let () =
  let open Alcotest in
  run ~record_backtrace:false ~tail_errors:(`Limit 10) __FILE__
    [ ("failing", [ test_case "test" `Quick test_error_output ]) ]
