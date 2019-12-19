let%expect_test "expect.output" =
  Printf.printf "hello\n";
  let output = [%expect.output] in
  Printf.printf "'%s'\n" (String.uppercase_ascii output);
  [%expect {|
    'HELLO
    ' |}];
  Printf.printf "string without line break";
  let output = [%expect.output] in
  Printf.printf "%s\n" (String.uppercase_ascii output);
  [%expect {| STRING WITHOUT LINE BREAK |}]
;;

let%expect_test "Ensure repeated expect.outputs clean up in-betweeen" =
  Printf.printf "first";
  let output1 = [%expect.output] in
  Printf.printf "second";
  let output2 = [%expect.output] in
  Printf.printf "%s" (String.uppercase_ascii output2);
  [%expect {| SECOND |}];
  Printf.printf "%s" (String.uppercase_ascii output1);
  [%expect {| FIRST |}]
;;
