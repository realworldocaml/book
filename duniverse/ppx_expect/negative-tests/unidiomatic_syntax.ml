[%%expect_test
  let _ =
    Printf.printf "Hello, world.\n";
    [%expect {| Good night, moon. |}]]
;;
