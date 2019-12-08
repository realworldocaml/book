module M() = struct
  let%expect_test _ =
    print_string "hello world";
    [%expect {| hello world |}]
end

module A = M()
module B = M()
