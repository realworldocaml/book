let%expect_test "no sanitization" =
  print_endline "hi!";
  [%expect {| hi! |}]
;;

let%test_module _ =
  (module struct
    module Expect_test_config = struct
      include Expect_test_config

      let sanitize s = if s = "" then "" else "local module sanitize: " ^ s
    end

    let%expect_test "local sanitize" =
      print_endline "hi!";
      [%expect {| local module sanitize: hi! |}]
    ;;
  end)
;;

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = if s = "" then "" else "SANITIZED: " ^ s
end

let%expect_test "sanitize" =
  print_endline "hi!";
  [%expect {| SANITIZED: hi! |}]
;;
