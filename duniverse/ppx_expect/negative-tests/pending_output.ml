module Expect_test_config :
  Expect_test_config_types.S with type 'a IO_flush.t = 'a with type 'a IO_run.t = 'a
= struct
  include Expect_test_config

  let flushed () =
    print_string "blah\n";
    false
  ;;
end

let%expect_test _ =
  print_endline "foo";
  [%expect {| foo
           |}]
;;
