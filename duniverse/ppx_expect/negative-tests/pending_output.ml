module Expect_test_config : Expect_test_config.S with type 'a IO.t = 'a = struct
  include Expect_test_config

  let flushed () = print_string "blah\n"; false
end

let%expect_test _ =
  print_endline "foo";
  [%expect {| foo
           |}]
;;

