module Expect_test_config = struct
  include Expect_test_config
  let upon_unreleasable_issue = `Warning_for_collector_testing
end

let%expect_test _ =
  let f output =
    print_string output;
    [%expect {| \(foo\|bar\) (regexp) |}]
  in
  f "foo";
  f "bar";
;;
