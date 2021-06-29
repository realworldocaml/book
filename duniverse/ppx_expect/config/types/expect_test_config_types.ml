module type S = Expect_test_config_types_intf.S

module type Expect_test_config_types =
  Expect_test_config_types_intf.Expect_test_config_types

module Upon_unreleasable_issue = struct
  include Expect_test_config_types_intf.Upon_unreleasable_issue

  let equal t1 t2 = t1 = t2

  let comment_prefix = function
    | `CR -> "CR "
    | `Warning_for_collector_testing -> ""
  ;;

  let message_when_expectation_contains_backtrace t =
    Printf.sprintf
      {|
(* %sexpect_test_collector: This test expectation appears to contain a backtrace.
   This is strongly discouraged as backtraces are fragile.
   Please change this test to not include a backtrace. *)

|}
      (comment_prefix t)
  ;;
end
