open Expect_test_disabling_test_lib

let () = [%test_result: Test_ref.t] (Test_ref.value ()) ~expect:Init
