(** Regression test runner. *)

open Core
open Qtest_deprecated.Std

let tests = [] @ Unpack_sequence_test.tests
let () = Runner.main tests
