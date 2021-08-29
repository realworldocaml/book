(** Regression test runner. *)

open Core
open Qtest_lib.Std

let tests = [] @ Unpack_sequence_test.tests
let () = Runner.main tests
