(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



open OUnit2
open Test_helpers

let count = 1000000
let command = Printf.sprintf "./a.out %i" count

(* This test runs the program in fixtures/thread-safety. That program starts two
   threads, each running code instrumented by Bisect_ppx. Since Bisect_ppx does
   not use a mutex to protect visitation counts, we expect the counts to be
   wrong in the presence of either true parallelism or fully preemptive
   threading.

   OCaml does not have either at the moment, so this test is a kind of
   future-proofing for now: if parallelism or full preemption become the default
   in OCaml, this test will fail, letting the maintainers know. *)
let test ?(bisect = "") name expect_correctness =
  test
    (if expect_correctness then name else name ^ ".should-have-diff")
    begin fun () ->

    skip_if (not @@ expect_correctness) "No pre-emptive threads";

    let cflags =
      "-thread -package threads.posix " ^ (with_bisect_args bisect) in

    compile cflags "fixtures/thread-safety/source.ml";
    run command;
    report "-dump -" ~r:" > output";

    if expect_correctness then
      diff "fixtures/thread-safety/reference"
    else
      let redirections = "> /dev/null 2> /dev/null" in
      run ("! diff ../fixtures/thread-safety/reference output" ^ redirections)
  end

let tests = "thread-safety" >::: [
  test "bisect"         true;
  test "bisect-threads" false;
]
