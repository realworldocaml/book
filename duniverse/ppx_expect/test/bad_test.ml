module A = struct
  module Expect_test_config = struct
    include Expect_test_config
    let upon_unreleasable_issue = `Warning_for_collector_testing
  end

  let get_a_trace () =
    let rec loop n =
      if n < 0 then (Printexc.get_callstack 10, 0)
      else begin
        let (x, y) = loop (n - 1) in
        (x, y + 1)
      end
    in
    let trace, _ = loop 10 in
    trace

  let print_slot trace n =
    match Printexc.backtrace_slots trace with
    | None -> assert false
    | Some slots ->
      let slot = slots.(n) in
      match Printexc.Slot.format 0 slot with
      | None -> assert false
      | Some str -> print_endline str

  let%expect_test _ [@tags "no-js"] =
    (* We create a backtrace with 10 identical slots and then only print the 5th slot.
       Otherwise flambda and non-flambda compilers create slightly different
       backtraces. *)
    let trace = get_a_trace () in
    print_slot trace 5;
    [%expect {|
      (* expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)

      Raised by primitive operation at file "bad_test.ml", line 11, characters 21-33
    |}]
end

