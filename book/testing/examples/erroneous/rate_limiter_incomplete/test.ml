[@@@part "1"];;
open! Core

let start_time = Time_ns.of_string "2021-06-01 7:00:00"

let limiter () =
  Rate_limiter.create ~now:start_time ~period:(Time_ns.Span.of_sec 1.) ~rate:5

let consume lim offset =
  let result =
    Rate_limiter.maybe_consume
      lim
      ~now:(Time_ns.add start_time (Time_ns.Span.of_sec offset))
  in
  printf
    "%4.2f: %s\n"
    offset
    (match result with
    | `Consumed -> "C"
    | `No_capacity -> "N")


[@@@part "2"];;

let%expect_test _ =
  let lim = limiter () in
  let consume offset = consume lim offset in
  (* Consume 10 times in a row, without advancing the clock.  The
     first five should succeed. *)
  for _ = 1 to 10 do
    consume 0.
  done;
  [%expect {| |}];
  (* Wait until a half-second has elapsed, try again *)
  consume 0.5;
  [%expect {| |}];
  (* Wait for a full second, try again *)
  consume 1.;
  [%expect {|  |}]
