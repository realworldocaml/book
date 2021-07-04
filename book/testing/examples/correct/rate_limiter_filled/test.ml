open! Core

let start_time = Time_ns.of_string "2021-06-01 7:00:00"

let consume lim offset =
  let result =
    Rate_limiter.maybe_consume lim
      ~now:(Time_ns.add start_time (Time_ns.Span.of_sec offset))
  in
  printf "%4.2f: %s\n" offset
    (match result with `Consumed -> "C" | `No_capacity -> "N")

let limiter () =
  Rate_limiter.create
    ~now:start_time
    ~period:(Time_ns.Span.of_sec 1.)
    ~rate:(100)

let%expect_test _ =
  let lim = limiter () in
  let consume offset = consume lim offset in
  for _ = 0 to 11 do
    consume 0.;
  done;
  [%expect{|
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C
    0.00: C |}];
  consume 0.009;
  [%expect{| 1.0: C |}];
  consume 0.01;
  [%expect{| 0.01: C |}]
