(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let log f = Format.printf (f ^^ "@.")

let test_available () =
  try ignore (Mtime_clock.elapsed ()) with
  | Sys_error e -> log "[ERROR] no monotonic time available: %s" e; exit 1

let count = ref 0
let fail = ref 0
let test f v =
  incr count;
  try f v with
  | Failure _ | Assert_failure _ as exn ->
      let bt = Printexc.get_backtrace () in
      incr fail; log "[ERROR] %s@.%s" (Printexc.to_string exn) bt

let log_result () =
  if !fail = 0 then log "[OK] All %d tests passed !" !count else
  log "[FAIL] %d failure(s) out of %d" !fail !count;
  ()

let test_pp_span () =
  log "Testing Mtime.pp_span";
  (* The floating point stuff here comes from the previous incarnations
     of the formatter. Let's keep that it exercices a bit the of_float_ns. *)
  let pp s =
    let s = Option.get (Mtime.Span.of_float_ns (s *. 1e+9)) in
    Format.asprintf "%a" Mtime.Span.pp s
  in
  let eq_str s s' = if s <> s' then failwith (Printf.sprintf "%S <> %S" s s') in
  (* sub ns scale *)
  eq_str (pp 1.0e-10) "0ns";
  eq_str (pp 4.0e-10) "0ns";
  eq_str (pp 6.0e-10) "0ns";
  eq_str (pp 9.0e-10) "0ns";
  (* ns scale *)
  eq_str (pp 2.0e-9) "2ns";
  eq_str (pp 2.136767676e-9) "2ns";
  eq_str (pp 2.6e-9) "2ns";
  eq_str (pp 2.836767676e-9) "2ns";
  (* us scale *)
  eq_str (pp 2.0e-6) "2μs";
  eq_str (pp 2.555e-6) "2.56μs";
  eq_str (pp 2.5556e-6) "2.56μs";
  eq_str (pp 99.9994e-6) "100μs";
  eq_str (pp 99.9996e-6) "100μs";
  eq_str (pp 100.1555e-6) "101μs";
  eq_str (pp 100.5555e-6) "101μs";
  eq_str (pp 100.6555e-6) "101μs";
  eq_str (pp 999.4e-6) "1ms";
  eq_str (pp 999.6e-6) "1ms";
  (* ms scale *)
  eq_str (pp 1e-3) "1ms";
  eq_str (pp 1.555e-3) "1.56ms";
  eq_str (pp 1.5556e-3) "1.56ms";
  eq_str (pp 99.9994e-3) "100ms";
  eq_str (pp 99.9996e-3) "100ms";
  eq_str (pp 100.1555e-3) "101ms";
  eq_str (pp 100.5555e-3) "101ms";
  eq_str (pp 100.6555e-3) "101ms";
  eq_str (pp 999.4e-3) "1s";
  eq_str (pp 999.6e-3) "1s";
  (* s scale *)
  eq_str (pp 1.) "1s";
  eq_str (pp 1.555) "1.56s";
  eq_str (pp 1.5554) "1.56s";
  eq_str (pp 1.5556) "1.56s";
  eq_str (pp 59.) "59s";
  eq_str (pp 59.9994) "1min";
  eq_str (pp 59.9996) "1min";
  (* min scale *)
  eq_str (pp 60.) "1min";
  eq_str (pp 62.) "1min2s";
  eq_str (pp 62.4) "1min3s";
  eq_str (pp 3599.) "59min59s";
  (* hour scale *)
  eq_str (pp 3600.0) "1h";
  eq_str (pp 3629.0) "1h1min";
  eq_str (pp 3660.0) "1h1min";
  eq_str (pp 7164.0) "2h";
  eq_str (pp 7200.0) "2h";
  eq_str (pp 86399.) "1d";
  (* day scale *)
  eq_str (pp 86400.) "1d";
  eq_str (pp (86400. +. (23. *. 3600.))) "1d23h";
  eq_str (pp (86400. +. (24. *. 3600.))) "2d";
  (* These tests come from the b0 test suite *);
  let span s =
    Format.asprintf "%a"
      Mtime.Span.pp (Mtime.Span.of_uint64_ns (Int64.of_string s));
  in
  assert (span "0u0" = "0ns");
  assert (span "0u999" = "999ns");
  assert (span "0u1_000" = "1μs");
  assert (span "0u1_001" = "1.01μs");
  assert (span "0u1_009" = "1.01μs");
  assert (span "0u1_010" = "1.01μs");
  assert (span "0u1_011" = "1.02μs");
  assert (span "0u1_090" = "1.09μs");
  assert (span "0u1_091" = "1.1μs");
  assert (span "0u1_100" = "1.1μs");
  assert (span "0u1_101" = "1.11μs");
  assert (span "0u1_109" = "1.11μs");
  assert (span "0u1_110" = "1.11μs");
  assert (span "0u1_111" = "1.12μs");
  assert (span "0u1_990" = "1.99μs");
  assert (span "0u1_991" = "2μs");
  assert (span "0u1_999" = "2μs");
  assert (span "0u2_000" = "2μs");
  assert (span "0u2_001" = "2.01μs");
  assert (span "0u9_990" = "9.99μs");
  assert (span "0u9_991" = "10μs");
  assert (span "0u9_999" = "10μs");
  assert (span "0u10_000" = "10μs");
  assert (span "0u10_001" = "10.1μs");
  assert (span "0u10_099" = "10.1μs");
  assert (span "0u10_100" = "10.1μs");
  assert (span "0u10_101" = "10.2μs");
  assert (span "0u10_900" = "10.9μs");
  assert (span "0u10_901" = "11μs");
  assert (span "0u10_999" = "11μs");
  assert (span "0u11_000" = "11μs");
  assert (span "0u11_001" = "11.1μs");
  assert (span "0u11_099" = "11.1μs");
  assert (span "0u11_100" = "11.1μs");
  assert (span "0u11_101" = "11.2μs");
  assert (span "0u99_900" = "99.9μs");
  assert (span "0u99_901" = "100μs");
  assert (span "0u99_999" = "100μs");
  assert (span "0u100_000" = "100μs");
  assert (span "0u100_001" = "101μs");
  assert (span "0u100_999" = "101μs");
  assert (span "0u101_000" = "101μs");
  assert (span "0u101_001" = "102μs");
  assert (span "0u101_999" = "102μs");
  assert (span "0u102_000" = "102μs");
  assert (span "0u999_000" = "999μs");
  assert (span "0u999_001" = "1ms");
  assert (span "0u999_001" = "1ms");
  assert (span "0u999_999" = "1ms");
  assert (span "0u1_000_000" = "1ms");
  assert (span "0u1_000_001" = "1.01ms");
  assert (span "0u1_009_999" = "1.01ms");
  assert (span "0u1_010_000" = "1.01ms");
  assert (span "0u1_010_001" = "1.02ms");
  assert (span "0u9_990_000" = "9.99ms");
  assert (span "0u9_990_001" = "10ms");
  assert (span "0u9_999_999" = "10ms");
  assert (span "0u10_000_000" = "10ms");
  assert (span "0u10_000_001" = "10.1ms");
  assert (span "0u10_000_001" = "10.1ms");
  assert (span "0u10_099_999" = "10.1ms");
  assert (span "0u10_100_000" = "10.1ms");
  assert (span "0u10_100_001" = "10.2ms");
  assert (span "0u99_900_000" = "99.9ms");
  assert (span "0u99_900_001" = "100ms");
  assert (span "0u99_999_999" = "100ms");
  assert (span "0u100_000_000" = "100ms");
  assert (span "0u100_000_001" = "101ms");
  assert (span "0u100_999_999" = "101ms");
  assert (span "0u101_000_000" = "101ms");
  assert (span "0u101_000_001" = "102ms");
  assert (span "0u999_000_000" = "999ms");
  assert (span "0u999_000_001" = "1s");
  assert (span "0u999_999_999" = "1s");
  assert (span "0u1_000_000_000" = "1s");
  assert (span "0u1_000_000_001" = "1.01s");
  assert (span "0u1_009_999_999" = "1.01s");
  assert (span "0u1_010_000_000" = "1.01s");
  assert (span "0u1_010_000_001" = "1.02s");
  assert (span "0u1_990_000_000" = "1.99s");
  assert (span "0u1_990_000_001" = "2s");
  assert (span "0u1_999_999_999" = "2s");
  assert (span "0u2_000_000_000" = "2s");
  assert (span "0u2_000_000_001" = "2.01s");
  assert (span "0u9_990_000_000" = "9.99s");
  assert (span "0u9_999_999_999" = "10s");
  assert (span "0u10_000_000_000" = "10s");
  assert (span "0u10_000_000_001" = "10.1s");
  assert (span "0u10_099_999_999" = "10.1s");
  assert (span "0u10_100_000_000" = "10.1s");
  assert (span "0u10_100_000_001" = "10.2s");
  assert (span "0u59_900_000_000" = "59.9s");
  assert (span "0u59_900_000_001" = "1min");
  assert (span "0u59_999_999_999" = "1min");
  assert (span "0u60_000_000_000" = "1min");
  assert (span "0u60_000_000_001" = "1min1s");
  assert (span "0u60_999_999_999" = "1min1s");
  assert (span "0u61_000_000_000" = "1min1s");
  assert (span "0u61_000_000_001" = "1min2s");
  assert (span "0u119_000_000_000" = "1min59s");
  assert (span "0u119_000_000_001" = "2min");
  assert (span "0u119_999_999_999" = "2min");
  assert (span "0u120_000_000_000" = "2min");
  assert (span "0u120_000_000_001" = "2min1s");
  assert (span "0u3599_000_000_000" = "59min59s");
  assert (span "0u3599_000_000_001" = "1h");
  assert (span "0u3599_999_999_999" = "1h");
  assert (span "0u3600_000_000_000" = "1h");
  assert (span "0u3600_000_000_001" = "1h1min");
  assert (span "0u3659_000_000_000" = "1h1min");
  assert (span "0u3659_000_000_001" = "1h1min");
  assert (span "0u3659_999_999_999" = "1h1min");
  assert (span "0u3660_000_000_000" = "1h1min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3720_000_000_000" = "1h2min");
  assert (span "0u3720_000_000_001" = "1h3min");
  assert (span "0u7140_000_000_000" = "1h59min");
  assert (span "0u7140_000_000_001" = "2h");
  assert (span "0u7199_999_999_999" = "2h");
  assert (span "0u7200_000_000_000" = "2h");
  assert (span "0u7200_000_000_001" = "2h1min");
  assert (span "0u86340_000_000_000" = "23h59min");
  assert (span "0u86340_000_000_001" = "1d");
  assert (span "0u86400_000_000_000" = "1d");
  assert (span "0u86400_000_000_001" = "1d1h");
  assert (span "0u89999_999_999_999" = "1d1h");
  assert (span "0u90000_000_000_000" = "1d1h");
  assert (span "0u90000_000_000_001" = "1d2h");
  assert (span "0u169200_000_000_000" = "1d23h");
  assert (span "0u169200_000_000_001" = "2d");
  assert (span "0u169200_000_000_001" = "2d");
  assert (span "0u172799_999_999_999" = "2d");
  assert (span "0u172800_000_000_000" = "2d");
  assert (span "0u172800_000_000_001" = "2d1h");
  assert (span "0u31536000_000_000_000" = "365d");
  assert (span "0u31554000_000_000_000" = "365d5h");
  assert (
    (* Technically this should round to a year but it does get rendered.
       I don't think it matters, it's not inacurate per se. *)
    span "0u31554000_000_000_001" = "365d6h");
  assert (span "0u31557600_000_000_000" = "1a");
  assert (span "0u31557600_000_000_001" = "1a1d");
  assert (span "0u63028800_000_000_000" = "1a365d");
  assert (span "0u63093600_000_000_000" = "1a365d");
  assert (span "0u63093600_000_000_001" = "2a");
  assert (span "0u63115200_000_000_000" = "2a");
  assert (span "0u63115200_000_000_001" = "2a1d");
  ()

let test_counters () =
  log "Test counters";
  let count max =
    let c = Mtime_clock.counter () in
    for i = 1 to max do () done;
    Mtime_clock.count c
  in
  let do_count max =
    let span = count max in
    let span_ns = Mtime.Span.to_uint64_ns span in
    let span_s = 0. (* Mtime.Span.to_s span *) in
    log " * Count to % 8d: % 10Luns %.10fs %a"
      max span_ns span_s Mtime.Span.pp span
  in
  do_count 1000000;
  do_count 100000;
  do_count 10000;
  do_count 1000;
  do_count 100;
  do_count 10;
  do_count 1;
  ()

let test_elapsed () =
  log "Test Mtime_clock.elapsed ns - s - pp - dump";
  let span = Mtime_clock.elapsed () in
  log " * Elapsed: %Luns - %gs - %a - %a"
    (Mtime.Span.to_uint64_ns span) (Mtime.Span.to_float_ns span *. 1e-9)
    Mtime.Span.pp span Mtime.Span.dump span;
  ()

let test_now () =
  log "Test Mtime_clock.now ns - s - pp - dump ";
  let t = Mtime_clock.now () in
  let span = Mtime.(span t (of_uint64_ns 0_L)) in
  log " * System: %Luns - %gs - %a - %a"
    (Mtime.to_uint64_ns t) (Mtime.Span.to_float_ns span *. 1e-9)
    Mtime.pp t Mtime.dump t;
  ()

let test_span_compare () =
  log "Test Mtime.Span.compare";
  let zero_mtime = Mtime.Span.of_uint64_ns 0_L in
  let large_mtime = Mtime.Span.of_uint64_ns Int64.max_int in
  let larger_mtime = Mtime.Span.of_uint64_ns Int64.min_int in
  let max_mtime = Mtime.Span.of_uint64_ns (-1_L) in
  let (<) x y = Mtime.Span.compare x y < 0 in
  assert (zero_mtime < large_mtime);
  assert (zero_mtime < larger_mtime);
  assert (zero_mtime < max_mtime);
  assert (large_mtime < larger_mtime);
  assert (large_mtime < max_mtime);
  assert (larger_mtime < max_mtime);
  let (<) x y = Mtime.Span.compare y x > 0 in
  assert (zero_mtime < large_mtime);
  assert (zero_mtime < large_mtime);
  assert (zero_mtime < larger_mtime);
  assert (zero_mtime < max_mtime);
  assert (large_mtime < larger_mtime);
  assert (large_mtime < max_mtime);
  assert (larger_mtime < max_mtime);
  ()

let test_span_constants () =
  log "Test Mtime.Span.{zero,one,max_span,min_span}";
  let (<) x y = Mtime.Span.compare x y < 0 in
  assert (Mtime.Span.zero < Mtime.Span.one);
  assert (Mtime.Span.zero < Mtime.Span.max_span);
  assert (Mtime.Span.min_span < Mtime.Span.one);
  assert (Mtime.Span.min_span < Mtime.Span.max_span);
  assert (Mtime.Span.one < Mtime.Span.max_span);
  ()

let test_span_arith () =
  log "Test Mtime.Span.{abs_diff,add}";
  assert (Mtime.Span.(equal (add zero one) one));
  assert (Mtime.Span.(equal (add one zero) one));
  assert (Mtime.Span.(equal (add (abs_diff max_span one) one) max_span));
  ()

let test_float_ns () =
  log "Test Mtime.{to,of}_float_ns";
  assert (Mtime.Span.to_float_ns Mtime.Span.max_span = (2. ** 64.) -. 1.);
  assert (Mtime.Span.to_float_ns Mtime.Span.min_span = 0.);
  assert (Mtime.Span.of_float_ns (2. ** 53. -. 1.) =
          Some (Mtime.Span.of_uint64_ns (Int64.(sub (shift_left 1L 53) one))));
  assert (Mtime.Span.of_float_ns (2. ** 53.) = None);
  assert (Mtime.Span.of_float_ns 0. = Some Mtime.Span.zero);
  assert (Mtime.Span.of_float_ns (-.0.) = Some Mtime.Span.zero);
  assert (Mtime.Span.of_float_ns infinity = None);
  assert (Mtime.Span.of_float_ns nan = None);
  assert (Mtime.Span.of_float_ns (-3.) = None);
  assert (Mtime.Span.of_float_ns 1. = Some Mtime.Span.one);
  ()

let run () =
  test test_available ();
  test test_pp_span ();
  test test_counters ();
  test test_elapsed ();
  test test_now ();
  test test_span_compare ();
  test test_span_constants ();
  test_span_arith ();
  test_float_ns ();
  log_result ();
  exit !fail

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
