(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let p s = match Ptime.Span.of_d_ps s with
| None -> invalid_arg ""
| Some s -> s

let n s = Ptime.Span.(neg (p s))
let pps ps = p (0, ps)
let nps ps = n (0, ps)

let conversions = test "Constants and conversions" @@ fun () ->
  (* int *)
  let trip_int secs =
    eq_option ~eq:(=) ~pp:pp_int
      Ptime.Span.(to_int_s (of_int_s secs)) (Some secs)
  in
  eq_span (Ptime.Span.of_int_s 0) Ptime.Span.zero;
  eq_span (Ptime.Span.of_int_s 1) (pps 1_000_000_000_000L);
  eq_span (Ptime.Span.of_int_s (-1)) (nps 1_000_000_000_000L);
  eq_span (Ptime.Span.of_int_s 86_400) (p (1, 0L));
  eq_span (Ptime.Span.of_int_s (-86_400)) (n (1, 0L));
  trip_int (86_400);
  trip_int (-86_400);
  trip_int (234_322_342);
  trip_int (-234_322_352);
  trip_int (1);
  trip_int (-1);
  trip_int (0);
  (* float *)
  let trip_float secs =
    let of_float = Ptime.Span.of_float_s $ float @-> ret_get_option span in
    eq_float (Ptime.Span.to_float_s (of_float secs)) secs
  in
  eq_span_opt
    (Ptime.Span.of_float_s 1.0000000000005) (Some (pps 1_000_000_000_000L));
  eq_span_opt
    (Ptime.Span.of_float_s (-1.0000000000005)) (Some (nps 1_000_000_000_000L));
  eq_span_opt (Ptime.Span.of_float_s 0.) (Some Ptime.Span.zero);
  eq_span_opt (Ptime.Span.of_float_s (min_float)) (Some Ptime.Span.zero);
  eq_span_opt (Ptime.Span.of_float_s (-.min_float))(Some Ptime.Span.zero);
  eq_span_opt (Ptime.Span.of_float_s max_float) None;
  eq_span_opt (Ptime.Span.of_float_s (-.max_float)) None;
  eq_span_opt (Ptime.Span.of_float_s nan) None;
  eq_span_opt (Ptime.Span.of_float_s infinity) None;
  eq_span_opt (Ptime.Span.of_float_s (-.infinity)) None;
  trip_float 0.;
  trip_float (-0.);
  trip_float 1.;
  trip_float (-1.);
  trip_float (Pervasives.float (1 lsl 30 - 1));
  trip_float (Pervasives.float (- (1 lsl 30)));
  eq_span_opt (Ptime.Span.of_d_ps (23, -1L)) None;
  eq_span_opt (Ptime.Span.of_d_ps (23, 86_400_000_000_000_000L)) None;
  ()

let predicates = test "Predicates" @@ fun () ->
  eq_bool (Ptime.Span.equal Ptime.Span.zero Ptime.Span.zero) true;
  eq_bool (Ptime.Span.equal Ptime.Span.zero (pps 1L)) false;
  eq_bool (Ptime.Span.equal Ptime.Span.zero (nps 1L)) false;
  eq_bool (Ptime.Span.equal (p (30, 3434L)) (p (30, 3434L))) true;
  eq_bool (Ptime.Span.equal (p (30, 3434L)) (p (30, 3435L))) false;
  eq_bool (Ptime.Span.equal (n (30, 3434L)) (n (30, 3434L))) true;
  eq_bool (Ptime.Span.equal (n (30, 3434L)) (n (30, 3435L))) false;
  eq_bool (Ptime.Span.equal (n (30, 3434L)) (p (30, 3434L))) false;
  eq_int (Ptime.Span.compare Ptime.Span.zero Ptime.Span.zero) 0;
  eq_int (Ptime.Span.compare Ptime.Span.zero (pps 1L)) (-1);
  eq_int (Ptime.Span.compare Ptime.Span.zero (nps 1L)) 1;
  eq_int (Ptime.Span.compare (n (30, 3434L)) (n (30, 3434L))) 0;
  eq_int (Ptime.Span.compare (n (30, 3434L)) (n (30, 3435L))) 1;
  eq_int (Ptime.Span.compare (n (30, 3434L)) (p (30, 3435L))) (-1);
  eq_int (Ptime.Span.compare (n (30, 3434L)) (n (30, 3433L))) (-1);
  eq_int (Ptime.Span.compare (n (30, 3434L)) (p (30, 3433L))) (-1);
  ()

let arithmetic = test "Arithmetic" @@ fun () ->
  eq_span
    (Ptime.Span.add (pps 86_399_999_999_999_999L) (pps 1L))
    (p (1, 0L));
  eq_span
    (Ptime.Span.add (nps 86_399_999_999_999_999L) (nps 1L))
    (n (1, 0L));
  eq_span (Ptime.Span.sub Ptime.Span.zero (pps 1L)) (nps 1L);
  eq_span (Ptime.Span.sub Ptime.Span.zero (nps 1L)) (pps 1L);
  eq_span (Ptime.Span.add (nps 1L) (pps 1L)) Ptime.Span.zero;
  eq_span (Ptime.Span.abs (n (3, 342L))) (p (3, 342L));
  eq_span (Ptime.Span.abs (p (3, 342L))) (p (3, 342L));
  ()

let rounding = test "Rounding" @@ fun () ->
  let r ~frac a b =
    eq_span (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (3, b));
    eq_span (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (3, b))
  in
  let r_carry ~frac a =
    eq_span (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (4, 0L));
    eq_span (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (4, 0L))
  in
  let t ~frac a b =
    eq_span (Ptime.Span.truncate ~frac_s:frac (p (3, a))) (p (3, b));
    eq_span (Ptime.Span.truncate ~frac_s:frac (n (3, a))) (n (3, b))
  in
  for i = 0 to 12 do r ~frac:0 0L 0L done;
  r_carry ~frac:(-1) 86_399_500_000_000_000L;
  r_carry ~frac:0 86_399_500_000_000_000L;
  r       ~frac:0 86_399_499_999_999_999L 86_399_000_000_000_000L;
  r       ~frac:0 10_001_500_000_000_000L 10_002_000_000_000_000L;
  r       ~frac:0 10_001_499_999_999_999L 10_001_000_000_000_000L;
  r_carry ~frac:1 86_399_950_000_000_000L;
  r       ~frac:1 86_399_949_999_999_999L 86_399_900_000_000_000L;
  r       ~frac:1 10_001_150_000_000_000L 10_001_200_000_000_000L;
  r       ~frac:1 10_001_149_999_999_999L 10_001_100_000_000_000L;
  r_carry ~frac:2 86_399_995_000_000_000L;
  r       ~frac:2 86_399_994_999_999_999L 86_399_990_000_000_000L;
  r       ~frac:2 10_001_115_000_000_000L 10_001_120_000_000_000L;
  r       ~frac:2 10_001_114_999_999_999L 10_001_110_000_000_000L;
  r_carry ~frac:3 86_399_999_500_000_000L;
  r       ~frac:3 86_399_999_499_999_999L 86_399_999_000_000_000L;
  r       ~frac:3 10_001_111_500_000_000L 10_001_112_000_000_000L;
  r       ~frac:3 10_001_111_499_999_999L 10_001_111_000_000_000L;
  r_carry ~frac:4 86_399_999_950_000_000L;
  r       ~frac:4 86_399_999_949_999_999L 86_399_999_900_000_000L;
  r       ~frac:4 10_001_111_150_000_000L 10_001_111_200_000_000L;
  r       ~frac:4 10_001_111_149_999_999L 10_001_111_100_000_000L;
  r_carry ~frac:5 86_399_999_995_000_000L;
  r       ~frac:5 86_399_999_994_999_999L 86_399_999_990_000_000L;
  r       ~frac:5 10_001_111_115_000_000L 10_001_111_120_000_000L;
  r       ~frac:5 10_001_111_114_999_999L 10_001_111_110_000_000L;
  r_carry ~frac:6 86_399_999_999_500_000L;
  r       ~frac:6 86_399_999_999_499_999L 86_399_999_999_000_000L;
  r       ~frac:6 10_001_111_111_500_000L 10_001_111_112_000_000L;
  r       ~frac:6 10_001_111_111_499_999L 10_001_111_111_000_000L;
  r_carry ~frac:7 86_399_999_999_950_000L;
  r       ~frac:7 86_399_999_999_949_999L 86_399_999_999_900_000L;
  r       ~frac:7 10_001_111_111_150_000L 10_001_111_111_200_000L;
  r       ~frac:7 10_001_111_111_149_999L 10_001_111_111_100_000L;
  r_carry ~frac:8 86_399_999_999_995_000L;
  r       ~frac:8 86_399_999_999_994_999L 86_399_999_999_990_000L;
  r       ~frac:8 10_001_111_111_115_000L 10_001_111_111_120_000L;
  r       ~frac:8 10_001_111_111_114_999L 10_001_111_111_110_000L;
  r_carry ~frac:9 86_399_999_999_999_500L;
  r       ~frac:9 86_399_999_999_999_499L 86_399_999_999_999_000L;
  r       ~frac:9 10_001_111_111_111_500L 10_001_111_111_112_000L;
  r       ~frac:9 10_001_111_111_111_499L 10_001_111_111_111_000L;
  r_carry ~frac:10 86_399_999_999_999_950L;
  r       ~frac:10 86_399_999_999_999_949L 86_399_999_999_999_900L;
  r       ~frac:10 10_001_111_111_111_150L 10_001_111_111_111_200L;
  r       ~frac:10 10_001_111_111_111_149L 10_001_111_111_111_100L;
  r_carry ~frac:11 86_399_999_999_999_995L;
  r       ~frac:11 86_399_999_999_999_994L 86_399_999_999_999_990L;
  r       ~frac:11 10_001_111_111_111_115L 10_001_111_111_111_120L;
  r       ~frac:11 10_001_111_111_111_114L 10_001_111_111_111_110L;
  r       ~frac:12 86_399_999_999_999_999L 86_399_999_999_999_999L;
  r       ~frac:12 10_001_111_111_111_115L 10_001_111_111_111_115L;
  r       ~frac:12 10_001_111_111_111_114L 10_001_111_111_111_114L;
  r       ~frac:13 10_001_111_111_111_114L 10_001_111_111_111_114L;
  for i = 0 to 12 do t ~frac:0 0L 0L done;
  t ~frac:(-1) 86_399_999_999_999_999L 86_399_000_000_000_000L;
  t ~frac:0 86_399_999_999_999_999L 86_399_000_000_000_000L;
  t ~frac:1 86_399_999_999_999_999L 86_399_900_000_000_000L;
  t ~frac:2 86_399_999_999_999_999L 86_399_990_000_000_000L;
  t ~frac:3 86_399_999_999_999_999L 86_399_999_000_000_000L;
  t ~frac:4 86_399_999_999_999_999L 86_399_999_900_000_000L;
  t ~frac:5 86_399_999_999_999_999L 86_399_999_990_000_000L;
  t ~frac:6 86_399_999_999_999_999L 86_399_999_999_000_000L;
  t ~frac:7 86_399_999_999_999_999L 86_399_999_999_900_000L;
  t ~frac:8 86_399_999_999_999_999L 86_399_999_999_990_000L;
  t ~frac:9 86_399_999_999_999_999L 86_399_999_999_999_000L;
  t ~frac:10 86_399_999_999_999_999L 86_399_999_999_999_900L;
  t ~frac:11 86_399_999_999_999_999L 86_399_999_999_999_990L;
  t ~frac:12 86_399_999_999_999_999L 86_399_999_999_999_999L;
  t ~frac:13 86_399_999_999_999_999L 86_399_999_999_999_999L;
  ()

let pretty_printing =
  test "Pretty printing" @@ fun () ->
  let fmt s = Format.asprintf "%a" Ptime.Span.pp s in
  let n s = fmt @@ Ptime.Span.(neg (p s)) in
  let p s = fmt @@ p s in
  let pps ps = p (0, ps) in
  let nps ps = n (0, ps) in
  (* y d *)
  eq_str (p (366, 0L)) "1y1d";
  eq_str (n (366, 0L)) "-1y1d";
  eq_str (p (1461, 0L)) "4y";
  eq_str (n (1461, 0L)) "-4y";
  eq_str (p (1461, 43_200_000_000_000_000L)) "4y1d";
  eq_str (n (1461, 43_200_000_000_000_000L)) "-4y1d";
  eq_str (p (1461, 43_199_199_199_199_199L)) "4y";
  eq_str (n (1461, 43_199_199_199_199_199L)) "-4y";
  eq_str (p (1462, 43_200_000_000_000_000L)) "4y2d";
  eq_str (n (1462, 43_200_000_000_000_000L)) "-4y2d";
  eq_str (p (1462, 43_199_199_199_199_199L)) "4y1d";
  eq_str (n (1462, 43_199_199_199_199_199L)) "-4y1d";
  (* d h *)
  eq_str (p (365, 84_600_000_000_000_000L)) "1y1d";
  eq_str (n (365, 84_600_000_000_000_000L)) "-1y1d";
  eq_str (p (365, 84_599_999_999_999_999L)) "1y";
  eq_str (n (365, 84_599_999_999_999_999L)) "-1y";
  eq_str (p (365, 19_800_000_000_000_000L)) "1y";
  eq_str (n (365, 19_800_000_000_000_000L)) "-1y";
  eq_str (p (365, 19_799_999_999_999_999L)) "365d5h";
  eq_str (n (365, 19_799_999_999_999_999L)) "-365d5h";
  eq_str (p (1, 84_600_000_000_000_000L)) "2d";
  eq_str (n (1, 84_600_000_000_000_000L)) "-2d";
  eq_str (p (1, 84_599_999_999_999_999L)) "1d23h";
  eq_str (n (1, 84_599_999_999_999_999L)) "-1d23h";
  eq_str (p (2, 0L)) "2d";
  eq_str (n (2, 0L)) "-2d";
  (* h m *)
  eq_str (pps 86_370_000_000_000_000L) "1d";
  eq_str (nps 86_370_000_000_000_000L) "-1d";
  eq_str (pps 86_369_999_999_999_999L) "23h59min";
  eq_str (nps 86_369_999_999_999_999L) "-23h59min";
  eq_str (pps 3660_000_000_000_000L) "1h1min";
  eq_str (nps 3660_000_000_000_000L) "-1h1min";
  eq_str (pps 3630_000_000_000_000L) "1h1min";
  eq_str (pps 3629_999_999_999_999L) "1h";
  eq_str (nps 3629_999_999_999_999L) "-1h";
  eq_str (pps 3600_000_000_000_000L) "1h";
  eq_str (nps 3600_000_000_000_000L) "-1h";
  (* m s *)
  eq_str (pps 3599_500_000_000_000L) "1h";
  eq_str (nps 3599_500_000_000_000L) "-1h";
  eq_str (pps 3599_499_999_999_999L) "59min59s";
  eq_str (nps 3599_499_999_999_999L) "-59min59s";
  eq_str (pps 60_500_000_000_000L) "1min1s";
  eq_str (nps 60_500_000_000_000L) "-1min1s";
  eq_str (pps 60_499_000_000_000L) "1min";
  eq_str (nps 60_499_000_000_000L) "-1min";
  eq_str (pps 60_000_000_000_000L) "1min";
  eq_str (nps 60_000_000_000_000L) "-1min";
  (* s *)
  eq_str (pps 59_999_500_000_000L) "1min";
  eq_str (nps 59_999_500_000_000L) "-1min";
  eq_str (pps 59_999_499_999_999L) "59.999s";
  eq_str (nps 59_999_499_999_999L) "-59.999s";
  eq_str (pps 1_999_500_000_000L) "2s";
  eq_str (nps 1_999_500_000_000L) "-2s";
  eq_str (pps 1_999_499_999_999L) "1.999s";
  eq_str (nps 1_999_499_999_999L) "-1.999s";
  eq_str (pps 1_534_500_000_000L) "1.535s";
  eq_str (nps 1_534_500_000_000L) "-1.535s";
  eq_str (pps 1_534_499_999_999L) "1.534s";
  eq_str (nps 1_534_499_999_999L) "-1.534s";
  eq_str (pps 1_000_000_000_000L) "1s";
  eq_str (nps 1_000_000_000_000L) "-1s";
  (* ms *)
  eq_str (pps 999_500_000_000L) "1s";
  eq_str (nps 999_500_000_000L) "-1s";
  eq_str (pps 999_499_999_999L) "999ms";
  eq_str (nps 999_499_999_999L) "-999ms";
  eq_str (pps 1_999_500_000L) "2ms";
  eq_str (nps 1_999_500_000L) "-2ms";
  eq_str (pps 1_999_499_999L) "1.999ms";
  eq_str (nps 1_999_499_999L) "-1.999ms";
  eq_str (pps 1_332_500_000L) "1.333ms";
  eq_str (nps 1_332_500_000L) "-1.333ms";
  eq_str (pps 1_332_499_999L) "1.332ms";
  eq_str (nps 1_332_499_999L) "-1.332ms";
  eq_str (pps 1_000_000_000L) "1ms";
  eq_str (nps 1_000_000_000L) "-1ms";
  (* us *)
  eq_str (pps 999_500_000L) "1ms";
  eq_str (nps 999_500_000L) "-1ms";
  eq_str (pps 999_499_999L) "999us";
  eq_str (nps 999_499_999L) "-999us";
  eq_str (pps 1_999_500L) "2us";
  eq_str (nps 1_999_500L) "-2us";
  eq_str (pps 1_999_499L) "1.999us";
  eq_str (nps 1_999_499L) "-1.999us";
  eq_str (pps 1_332_500L) "1.333us";
  eq_str (nps 1_332_500L) "-1.333us";
  eq_str (pps 1_332_499L) "1.332us";
  eq_str (nps 1_332_499L) "-1.332us";
  eq_str (pps 1_000_000L) "1us";
  eq_str (nps 1_000_000L) "-1us";
  (* ns *)
  eq_str (pps 999_500L) "1us";
  eq_str (nps 999_500L) "-1us";
  eq_str (pps 999_499L) "999ns";
  eq_str (nps 999_499L) "-999ns";
  eq_str (pps 1_995L) "1.995ns";
  eq_str (nps 1_995L) "-1.995ns";
  eq_str (pps 1_994L) "1.994ns";
  eq_str (nps 1_994L) "-1.994ns";
  eq_str (pps 1_332L) "1.332ns";
  eq_str (nps 1_332L) "-1.332ns";
  eq_str (pps 1_000L) "1ns";
  eq_str (nps 1_000L) "-1ns";
  (* ps *)
  eq_str (pps 999L) "999ps";
  eq_str (nps 999L) "-999ps";
  eq_str (pps 50L) "50ps";
  eq_str (nps 50L) "-50ps";
  eq_str (pps 1L) "1ps";
  eq_str (nps 1L) "-1ps";
  eq_str (pps 0L) "0ps";
  eq_str (nps 0L) "0ps";
  ()

let suite = suite "Ptime span tests"
    [ conversions;
      predicates;
      arithmetic;
      rounding;
      pretty_printing; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers

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
