(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result
open Testing
open Testing_ptime

let pp_result ppf r =
  let pp_ok ppf (t, tz, count) =
    Format.fprintf ppf "(%a, %a, %d)" stamp t (pp_opt ~pp_v:pp_int) tz count
  in
  let pp_error ppf = function `RFC3339 ((s, e), err) ->
    Format.fprintf ppf "@[<1>%d-%d:@ @[%a@]@]" s e Ptime.pp_rfc3339_error err
  in
  match r with
  | Ok v -> Format.fprintf ppf "@[Ok %a@]" pp_ok v
  | Error e -> Format.fprintf ppf "@[Error %a@]" pp_error e

let eq_result =
  let ok (t, tz, c) (t', tz',c') = Ptime.equal t t' && tz = tz' && c = c' in
  let r_equal r r' = match r, r' with
  | Ok v, Ok v' -> ok v v'
  | Error e, Error e' -> e = e'
  | _ -> false
  in
  eq ~eq:r_equal ~pp:pp_result

let stamp_of_rfc3339 =
  let p s = Ptime.of_rfc3339 s in
  (p $ pp_str @-> ret_get_result pp_result)

let stamp_of_date_time =
  Ptime.of_date_time $ raw_date_time @-> ret_get_option stamp

let stamp_of_s =
  (Ptime.of_float_s $ pp_float @-> ret_get_option stamp)

let stamp_conversions = test "Stamp to RFC 3339 conversions" @@ fun () ->
  let dt ?space ?frac_s ?tz_offset_s dt =
    Ptime.to_rfc3339 ?space ?frac_s ?tz_offset_s (stamp_of_date_time dt)
  in
  let stamp ?space ?frac_s ?tz_offset_s s =
    Ptime.to_rfc3339 ?space ?frac_s ?tz_offset_s s
  in
  let dt0 = (1999, 01, 02), ((01, 02, 03), 0) in
  eq_str "1999-01-02T01:02:03Z" (dt ~tz_offset_s:0 dt0);
  eq_str "1999-01-02 01:02:03Z" (dt ~tz_offset_s:0 ~space:true dt0);
  eq_str "1999-01-02T01:02:03-00:00" (dt dt0);
  eq_str "1999-01-02 01:02:03-00:00" (dt ~space:true dt0);
  eq_str "1999-01-02T02:03:03+01:01" (dt ~tz_offset_s:3660 dt0);
  eq_str "1999-01-02T00:01:03-01:01" (dt ~tz_offset_s:(-3660) dt0);
  eq_str "1999-01-02T01:02:03-00:00" (dt ~tz_offset_s:1 dt0);
  eq_str "1999-01-02T01:02:03-00:00" (dt ~tz_offset_s:12960000 dt0);
  eq_str "1969-12-31T23:59:59.75Z"
    (stamp ~frac_s:2 ~tz_offset_s:0 (stamp_of_s (-.(1. /. 4.))));
  eq_str "1969-12-31T23:59:59.25Z"
    (stamp ~frac_s:2 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. 4.))));
  eq_str "1970-01-01T00:00:01.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 9.)))));
  eq_str "1969-12-31T23:59:59.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 9.)))));
  eq_str "1970-01-01T00:00:01.125Z"
    (stamp ~frac_s:3 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 3.)))));
  eq_str "1969-12-31T23:59:59.125Z"
    (stamp ~frac_s:3 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 3.)))));
  eq_str "1970-01-01T00:00:01.5Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 1.)))));
  eq_str "1969-12-31T23:59:59.5Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 1.)))));
  eq_str "1970-01-01T00:00:02.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 2. +. (1. /. (2. ** 9.)))));
  eq_str "1969-12-31T23:59:58.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-2. +. (1. /. (2. ** 9.)))));
  eq_str "1970-01-01T00:00:02.000000000Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  eq_str "1969-12-31T23:59:58.000000000Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-2.)));
  eq_str "1970-01-01T00:00:02.0Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  eq_str "1969-12-31T23:59:58.0Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s (-2.)));
  eq_str "1970-01-01T00:00:02Z"
    (stamp ~frac_s:0 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  eq_str "1969-12-31T23:59:58Z"
    (stamp ~frac_s:0 ~tz_offset_s:0 (stamp_of_s (-2.)));
  eq_str "1969-12-31T23:59:58Z"
    (stamp ~frac_s:(-1) ~tz_offset_s:0 (stamp_of_s (-2.)));
  eq_str "9999-12-31T23:59:59.999999999999Z"
    (stamp ~frac_s:12 ~tz_offset_s:0 Ptime.max);
  eq_str "0000-01-01T00:00:00.000000000000Z"
    (stamp ~frac_s:12 ~tz_offset_s:0 Ptime.min);
  eq_str "0000-01-01T00:00:00.000000000000Z"
    (stamp ~frac_s:13 ~tz_offset_s:0 Ptime.min);
  ()

let parse = test "RFC 3339 to stamp conversions" @@ fun () ->
  let edigit = `Exp_chars ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
  let etz = `Exp_chars ['+'; '-'; 'Z'; 'z'] in
  let etz_strict = `Exp_chars ['+'; '-'; 'Z'] in
  let edtsep = `Exp_chars ['T';'t';' '] in
  let edtsep_strict = `Exp_chars ['T'] in
  let p ?strict ?sub ?start ?len s =
    Ptime.of_rfc3339 ?strict ?sub ?start s
  in
  let err (s,e) err = Error (`RFC3339 ((s, e), err)) in
  let err_pos pos e = err (pos, pos) e in
  let ok s ~tz ~count = Ok (stamp_of_s s, tz, count) in
  eq_result (p "1970-01-01T00:00:02.001953125Z")
    (ok ( 2. +. (1. /. (2. ** 9.))) ~tz:(Some 0) ~count:30);
  eq_result (p "1970-01-01T00:00:02.001953125-00:00")
    (ok ( 2. +. (1. /. (2. ** 9.))) ~tz:None ~count:35);
  eq_result (p "1969-12-31T23:59:58.001953125Z")
    (ok (-2. +. (1. /. (2. ** 9.))) ~tz:(Some 0) ~count:30);
  eq_result (p "1969-12-31T23:59:58.001953125-00:00")
    (ok (-2. +. (1. /. (2. ** 9.))) ~tz:None ~count:35);
  eq_result (p "1969-13-31T23:59:58.5Z") (err (0, 21) `Invalid_stamp);
  eq_result (p "1969-12-31T23:59:58.Z") (err_pos 20 edigit);
  eq_result (p "1969-12-31T23:59:58.5") (err_pos 20 `Eoi);
  eq_result (p "1969-12-31T23:59:58.5a") (err_pos 21 etz);
  eq_result (p "1969-12-31T23:59:58.5Za") (err_pos 22 `Trailing_input);
  eq_result (p "1969-12-31t23:59:58.5Z") (ok (-1.5) ~tz:(Some 0) ~count:22);
  eq_result (p "1969-12-31 23:59:58.5z") (ok (-1.5) ~tz:(Some 0) ~count:22);
  eq_result (p "1969-12-31T23:59:58.5Z") (ok (-1.5) ~tz:(Some 0) ~count:22);
  eq_result (p "1969-12-31a23:59:58.5Z") (err_pos 10 edtsep);
  eq_result (p ~strict:true "1969-12-31 23:59:58.5Z")
    (err_pos 10 edtsep_strict);
  eq_result (p ~strict:true "1969-12-31t23:59:58.5Z")
    (err_pos 10 edtsep_strict);
  eq_result (p ~strict:true "1969-12-31T23:59:58.5z")
    (err_pos 21 etz_strict);
  eq_result (p "1970-01-01T00:00:00.5+00:01")
    (ok (-59.5) ~tz:(Some 60) ~count:27);
  eq_result (p "1970-01-01T00:00:00.5+01:01")
    (ok (-3659.5) ~tz:(Some 3660) ~count:27);
  eq_result (p "1970-01-01T00:00:00.5-00:01")
    (ok (60.5) ~tz:(Some ~-60) ~count:27);
  eq_result (p "1970-01-01T00:00:00.00+01:01")
    (ok (-3660.00) ~tz:(Some 3660) ~count:28);
  eq_result (p "1970-01-01T00:00:00.25+01:01")
    (ok (-3659.75) ~tz:(Some 3660) ~count:28);
  eq_result (p "1970-01-01T00:00:00.25-00:01")
    (ok (60.25) ~tz:(Some ~-60) ~count:28);
  eq_result (p "1970-01-01T00:00:00-23:59")
    (ok (86340.) ~tz:(Some ~-86340) ~count:25);
  eq_result (p "1970-01-01T00:00:00-23:59")
    (ok (86340.) ~tz:(Some ~-86340) ~count:25);
  eq_result (p "1970-01-01T00:00:00+23:59")
    (ok (-86340.) ~tz:(Some 86340) ~count:25);
  eq_result (p "1970-01-01T00:00:00+23:59")
    (ok (-86340.) ~tz:(Some 86340) ~count:25);
  eq_result (p "1970-01-01T00:00:00.5-01:01")
    (ok (3660.5) ~tz:(Some ~-3660) ~count:27);
  eq_result (p "1970-01-01T00:00:00.5-24:01") (err (22, 23) `Invalid_stamp);
  eq_result (p "1970-01-01T00:00:00.5-01:60") (err (25, 26) `Invalid_stamp);
  eq_result (p ~sub:true ~start:1 "X1969-12-31T23:59:58.5ZX")
    (ok (-1.5) ~tz:(Some 0) ~count:22);
  eq_result (p ~start:1 "X1969-12-31T23:59:58.5ZX")
    (err_pos 23 `Trailing_input);
  eq_result (p "1969X12-31T23:59:58Z") (err_pos 4 (`Exp_chars ['-']));
  eq_result (p "1969-12X31T23:59:58Z") (err_pos 7 (`Exp_chars ['-']));
  eq_result (p "1969-12-31T23X59:58Z") (err_pos 13 (`Exp_chars [':']));
  eq_result (p "1969-12-31T23:59X58Z") (err_pos 16 (`Exp_chars [':']));
  eq_result (p "1969-12-31T23:59:58+00X00") (err_pos 22 (`Exp_chars [':']));
  eq_result (p ~start:(-1) "1970-01-01") (err_pos (-1) `Eoi);
  eq_result (p ~start:11 "1970-01-01") (err_pos 11 `Eoi);
  eq_result (p "") (err_pos 0 `Eoi);
  eq_result (p "0000-01-01T00:00:00+00:01") (err (0, 24) `Invalid_stamp);
  eq_result (p "9999-12-31T23:59:59-00:01") (err (0, 24) `Invalid_stamp);
  eq_result (p "1900-02-29T01:02:03Z") (err (0, 19) `Invalid_stamp);
  eq_result (p "01-02-29T01:02:03Z") (err_pos 2 edigit);
  ()

let stamp_trips = test "Random stamps to RFC 3339 round trips" @@ fun () ->
  let stamp_of_posix_s =
    Ptime.of_float_s $ pp_float @-> (ret_get_option stamp)
  in
  let trip ?tz_offset_s t =
    let back = stamp_of_posix_s (floor (Ptime.to_float_s t)) in
    let trip, tz, _ = stamp_of_rfc3339 (Ptime.to_rfc3339 ?tz_offset_s t) in
    eq_stamp back trip;
  in
  for i = 1 to Test_rand.loop_len () do
    trip ?tz_offset_s:(Some 0)(* UTC *) (Test_rand.float_stamp ());
    trip ?tz_offset_s:None (* Unknown *) (Test_rand.float_stamp ());
    trip ?tz_offset_s:(Some (Test_rand.tz_offset_s ()))
      (Test_rand.float_stamp ())
  done;
  ()

let suite = suite "Ptime RFC 3339 support"
    [ stamp_conversions;
      parse;
      stamp_trips ]

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
