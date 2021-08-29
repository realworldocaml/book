(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let stamp_of_date_time d =
  (Ptime.of_date_time $ raw_date_time @-> ret_get_option stamp) d

let valid_date d =
  ignore ((Ptime.of_date $ raw_date @-> ret_some stamp) d)

let wrong_date d =
  ignore ((Ptime.of_date $ raw_date @-> ret_none stamp) d)

let bounds = test "Testing calendar date field bounds" @@ fun () ->
  (* Check year bounds *)
  wrong_date (-1, 01, 01);
  valid_date (0, 01, 01);
  valid_date (1, 01, 01);
  valid_date (9999, 01, 01);
  wrong_date (10000, 01, 01);
  wrong_date (10001, 01, 01);
  (* Check month bounds *)
  wrong_date (0, 00, 01);
  valid_date (0, 01, 01);
  valid_date (0, 12, 01);
  wrong_date (0, 13, 01);
  (* Check day bounds in 2015 (month lengths) *)
  (* Jan 2015 *)
  wrong_date (2015, 01, -1);
  valid_date (2015, 01, 01);
  valid_date (2015, 01, 31);
  wrong_date (2015, 01, 32);
  (* Feb 2015, is not leap *)
  wrong_date (2015, 02, -1);
  valid_date (2015, 02, 01);
  valid_date (2015, 02, 28);
  wrong_date (2015, 02, 29);
  (* Mar 2015 *)
  wrong_date (2015, 03, -1);
  valid_date (2015, 03, 01);
  valid_date (2015, 03, 31);
  wrong_date (2015, 03, 32);
  (* Apr 2015 *)
  wrong_date (2015, 04, -1);
  valid_date (2015, 04, 01);
  valid_date (2015, 04, 30);
  wrong_date (2015, 04, 31);
  (* May 2015 *)
  wrong_date (2015, 05, -1);
  valid_date (2015, 05, 01);
  valid_date (2015, 05, 31);
  wrong_date (2015, 05, 32);
  (* June 2015 *)
  wrong_date (2015, 06, -1);
  valid_date (2015, 06, 01);
  valid_date (2015, 06, 30);
  wrong_date (2015, 06, 31);
  (* July 2015 *)
  wrong_date (2015, 07, -1);
  valid_date (2015, 07, 01);
  valid_date (2015, 07, 31);
  wrong_date (2015, 07, 32);
  (* Aug 2015 *)
  wrong_date (2015, 08, -1);
  valid_date (2015, 08, 01);
  valid_date (2015, 08, 31);
  wrong_date (2015, 08, 32);
  (* Sept 2015 *)
  wrong_date (2015, 09, -1);
  valid_date (2015, 09, 01);
  valid_date (2015, 09, 30);
  wrong_date (2015, 09, 31);
  (* Oct 2015 *)
  wrong_date (2015, 10, -1);
  valid_date (2015, 10, 01);
  valid_date (2015, 10, 31);
  wrong_date (2015, 10, 32);
  (* Nov 2015 *)
  wrong_date (2015, 11, -1);
  valid_date (2015, 11, 01);
  valid_date (2015, 11, 30);
  wrong_date (2015, 11, 31);
  (* Dec 2015 *)
  wrong_date (2015, 12, -1);
  valid_date (2015, 12, 01);
  valid_date (2015, 12, 31);
  wrong_date (2015, 12, 32);
  (* 1500 is not leap *)
  valid_date (1500, 02, 28);
  wrong_date (1500, 02, 29);
  (* 1700 is not leap *)
  valid_date (1700, 02, 28);
  wrong_date (1700, 02, 29);
  (* 1800 is not leap *)
  valid_date (1800, 02, 28);
  wrong_date (1800, 02, 29);
  (* 1900 is not leap, Lotus 1-2-3 & Excel bug *)
  valid_date (1900, 02, 28);
  wrong_date (1900, 02, 29);
  (* 2000 is leap *)
  valid_date (2000, 02, 28);
  valid_date (2000, 02, 29);
  wrong_date (2000, 02, 30);
  (* 2010 is not leap *)
  valid_date (2010, 02, 28);
  wrong_date (2010, 02, 29);
  (* 2012 is leap *)
  valid_date (2012, 02, 29);
  valid_date (2012, 02, 29);
  wrong_date (2012, 02, 30);
  (* 2100 is not leap *)
  valid_date (2100, 02, 28);
  wrong_date (2100, 02, 29);
  ()

let stamp_trips = test "Random valid dates to stamps round trips" @@ fun () ->
  let of_date = Ptime.of_date $ raw_date @-> ret_get_option stamp in
  for i = 1 to Test_rand.loop_len () do
    let date = Test_rand.date () in
    let trip = Ptime.to_date (of_date date) in
    eq_date date trip
  done;
  ()

let suite = suite "Ptime date tests"
    [ bounds;
      stamp_trips; ]

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
