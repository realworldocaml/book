(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let eq_weekday pd d =
  let eq_int v v' = eq ~eq:(=) ~pp:pp_int v v' in
  let pd = match pd with
  | `Sun -> 0 | `Mon -> 1 | `Tue -> 2 | `Wed -> 3 | `Thu -> 4 | `Fri -> 5
  | `Sat -> 6
  in
  eq_int pd d

let rand_stamp =
  Test_rand.(if Sys.word_size = 32 then float_stamp_32bits else float_stamp)

let value_of_posix_s =
  Ptime.of_float_s $ pp_float @-> (ret_get_option stamp)

let unix_to_date_time t =
  let t = Ptime.to_float_s t in
  let t = floor t (* see http://caml.inria.fr/mantis/view.php?id=6921 *) in
  let tm = Unix.gmtime t in
  let d = (tm.Unix.tm_year + 1900), (tm.Unix.tm_mon + 1), (tm.Unix.tm_mday) in
  let t = tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec in
  (d, (t, 0)), tm.Unix.tm_wday

let compare t =
  let dt, wday = unix_to_date_time t in
  let ut = Ptime.to_date_time t in
  eq_date_time dt ut;
  eq_weekday (Ptime.weekday t) wday;
  dt

let compare = compare $ stamp @-> ret raw_date_time

let stamp_to_date_time =
  Testing.test "Random Ptime-valid stamps to date-time" @@ fun () ->
  if Sys.word_size > 32 then begin
    ignore (compare Ptime.min);
    ignore (compare Ptime.(truncate ~frac_s:0 max));
  end;
  for i = 1 to Test_rand.loop_len () do ignore (compare (rand_stamp ())) done;
  ()

let exhaustive_min_max =
  if Sys.word_size > 32
  then Ptime.(to_float_s min), Ptime.(to_float_s  (truncate ~frac_s:0 max))
  else Int32.(to_float min_int), Int32.(to_float max_int)

let exhaustive =
  Testing.test "Each second Ptime-valid stamps to date-time" @@ fun () ->
  let min, max = exhaustive_min_max in
  let rec loop t y =
    if t > max then () else
    let stamp = value_of_posix_s t in
    let y', _, _ = fst (compare (stamp)) in
    if y <> y' then log "year: %d" y';
    loop (t +. 1.0) y'
  in
  loop min (-min_int)

let exhaustive_test = ref false

let suite () =
  Testing.suite "Ptime tests against Unix.gmtime" @@
  [ stamp_to_date_time; ] @
  if !exhaustive_test then [exhaustive] else []

let exhaustive =
  "-e", Arg.Set exhaustive_test,
  "Perform exhaustive tests on the whole Ptime range"

let run () =
  Test_rand.cmdline ~opts:[exhaustive] ();
  Testing.run [suite ()];
  Testing.log_results ()

let () = if run () then exit 0 else exit 1

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
