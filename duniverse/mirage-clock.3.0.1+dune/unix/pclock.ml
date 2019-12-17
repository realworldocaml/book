
(*
 * Copyright (c) 2015 Matt Gray <matthew.thomas.gray@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let ps_count_in_s = 1_000_000_000_000L

external posix_clock_gettime_s_ns : unit -> int64 * int64 = "ocaml_posix_clock_gettime_s_ns"

let now_d_ps () =
  let secs, ns = posix_clock_gettime_s_ns () in
  let days = Int64.div secs 86_400L in
  let rem_s = Int64.rem secs 86_400L in
  let frac_ps = Int64.mul ns 1000L in
  let rem_ps = Int64.mul rem_s ps_count_in_s in
  (Int64.to_int days, (Int64.add rem_ps frac_ps))

let current_tz_offset_s () =
  let now = Unix.gettimeofday () in
  let utc = Unix.gmtime now in
  let local = Unix.localtime now in
  let d_day = local.Unix.tm_yday - utc.Unix.tm_yday in
  let d_hour = local.Unix.tm_hour - utc.Unix.tm_hour in
  let d_min = d_hour * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
  let min_per_day = 24 * 60 in
  let d_min =
    match d_day with
      | 0 -> d_min (* same day *)
      (* day wrapped *)
      | 1 -> d_min + min_per_day
      | -1 -> d_min - min_per_day
      (* year wrapped *)
      | _ -> if d_min < -1 then d_min + min_per_day else d_min - min_per_day
    in
  Some (d_min * 60)

external posix_clock_period_ns : unit -> int64 = "ocaml_posix_clock_period_ns"

let period_d_ps () =
  let period_ns = posix_clock_period_ns () in
  match period_ns with
    | 0L -> None
    | ns -> Some (0, Int64.mul ns 1000L)
