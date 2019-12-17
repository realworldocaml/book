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

external time : unit -> float = "unix_gettimeofday"

let min_int_float = float min_int
let max_int_float = float max_int
let ps_count_in_s = 1_000_000_000_000L

(* Based on Ptime.of_float_s *)
let now_d_ps () =
  let secs = time () in
  if secs <> secs then failwith "unix_gettimeofday returned NaN" else
  let days = floor (secs /. 86_400.) in
  if days < min_int_float || days > max_int_float then failwith
    "unix_gettimeofday returned number of days outside int range" else
  let rem_s = mod_float secs 86_400. in
  let rem_s = if rem_s < 0. then 86_400. +. rem_s else rem_s in
  if rem_s >= 86_400. then (int_of_float days + 1, 0L) else
  let frac_s, rem_s = modf rem_s in
  let rem_ps = Int64.(mul (of_float rem_s) ps_count_in_s) in
  let frac_ps = Int64.(of_float (frac_s *. 1e12)) in
  (int_of_float days, (Int64.add rem_ps frac_ps))

let current_tz_offset_s () = None

(* According to
 * https://github.com/mirage/mini-os/blob/edfd5aae6ec5ba7d0a8834a3e9dfe5e69424150a/arch/x86/time.c#L194
 * the clock period is 1 microsecond
 * *)
let period_d_ps () = Some (0, 1_000_000L)
