(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let us_to_ns = 1000L (* microsecond to nanosecond uint64 multiplier *)

(* Get a handle on JavaScript's performance.now *)

let performance_now_ms_unavailable () =
  raise (Sys_error ("performance.now () is not available"))

let performance_now_ms =
  let has_perf = Js.Unsafe.get Dom_html.window "performance" in
  match Js.Optdef.to_option has_perf with
  | None -> performance_now_ms_unavailable
  | Some p ->
      match Js.Unsafe.get p "now" with
      | None -> performance_now_ms_unavailable
      | Some n -> fun () -> Js.Unsafe.meth_call p "now" [||]

(* Conversion of DOMHighResTimeStamp to uint64 nanosecond timestamps.

   The spec https://www.w3.org/TR/hr-time-3 says DOMHighResTimeStamp
   are double milliseconds that *should* be accurate to 5 microseconds.
   We simply assume we have microsecond precision and multiply the
   stamps given by performance.now () by 1e3 to get double microseconds.

   We then use Int64.of_float on these double microseconds to get an
   uint64 in microseconds. This works in practice for the following
   reasons. Let us assume we have the largest integer microsecond
   timestamp representable exactly in double, i.e. 2^53 :

   1) Assuming the zero of performance.now is when the tab is created,
      our 2^53 timestamp only occurs after:

        2^53 / 1_000_000 / (24 * 3600 * 365.25) ≅ 285.4 Julian years

   2) 2^53 < Int64.max_int = 2^63 - 1, so seing the result of
      Int64.of_float as unsigned for this timestamp is correct and in
      the defined domain of the conversion function (the truncated float
      must lie in [Int64.min_int;Int64.max_int] for defined behaviour).

   So the Int64.of_float conversion is unlikely to be problematic and
   we simply bring the resulting uint64 microsecond to an uint64
   nanosecond by multiplying by 1000L, which for 2^53 microseconds
   remains smaller than Int64.max_int, yielding a correct uint64
   nanosecond timestamp for a reasonable time range. *)

(* Raw interface *)

let now_us () = performance_now_ms () *. 1e3

let start_us = now_us ()
let elapsed_ns () = Int64.(mul (of_float @@ now_us () -. start_us) us_to_ns)
let now_ns () = Int64.(mul (of_float @@ now_us ()) us_to_ns)
let period_ns () = None

(* Monotonic clock *)

let elapsed () = Mtime.Span.of_uint64_ns (elapsed_ns ())
let now () = Mtime.of_uint64_ns (now_ns ())
let period () = Mtime.Span.unsafe_of_uint64_ns_option (period_ns ())

(* Counters *)

type counter = float
let counter = now_us
let count c =
  Mtime.Span.of_uint64_ns (Int64.(mul (of_float  @@ now_us () -. c)) us_to_ns)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli

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
