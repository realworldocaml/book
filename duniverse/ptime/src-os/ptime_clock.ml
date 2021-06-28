(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Stubs *)

external ptime_clock_now_d_ps : unit -> int * int64 =
  "ocaml_ptime_clock_now_d_ps"

external ptime_clock_period_d_ps : unit -> (int * int64) option =
  "ocaml_ptime_clock_period_d_ps"

external ptime_clock_current_tz_offset_s : unit -> int option =
  "ocaml_ptime_clock_current_tz_offset_s"

(* POSIX clock *)

let now () = Ptime.unsafe_of_d_ps (ptime_clock_now_d_ps ())
let period () = Ptime.Span.unsafe_of_d_ps_option (ptime_clock_period_d_ps ())

(* System time zone offset *)

let current_tz_offset_s = ptime_clock_current_tz_offset_s

(* Raw interface *)

let now_d_ps = ptime_clock_now_d_ps
let period_d_ps = ptime_clock_period_d_ps

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
