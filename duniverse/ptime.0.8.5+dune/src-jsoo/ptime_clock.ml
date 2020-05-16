(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Js_of_ocaml

let str = Printf.sprintf

(* JavaScript values *)

let date = Js.Unsafe.variable "Date"
let date_now = Js.Unsafe.variable "Date.now"
let date_period = None (* Unknown *)

(* POSIX clock *)

let raw t = Ptime.(Span.to_d_ps (to_span t))
let (dmin, _) = raw Ptime.min
let (dmax, _) = raw Ptime.max

let err t =
  let err = str "Ptime_clock: can't represent JavaScript timestamp %f" t in
  raise (Sys_error err)

let now () =
  let (ms : float) = Js.Unsafe.fun_call date_now [||] in
  if ms <> ms (* nan *) then err ms else
  let days = int_of_float (floor (ms /. 86_400_000.)) in
  if days < dmin || days > dmax then err ms else
  let rem_ms = mod_float ms 86_400_000. in
  let rem_ms = if rem_ms < 0. then 86_400_000. +. rem_ms else rem_ms in
  if rem_ms >= 86_400_000. then
    let days = days + 1 in
    if days > dmax then err ms else
    Ptime.unsafe_of_d_ps (days, 0L)
  else
  let frac_ms, rem_ms = modf rem_ms in
  let frac_ps = Int64.(of_float (frac_ms *. 1e9)) in
  let rem_ps = Int64.(mul (of_float rem_ms) 1_000_000_000L) in
  Ptime.unsafe_of_d_ps (days, (Int64.add rem_ps frac_ps))

let period () = Ptime.Span.unsafe_of_d_ps_option date_period

(* System time zone offset *)

let current_tz_offset_s () =
  let d = Js.Unsafe.new_obj date [||] in
  Some ((Js.Unsafe.meth_call d "getTimezoneOffset" [||] : int) * (-60))

(* Raw interface *)

let now_d_ps () = raw (now ())
let period_d_ps () = date_period

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
