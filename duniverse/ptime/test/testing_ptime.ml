(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing

let unit ppf () = Format.fprintf ppf "()"
let float ppf f = Format.fprintf ppf "%.10f" f
let int64 ppf i = Format.fprintf ppf "%Ld" i

(* Time spans *)

let raw_span ppf (d, ps) = Format.fprintf ppf "@[<1>(%d,@ %Ld)@]" d ps
let eq_raw_span = eq ~eq:(=) ~pp:raw_span

let span = Ptime.Span.dump
let eq_span = eq ~eq:Ptime.Span.equal ~pp:span
let eq_span_opt = eq_option ~eq:Ptime.Span.equal ~pp:span

(* Timestamps *)

let stamp = Ptime.dump
let eq_stamp = eq ~eq:Ptime.equal ~pp:stamp
let eq_stamp_opt = eq_option ~eq:Ptime.equal ~pp:stamp

(* Dates *)

let raw_date ppf (y,m,d) = Format.fprintf ppf "(%d, %d, %d)" y m d
let eq_date = eq ~eq:(=) ~pp:raw_date

(* Times *)

let raw_time ppf (_, ((hh, mm, ss), tz)) =
  Format.fprintf ppf "(%d, %d, %d), %d" hh mm ss tz

(* Date times *)

let raw_date_time ppf ((y, m, d), ((hh, mm, ss), tz)) =
  Format.fprintf ppf "(%d, %d, %d), ((%d, %d, %d), %d)"
    y m d hh mm ss tz

let eq_date_time v v' = eq ~eq:(=) ~pp:raw_date_time v v'
let eq_date_time_opt v v' = eq_option ~eq:(=) ~pp:raw_date_time v v'

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
