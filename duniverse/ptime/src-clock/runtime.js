/*---------------------------------------------------------------------------
   Copyright (c) 2022 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

//Provides: ocaml_ptime_clock_period_d_ps
function ocaml_ptime_clock_period_d_ps (_unit) {
  return 0;
}

//Provides: ocaml_ptime_clock_current_tz_offset_s
function ocaml_ptime_clock_current_tz_offset_s (_unit) {
  return [0, ((new Date ()).getTimezoneOffset () * -60)]
}

//Provides: ocaml_ptime_clock_now_d_ps
//Requires: caml_int64_of_int32, caml_int64_of_float
//Requires: caml_int64_add, caml_int64_mul
//Requires: caml_modf_float
//Requires: caml_raise_sys_error
function ocaml_ptime_clock_now_d_ps (_unit) {
  function err (ms) {
    caml_raise_sys_error
    ("Ptime_clock: can't represent JavaScript timestamp " + ms);
  }
  var dmin = -719528; /* Day component of Ptime.min */
  var dmax = 2932896; /* Day component of Ptime.max */
  var ms = Date.now ();
  var ps;
  if (ms != ms) err (ms)
  var days = Math.floor (ms / 86400000);
  if (days < dmin || days > dmax) err(ms);
  var rem_ms = ms % 86400000;
  if (rem_ms < 0) rem_ms += 86400000
  if (rem_ms >= 86400000) {
    /* Guard against a potential overflow in the computation of [rem_s] */
    days += 1;
    if (days > dmax) err (ms);
    ps = caml_int64_of_int32 (0);
  }
  else {
    var modf = caml_modf_float (rem_ms);
    var fract_ps = caml_int64_of_float (modf[1] * 1e9);
    var rem_ps = caml_int64_mul (caml_int64_of_float (modf[2]),
                                 caml_int64_of_int32 (1000000000));
    ps = caml_int64_add (rem_ps, fract_ps);
  }
  return [0, days, ps]
}

/*---------------------------------------------------------------------------
   Copyright (c) 2022 The ptime programmers

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
  ---------------------------------------------------------------------------*/
