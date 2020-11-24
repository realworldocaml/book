(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Compile with:

     ocamlfind ocamlc \
       -package ptime.clock.jsoo -o min_clock_jsoo.byte min_clock_jsoo.ml
     js_of_ocaml min_clock_jsoo.byte

   Use the resulting JavaScript file with min_clock_jsoo.html *)

let setup_log () =
  let log = Dom_html.(createDiv document) in
  let add_entry s =
    let e = Dom_html.(createP document) in
    Js.Unsafe.set e "innerHTML" (Js.string s);
    Dom.appendChild log e;
  in
  Dom.appendChild (Js.Unsafe.get Dom_html.document "body") log;
  Sys_js.set_channel_flusher stdout add_entry;
  Sys_js.set_channel_flusher stderr add_entry;
  ()

let pp_period ppf = function
| None -> Format.fprintf ppf "unknown"
| Some p -> Ptime.Span.pp ppf p

let pp_tz ppf = function
| None -> Format.fprintf ppf "unknown"
| Some tz -> Format.fprintf ppf "%ds" tz

let main _ =
  setup_log ();
  let now = Ptime_clock.now () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let period = Ptime_clock.period () in
  Format.printf "Clock period: %a@." pp_period period;
  Format.printf "   TZ offset: %a@." pp_tz tz_offset_s;
  Format.printf "   Now UTC  : %a@." Ptime.pp now;
  Format.printf "   Now local: %a@." Ptime.(pp_human ?tz_offset_s ()) now;
  Js._false

let () = Js.Unsafe.set Dom_html.window "onload" (Dom_html.handler main)

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
