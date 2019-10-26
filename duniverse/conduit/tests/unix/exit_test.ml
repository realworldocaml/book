(*
 * Copyright (c) 2016 Vincent Bernardoff <vb@luminar.eu.org>
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

open Lwt.Infix

let perform () =
  let stop, do_stop = Lwt.wait () in
  Conduit_lwt_unix.init ~src:"::1" () >>= fun ctx ->
  let serve () =
    let callback _flow ic oc =
      Lwt_io.read ~count:5 ic >>= fun msg ->
      Lwt_io.write oc "foo"
    in
    Conduit_lwt_unix.serve ~stop ~ctx ~mode:(`TCP (`Port 8080)) callback
  in
  let handle = serve () in
  Lwt.async (fun () -> (Lwt_unix.sleep 0.2 >|= Lwt.wakeup do_stop));
  handle

let () =
  Lwt.async_exception_hook := ignore;
  let t_start = Unix.gettimeofday () in
  Lwt_main.run (Lwt_unix.handle_unix_error perform ());
  let t_end = Unix.gettimeofday () in
  if (t_end -. t_start > 0.15) then Printf.printf "OK %.3f\n" (t_end -. t_start)
  else Printf.printf "FAILED %.3f (must be > 0.2)" (t_end -. t_start)
