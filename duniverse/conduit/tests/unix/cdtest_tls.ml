(*
 * Copyright (c) 2016 Skylable Ltd. <info-copyright@skylable.com>
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

let port =
  Random.self_init ();
  16_384 + Random.int 10_000

let config = `Crt_file_path "server.pem", `Key_file_path "server.key", `No_password, `Port port

let rec repeat n f =
  if n = 0 then Lwt.return_unit
  else f () >>= fun () -> repeat (n-1) f

let perform () =
  let stop, do_stop = Lwt.wait () in
  Conduit_lwt_unix.init ~src:"::1" () >>= fun ctx ->
  let serve () =
    Conduit_lwt_unix.serve ~stop ~ctx ~mode:(`TLS config) begin fun _flow ic oc ->
      Lwt_log.notice "Server: Callback started." >>= fun () ->
      Lwt_io.read ~count:5 ic >>= fun msg ->
      Lwt_log.notice "Server: read hello." >>= fun () ->
      Lwt_io.write oc "foo"
    end
  in
  let client_test () =
    (* connect using low-level operations to check what happens if client closes connection
       without calling ssl_shutdown (e.g. TCP connection is lost) *)
    let client = `TLS (`Hostname "", `IP Ipaddr.(V6 V6.localhost), `Port port) in
    Conduit_lwt_unix.(connect ~ctx:default_ctx client) >>= fun (_flow, ic, oc) ->
    Lwt_log.notice "Connected!" >>= fun () ->
    Lwt_io.write oc "hello" >>= fun () ->
    Lwt_log.notice "Written hello." >>= fun () ->
    Lwt_io.read ic ~count:3 >>= fun msg ->
    Lwt_log.notice "Got correct msg, disconnecting." >>= fun () ->
    Lwt_io.close ic
  in
  Lwt.async serve;
  Lwt_unix.sleep 1. >>= fun () ->
  Lwt_log.notice_f "Server running on port %d" port >>= fun () ->
  repeat 10 client_test >>= fun () ->
  Lwt.wakeup do_stop ();
  Lwt.return_unit

let () =
  Lwt.async_exception_hook := ignore;
  Sys.(set_signal sigpipe Signal_ignore);
  Lwt_main.run (Lwt_unix.handle_unix_error perform ());
  print_endline "OK"
