(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Lwt
open Printf
open Cohttp
open Cohttp_lwt_unix

(* main callback function *)
let callback con_id ?body req =
  Server.respond_string ~status:`Not_found ~body:"Site is temporarily down as we transition to an SSL-only site.  It'll be back up very shortly." ()

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let conn_closed con_id () = () in
  let spec = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Lwt_main.run (Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port:80 spec)
