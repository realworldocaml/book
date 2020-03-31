(*
 * Copyright (c) 2015 Anil Madhavapeddy <anil@recoil.org>
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

open Core.Std
open Async.Std

let handler sock ic oc =
  Reader.pipe ic |> fun rd ->
  Writer.pipe oc |> fun wr ->
  Pipe.transfer_id rd wr

let determine_mode cert_file_path key_file_path =
  (* Determines if the server runs in http or https *)
  match (cert_file_path, key_file_path) with
  | Some c, Some k -> `OpenSSL (`Crt_file_path c, `Key_file_path k)
  | None, None -> `TCP
  | _ -> failwith "Error: must specify both certificate and key for TLS"

let start_server port host cert_file key_file () =
  let mode = determine_mode cert_file key_file in
  let mode_str = (match mode with `OpenSSL _ -> "OpenSSL" | `TCP -> "TCP") in
  printf "Listening for %s requests on: %s %d\n%!" mode_str host port;
  Unix.Inet_addr.of_string_or_getbyname host
  >>= fun host ->
  let listen_on = Tcp.Where_to_listen.create
      ~socket_type:Socket.Type.tcp
      ~address:(`Inet (host,port))
      ~listening_on:(fun _ -> port)
  in
  Conduit_async.serve
    ~on_handler_error:`Raise
    mode
    listen_on handler
  >>= fun _ -> never ()

let _ =
  Command.async_basic
    ~summary:"Echo server over SSL"
    Command.Spec.(
      empty
      +> flag "-p" (optional_with_default 8080 int) ~doc:"port TCP port to listen on"
      +> flag "-s" (optional_with_default "0.0.0.0" string) ~doc:"address IP address to listen on"
      +> flag "-cert-file" (optional file) ~doc:"file Certificate file" 
      +> flag "-key-file" (optional file) ~doc:"File Private key file"
    ) start_server
  |> Command.run

