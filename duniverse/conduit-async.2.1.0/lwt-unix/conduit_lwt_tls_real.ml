(*
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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

open Lwt.Infix

let () = Mirage_crypto_rng_unix.initialize ()

module X509 = struct
  let private_of_pems ~cert ~priv_key =
    X509_lwt.private_of_pems ~cert ~priv_key
end

module Client = struct
  let connect ?src ?certificates host sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
        (match src with
         | None -> Lwt.return_unit
         | Some src_sa -> Lwt_unix.bind fd src_sa) >>= fun () ->
        let authenticator ~host:_ _ = Ok None in
        let config = Tls.Config.client ~authenticator ?certificates () in
        Lwt_unix.connect fd sa >>= fun () ->
        Tls_lwt.Unix.client_of_fd config ~host fd >|= fun t ->
        let ic, oc = Tls_lwt.of_t t in
        (fd, ic, oc)
      )
end

module Server = struct

  let init' ?backlog ?stop ?timeout tls sa callback =
    sa
    |> Conduit_lwt_server.listen ?backlog
    >>= Conduit_lwt_server.init ?stop (fun (fd, addr) ->
        Lwt.try_bind
          (fun () -> Tls_lwt.Unix.server_of_fd tls fd)
          (fun t ->
             let (ic, oc) = Tls_lwt.of_t t in
             Lwt.return (fd, ic, oc))
          (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)
        >>= Conduit_lwt_server.process_accept ?timeout (callback addr))

  let init ?backlog ~certfile ~keyfile ?stop ?timeout sa callback =
    X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile
    >>= fun certificate ->
    let config = Tls.Config.server ~certificates:(`Single certificate) () in
    init' ?backlog ?stop ?timeout config sa callback
end

let available = true
