(*
 * Copyright (c) 2014-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Sexplib.Conv

type direct = [`Direct of int * Vchan.Port.t]

let (>>=) = Lwt.(>>=)
let (/) = Filename.concat

let fail fmt = Printf.ksprintf (fun m -> Lwt.fail (Failure m)) fmt
let err_peer_not_found = fail "Conduit_xenstore: %s peer not found"
let err_no_entry_found () =
  fail "No /conduit Xenstore entry found. Run `xenstore-conduit-init`"
let err_port = fail "%s: invalid port"

module Make (Xs: Xs_client_lwt.S) = struct

  type t = { xs: (Xs.client [@sexp.opaque]); name: string } [@@deriving sexp_of]

  let get_my_id xs = Xs.(immediate xs (fun h -> read h "domid"))

  let xenstore_register xs myname =
    get_my_id xs >>= fun domid ->
    Xs.(immediate xs (fun h -> write h ("/conduit" / myname) domid))

  let get_peer_id xs name =
    Lwt.catch
      (fun () -> Xs.(immediate xs (fun h -> read h ("/conduit" / name))))
      (fun _  -> err_peer_not_found name)

  let readdir h d =
    Xs.(directory h d) >>= fun dirs ->
    let dirs = List.filter (fun p -> p <> "") dirs in
    match dirs with
    | []    -> Lwt.fail Xs_protocol.Eagain
    | hd::_ -> Lwt.return hd

  let register name =
    Xs.make () >>= fun xs ->
    (* Check that a /conduit directory exists *)
    Lwt.catch
      (fun () ->
         Xs.(immediate xs (fun h -> read h "/conduit")) >>= fun _ ->
         Lwt.return_unit)
      (fun _ -> err_no_entry_found ())
    >>= fun () ->
    xenstore_register xs name >>= fun () ->
    Lwt.return { xs; name }

  let accept {xs; name } =
    let waitfn h =
      readdir h ("/conduit" / name) >>= fun remote_name ->
      readdir h ("/conduit" / name / remote_name) >>= fun port ->
      Xs.read h ("/conduit" / remote_name) >>= fun remote_domid ->
      let remote_domid = int_of_string remote_domid in
      Xs.rm h ("/conduit" / name / remote_name) >>= fun () ->
      match Vchan.Port.of_string port with
      | Error (`Msg e) -> err_port e
      | Ok port -> Lwt.return (`Direct (remote_domid, port))
    in
    Xs.wait xs waitfn

  let listen ({name; _} as v) =
    (* TODO cancellation *)
    let conn, push_conn = Lwt_stream.create () in
    Printf.printf "Conduit_xenstore: listen on %s\n%!" name;
    let rec loop () =
      accept v >>= fun c ->
      push_conn (Some c);
      loop ()
    in
    Lwt.ignore_result (loop ());
    Lwt.return conn

  let connect {xs; name} ~remote_name ~port =
    let port_str = Vchan.Port.to_string port in
    get_peer_id xs remote_name >>= fun remote_domid ->
    let remote_domid = int_of_string remote_domid in
    let path = "/conduit" / remote_name / name / port_str in
    Xs.(immediate xs (fun h -> write h path port_str)) >>= fun () ->
    Lwt.return (`Direct (remote_domid, port))

end
