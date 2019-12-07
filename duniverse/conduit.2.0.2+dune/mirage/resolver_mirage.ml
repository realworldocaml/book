(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

let is_tls_service =
  (* TODO fill in the blanks. nowhere else to get this information *)
  function
  | "https" | "imaps" -> true
  | _ -> false

let get_host uri =
  match Uri.host uri with
  | None -> "localhost"
  | Some host ->
      match Ipaddr.of_string host with
      | Ok ip -> Ipaddr.to_string ip
      | Error _ -> host

let get_port service uri =
  match Uri.port uri with
  | None -> service.Resolver.port
  | Some port -> port

let static_resolver hosts service uri =
  let port = get_port service uri in
  try
    let fn = Hashtbl.find hosts (get_host uri) in
    Lwt.return (fn ~port)
  with Not_found ->
    Lwt.return (`Unknown ("name resolution failed"))

let static_service name =
  match Uri_services.tcp_port_of_service name with
  | [] -> Lwt.return_none
  | port::_ ->
     let tls = is_tls_service name in
     let svc = { Resolver.name; port; tls } in
     Lwt.return (Some svc)

let static hosts =
  let service = static_service in
  let rewrites = ["", static_resolver hosts] in
  Resolver_lwt.init ~service ~rewrites ()

let localhost =
  let hosts = Hashtbl.create 3 in
  Hashtbl.add hosts "localhost"
              (fun ~port -> `TCP (Ipaddr.(V4 V4.localhost), port));
  static hosts

module Make_with_stack (R: Mirage_random.S) (C: Mirage_clock.MCLOCK) (S: Mirage_stack.V4) = struct
  include Resolver_lwt

  module R = struct
    let vchan_resolver ~tld =
      let tld_len = String.length tld in
      let get_short_host uri =
        let n = get_host uri in
        let len = String.length n in
        if len > tld_len && (String.sub n (len-tld_len) tld_len = tld) then
          String.sub n 0 (len-tld_len)
        else
          n
      in
      fun service uri ->
        (* Strip the tld from the hostname *)
        let remote_name = get_short_host uri in
        Printf.printf "vchan_lookup: %s %s -> normalizes to %s\n%!"
          (Sexplib.Sexp.to_string_hum (Resolver.sexp_of_service service))
          (Uri.to_string uri) remote_name;
        Lwt.return (`Vchan_domain_socket (remote_name, service.Resolver.name))

    module DNS = Dns_client_mirage.Make(R)(C)(S)

    let dns_stub_resolver dns service uri : Conduit.endp Lwt.t =
      let hostn = get_host uri in
      let port = get_port service uri in
      (match Ipaddr.V4.of_string hostn with
       | Ok addr -> Lwt.return (Ok addr)
       | Error _ ->
         match Domain_name.of_string hostn with
         | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
         | Ok domain ->
           match Domain_name.host domain with
           | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
           | Ok host -> DNS.gethostbyname dns host) >|= function
      | Error (`Msg err) -> `Unknown ("name resolution failed: " ^ err)
      | Ok addr -> `TCP (Ipaddr.V4 addr, port)

    let register ?ns ?(ns_port = 53) ?stack res =
      begin match stack with
        | Some s ->
          (* DNS stub resolver *)
          let nameserver = match ns with None -> None | Some ip -> Some (`TCP, (ip, ns_port)) in
          let dns = DNS.create ?nameserver s in
          let f = dns_stub_resolver dns in
          Resolver_lwt.add_rewrite ~host:"" ~f res
        | None -> ()
      end;
      let service = Resolver_lwt.(service res ++ static_service) in
      Resolver_lwt.set_service ~f:service res;
      let vchan_tld = ".xen" in
      let vchan_res = vchan_resolver ~tld:vchan_tld in
      Resolver_lwt.add_rewrite ~host:vchan_tld ~f:vchan_res res

    let init ?ns ?ns_port ?stack () =
      let res = Resolver_lwt.init () in
      register ?ns ?ns_port ?stack res;
      res
  end
end
