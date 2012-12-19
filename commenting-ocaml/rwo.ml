(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

let docroot = "../commenting-build/ocaml_commenting/www"
let auth = Auth.Basic ("rwo", "Whirly2")

let check_auth req =
  match Header.get_authorization (Request.headers req) with
  |Some a when a = auth -> true
  |Some _ | None -> false

(* detect Github code and set a cookie if so, otherwise serve static file *)
let dispatch req =
  let headers =
    (* Always set the github_client_id Cookie if not already set *)
    let current_cookies = Cookie.Cookie_hdr.extract (Request.headers req) in
    match List.mem_assoc "github_client_id" current_cookies with
    |false -> 
      let t = Cookie.Set_cookie_hdr.make ("github_client_id", Config.client_id) in
      let k,v = Cookie.Set_cookie_hdr.serialize t in
      Header.init_with k v
    |true -> Header.init ()
  in
  (* See if we have a code in the GET header (signifying a Github redirect)  *)
  match Request.get_param req "code" with
  |None -> (* serve static file *)
    let fname = Server.resolve_file ~docroot ~uri:(Request.uri req) in
    Server.respond_file ~headers ~fname ()
  |Some code -> begin 
    (* talk to Github and get a client id and set the cookie *)
    lwt token = Config.(Github.Token.of_code ~client_id ~client_secret ~code ()) in
    match token with
    |None -> Server.respond_error ~status:`Internal_server_error ~body:"no token" ()
    |Some token ->
      (* Set a cookie with the token and redirect without the code param *)
      let token = Github.Token.to_string token in
      let cookie = Cookie.Set_cookie_hdr.make ("github_access_token", token) in
      let cookie_hdr, cookie_val = Cookie.Set_cookie_hdr.serialize cookie in
      let headers = Header.add headers cookie_hdr cookie_val in
      (* Strip out the code GET param and redirect to the original URL *)
      let new_uri = Uri.remove_query_param (Request.uri req) "code" in
      Server.respond_redirect ~headers ~uri:new_uri ()
  end

(* main callback function *)
let callback con_id ?body req =
  let path = Request.path req in
  printf "%s %s [%s]\n%!" (Code.string_of_method (Request.meth req)) path 
    (String.concat "," (List.map (fun (h,v) -> sprintf "%s=%s" h (String.concat "," v)) 
      (Request.params req)));
  (* Check for auth *)
  match check_auth req with
  |false -> Server.respond_need_auth (`Basic "Real World OCaml") ()
  |true -> dispatch req

let server_t =
  let conn_closed con_id () = () in
  let spec = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Lwt_main.run (Cohttp_lwt_unix.server ~address:"0.0.0.0" ~port:80 spec)
