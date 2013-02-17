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

let docroot = "./live_site"
let dataroot = "./fragments"

let user = "ocamllabs"
let repo = "rwo-comments"

let our_token =
  Lwt_main.run (
    match_lwt Github_cookie_jar.get "rwo" with
    |None -> failwith "No 'rwo' github cookie found: add with git-jar`"
    |Some auth -> return (Github.Token.of_auth auth)
  )

module Auth = struct
  (* Keep an in-memory list of access token to usernames *)
  let sessions = Hashtbl.create 1

  let lookup (token:Github.Token.t) =
    match Hashtbl.mem sessions token with
    |true -> return (Hashtbl.find sessions token)
    |false -> (* query Github for the user info *)
      Github.(Monad.run (User.current_info ~token ()))
      >>= fun user_info ->
      let login = user_info.Github_t.user_info_login in
      Hashtbl.add sessions token login; 
      return login

  let check ~milestone ~login =
    let allowed_users = Config.allowed_users milestone in
    List.mem login allowed_users

  let denied =
    let tmpl = Core.Std.In_channel.read_all "forbidden.html.in" in
    fun ~login ->
      Re_str.(global_replace (regexp_string "@USER@") login tmpl)
end

module Comment = struct
  let assoc_opt l id =
    try Some (List.assoc id l) with Not_found -> None

  (* Add start of day, make a milestone number -> name mapping *)
  let milestones =
    let ms = Lwt_main.run (Github.Monad.(run (
      Github.Milestone.for_repo ~user ~repo ~state:`Open ()
      >>= fun m ->
      Github.Milestone.for_repo ~user ~repo ~state:`Closed ()
      >>= fun m' ->
      return (m' @ m)
    ))) in
    let ms = List.sort (fun a b -> 
      let open Github_t in
      match a.milestone_title, b.milestone_title with
      |"trunk",_ -> -1
      |_,"trunk" -> 1
      |_ -> Pervasives.compare a.milestone_due_on b.milestone_due_on
    ) ms in
    print_endline "Known milestones\n";
    List.fold_left (fun acc m ->
      let open Github_t in
      printf " %d: %s (%s)\n%!" m.milestone_number m.milestone_title m.milestone_description;
      (m.milestone_number,(m.milestone_title, m.milestone_description))::acc
    ) [] ms

  let all_milestones =
    List.rev (List.fold_left (fun a (_,(n,_)) -> n :: a) [] milestones )

  (* Generate the index.html *)
  let index =
    let tmpl = Core.Std.In_channel.read_all "index.html.in" in
    let ms = String.concat "\n" (
      List.map (fun (id,(name,descr)) ->
        let pdf = if Sys.file_exists (docroot ^ "/" ^ name ^ "/rwo-snapshot.pdf") then sprintf "<a href=\"%s/rwo-snapshot.pdf\"><img src=\"media/img/pdf_icon_small.gif\" /></a>" name else "" in
        sprintf "<li><b><a href=\"%s/en/html/\">%s</a></b>: %s <br/><a href=\"%s/en/html/\"><img src=\"media/img/html_icon_small.gif\" /> %s</li>" name name descr name pdf;
      ) milestones)
    in
    Re_str.(global_replace (regexp_string "@MILESTONES@") ms tmpl )
     
  let t = Hashtbl.create 1
  (* Read in a milestone into our memory cache *)
  let init ~milestone =
    match Hashtbl.mem t milestone with
    |true -> Some (Hashtbl.find t milestone)
    |false -> begin
       try
         let ids = Core.Std.Sexp.load_sexp_conv_exn 
           (Filename.concat dataroot milestone) Para_frag.ts_of_sexp in
         Hashtbl.add t milestone ids;
         Some ids
       with _ -> None
    end

  (* Get fragment info for an ID *)
  let get ~milestone_number ~id =
    match assoc_opt milestones milestone_number with
    |None -> eprintf "Unknown milestone %d\n%!" milestone_number; None
    |Some (milestone,_) -> begin
      match init ~milestone with
      |None -> eprintf "No milestone dump data for %s\n%!" milestone; None
      |Some ids -> begin
         match assoc_opt ids id with
         |None -> None
         |Some frag -> Some (milestone, frag)
      end
    end

  (* Parse out an ID from the issue title *)
  let extract_id title =
    try
      let id = Scanf.sscanf title "New comment on block [block-%s@]" (fun s -> s) in
      Some id
    with _ -> None

  (* HTML comment for context *)
  let context_comment ~milestone ~id ~frag =
    let comment_header = sprintf "This comment references this from milestone %s:" milestone in
    let url = sprintf "http://www.realworldocaml.org/%s/en/html/%s#%s" milestone frag.Para_frag.file id in
    sprintf "%s [%s](%s)\n\nContext:\n\n%s" comment_header url url frag.Para_frag.html 
    
  (* Create and edit issue on Github *)
  let create_issue ~context ~user_token ~new_issue =
    Github.(Monad.(run (
      (* This creation will not include milestone/labels if the user isnt authorized *)
      Issues.create ~token:user_token ~user ~repo ~issue:new_issue ()
      >>= fun issue ->
      let issue_number = issue.Github_t.issue_number in
      (* Edit the issue to add a milestone using our builtin token *)
      Issues.edit ~token:our_token ~user ~repo ~issue_number ~issue:new_issue ()
      >>= fun issue ->
      (* Add our context comment *)
      Github.Issues.create_comment ~token:our_token ~user ~repo ~issue_number ~body:context ()
      >>= fun _ -> return issue
    )))
end

let check_auth req =
  match Header.get_authorization (Request.headers req) with
  |Some a when a = Config.auth -> true
  |Some _ | None -> false

let is_directory path =
  try Sys.is_directory path with _ -> false

(* Proxy issue creation so that we can set milestones *)
let dispatch_post ?body req =
  lwt body = Body.string_of_body body in
  match Request.get_param req "access_token" with
  |None -> print_endline "no access token"; Server.respond_not_found ()
  |Some token -> begin
    let open Github_t in
    let user_token = Github.Token.of_string token in
    let new_issue = Github_j.new_issue_of_string body in
    let milestone_number = new_issue.new_issue_milestone in
    match milestone_number, (Comment.extract_id new_issue.new_issue_title) with
    |_,None ->
       eprintf "unable to extract comment\n%!";
       Server.respond_not_found ()
    |None,_ ->
       eprintf "no milestone number\n";
       Server.respond_not_found ()
    |Some milestone_number, Some id -> begin
      match Comment.get ~milestone_number ~id with
      |None ->
        eprintf "unknown comment %s milestone %d\n%!" id milestone_number;
        Server.respond_not_found ()
      |Some (milestone,frag) ->
        let context = Comment.context_comment ~milestone ~id ~frag in
        lwt created_issue = Comment.create_issue ~context ~user_token ~new_issue in
        let body = Body.body_of_string (Github_j.string_of_issue created_issue) in
        let headers = Header.init_with "content-type" "application/json" in
        Server.respond ~headers ~status:`Created ~body ()  
    end
  end

(* Server static file with no auth checks or anything special.
 * Meant for the javascript/etc that is always needed without auth *)
let dispatch_static req =
  let uri = Request.uri req in
  let fname = Server.resolve_file ~docroot ~uri in
  Server.respond_file ~fname ()
  
(* detect Github code and set a cookie if so, otherwise serve static file *)
let dispatch ~milestone req =
  let current_cookies = Cookie.Cookie_hdr.extract (Request.headers req) in
  (* Extract the access_token so we can do an ACL check *)
  let access_token =
    match List.mem_assoc "github_access_token" current_cookies with
    |false -> None
    |true -> Some (Github.Token.of_string (List.assoc "github_access_token" current_cookies))
  in
  (* Always set the github_client_id Cookie if not already set *)
  let headers = 
    match List.mem_assoc "github_client_id" current_cookies with
    |false -> 
      let t = Cookie.Set_cookie_hdr.make ("github_client_id", Config.client_id) in
      let k,v = Cookie.Set_cookie_hdr.serialize t in
      Header.init_with k v
    |true -> Header.init ()
  in
  (* See if we have a code in the GET header (signifying a Github redirect) *)
  let code = Request.get_param req "code" in
  match access_token, code with
  (* Have access token and no code, so serve file *)
  |Some access_token, None -> begin 
    (* Check that the user is allowed to access this page *)
    lwt login = Auth.lookup access_token in
    match Auth.check ~milestone ~login with
    |false -> Server.respond_string ~status:`Forbidden ~body:(Auth.denied ~login) ()
    |true -> begin
      let uri = Request.uri req in
      let fname = Server.resolve_file ~docroot ~uri in
      let path = Uri.path uri in
      let pathlen = String.length path in
      match Uri.path uri with
      |path when pathlen>0 && path.[pathlen-1] = '/' ->
        let fname = fname ^ "index.html" in
        Server.respond_file ~headers ~fname ()
      |path when is_directory fname ->
        Server.respond_redirect ~headers ~uri:(Uri.with_path uri (path ^ "/")) ()
      |path ->
        Server.respond_file ~headers ~fname ()
    end
  end
  (* No access token and no code, so redirect to Github oAuth login *)
  |None, None ->
    let redirect_uri = Uri.(with_path (of_string "http://www.realworldocaml.org") (Request.path req)) in
    let uri = Github.URI.authorize ~scopes:[`Public_repo] ~redirect_uri 
      ~client_id:Config.client_id () in
    printf "Redirect for auth to %s\n%!" (Uri.to_string uri);
    Server.respond_redirect ~headers ~uri ()
  (* Have a code parameter, signifying a Github redirect *)
  |_, Some code -> begin 
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
  (* Check that the host is www.realworldocaml.org, as the Github redirect requires
   * the exact match, or it'll reject the cross-domain Javascript *)
  match Request.header req "host" with
  |Some "www.realworldocaml.org" -> begin
    match Request.meth req with
    |`POST -> dispatch_post ?body req
    |`GET -> begin
      let path = Uri.path (Request.uri req) in
      let bits = Re_str.(split (regexp_string "/") path) in
      match bits with
      |[] -> Server.respond_string ~status:`OK ~body:Comment.index ()
      |"media"::_ -> dispatch_static req (* No auth required for support files *)
      |_::"media"::_ -> dispatch_static req (* No auth required for support files *)
      |milestone::_ when List.mem milestone Comment.all_milestones ->
        dispatch ~milestone req
      |_ -> Server.respond_not_found ()
    end
    |_ -> Server.respond_not_found ()
  end
  |Some _ | None -> (* redirect to www.realworldocaml.org *)
    print_endline "redirecting to www.realworldocaml.org";
    let uri = Uri.with_host (Request.uri req) (Some "www.realworldocaml.org") in
    Server.respond_redirect ~uri ()

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let conn_closed con_id () = () in
  let spec = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Lwt_main.run (Cohttp_lwt_unix.server ~address:"0.0.0.0" ~port:80 spec)
