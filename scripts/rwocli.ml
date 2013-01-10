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

open Core.Std
open Printf
open Github_t

let version = "1.0.0"
let user = "ocamllabs"
let repo = "rwo-comments"

(* Run a Github function inside an Lwt evaluator *)
let run_gh fn = Lwt_main.run (Github.Monad.run (fn ()))
(* Get our API tokens from the Github cookie jar *)
let auth = Lwt.(Lwt_main.run (
  Github_cookie_jar.get "rwo" >|= fun t ->
  Option.value_exn ~message:"Use git-jar to create an `rwo` cookie first." t))
let token = Github.Token.of_string auth.auth_token

(* Ensure this is a valid issue by extracting the id from the title *)
let id_from_issue i =
  Option.try_with (fun () ->
    let id = Scanf.sscanf i.issue_title "New comment on block [block-%s@]" (fun s -> s) in
    id,i
  )

(* Resolve milestone names *)
let milestones = run_gh (fun () -> Github.Milestone.for_repo ~token ~user ~repo ())
let find_milestone name =
  List.find milestones ~f:(fun m -> m.milestone_title = name)
let find_milestone_num name =
  let m = find_milestone name in
  (Option.value_exn ~message:"Invalid milestone" m).milestone_number

(* Get context from comments on an issue. We assume bactrian-bot is enough here. *)
let find_context i =
  run_gh (fun () -> Github.Issues.comments ~token ~user ~repo ~issue_number:i.issue_number ()) |!
  List.find ~f:(fun c -> c.issue_comment_user.user_login = "bactrian")

let pandoc ?(output="plain") buf =
  let ic,oc = Unix.open_process (sprintf "pandoc -f markdown -t %s" output) in
  Out_channel.output_string oc buf;
  Out_channel.close oc;
  (* If output is "plain" then blockquote the context *)
  match output with
  |"plain" ->
     In_channel.input_all ic |!
     String.split ~on:'\n' |!
     List.map ~f:(fun l -> "> " ^ l) |!
     String.concat ~sep:"\n"
  |_ -> In_channel.input_all ic

(* List comments from Github *)
let list_comments filter_user (milestone:string option) output =
  (* Filter the issues aimed at this milestone *)
  let milestone =
    match milestone with
    |None -> `Any
    |Some name -> `Num (find_milestone_num name)
  in
  let assignee = Option.bind filter_user (fun u -> Some (`Login u)) in
  run_gh (fun () -> Github.Issues.for_repo ~token ~milestone ?assignee ~user ~repo ()) |!
  List.filter_map ~f:id_from_issue |!
  List.sort ~cmp:(fun (id1,_) (id2,_) -> compare id1 id2) |!
  List.iter ~f:(fun (id,i) ->
    let context = find_context i in
    printf "### %s %s\n" id (sprintf "http://github.com/%s/%s/issues/%d" user repo i.issue_number);
    match context with
    |None -> printf "<no context>\n\n"
    |Some c -> 
      let body = Re_str.(replace_first (regexp ".*\n\nContext:\n\n") "" c.issue_comment_body) in
      printf "%s\n%s\n" (pandoc ~output body) i.issue_body
  )
 
module Flag = struct
  open Command.Spec
  let user () =
    flag "-u" ~doc:"USER Github user to filter issues for."
      (optional string)
  let milestone () =
    flag "-m" ~doc:"MILESTONE Restrict search to a Github milestone name."
      (optional string)
  let context () =
    flag "-c" ~doc:"CONTEXT Show paragraph context before the comment."
      no_arg
  let format_ () =
    flag "-f" ~doc:"FORMAT Pipe output through Pandoc (valid: org, plain, markdown). Default is plain."
      (optional_with_default "plain" string)
end

let list_cmd = Command.basic 
    ~summary:"Show issues in paragraph order."
    Command.Spec.(empty
      +> Flag.user ()
      +> Flag.milestone ()
      +> Flag.context ()
      +> Flag.format_ ()
    )
    (fun user milestone context output () ->
       list_comments user milestone output
    )
 
let main =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version
      (Command.group ~summary:"rwo-cli commands"
        [ "list", list_cmd ]
      )
  )
