(* Dump the paragraph fragments and into a sexp *)
(* Usage: cmd output-file [input file1] [input file2...] *)
open Core.Std
open Github_t

type t = {
  file: string;
  html: string;
} with sexp
type ts = (string * t) list with sexp

let user = "ocamllabs"
let repo = "rwo-comments"

let comment_header = "This comment references this paragraph: "
let run_gh fn = Lwt_main.run (Github.Monad.run (fn ()))

let extract_id_from_issue i =
  try
    let id = Scanf.sscanf i.issue_title "New comment on block [block-%s@]" (fun s -> s) in
    Some (id,i)
  with _ -> None

let github_comments (ids:(string * t) list) token milestone =
  (* Filter the issues aimed at this milestone *)
  run_gh (Github.Issues.for_repo ~token ~user ~repo) |!
  List.filter ~f:(fun i ->
    match i.issue_milestone with
    |None -> false
    |Some m -> m.milestone_title = milestone) |!
  List.filter_map ~f:extract_id_from_issue |!
  List.iter ~f:(fun (id,i) ->
    match List.Assoc.find ids id with
    |None -> prerr_endline  ("WARNING: couldnt find id " ^ id)
    |Some t ->
      (* See if any of the issue comments match our header *)
      run_gh (Github.Issues.comments ~token ~user ~repo ~issue_number:i.issue_number) |!
      List.filter ~f:(fun {issue_comment_body} ->
        String.is_prefix issue_comment_body ~prefix:comment_header
      ) |!
      function
      |[] -> (* No comments from us, so create a new one *)
        let url = sprintf "http://www.realworldocaml.org/en/%s/%s#%s" milestone t.file id in
        let body = sprintf "%s [%s](%s)\n\nContext:\n\n%s" comment_header url url t.html in
        let _ = run_gh (Github.Issues.create_comment ~token ~user ~repo ~issue_number:i.issue_number ~body) in
        ()
      |_ -> eprintf "We have already commented on issue %s, so skipping it.\n" id
  )
    
let _ =
  let fin = Sys.argv.(1) in (* TODO cmdliner *)
  let milestone = Sys.argv.(2) in
  let ids = Sexp.load_sexp_conv_exn fin ts_of_sexp in
  let auth = Lwt_main.run (Github_cookie_jar.get "rwo") in
  match auth with
  |None -> failwith ("no 'rwo' git jar token found")
  |Some auth -> github_comments ids (Github.Token.of_auth auth) milestone
