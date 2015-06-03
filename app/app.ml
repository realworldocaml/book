open Core.Std
open Async.Std
open Sexplib
open Rwo
let (/) = Filename.concat

module Param = struct
  open Command.Spec

  let repo_root =
    let default = "./" in
    let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
    flag "-repo-root" (optional_with_default default file) ~doc

  let production =
    let default = false in
    let doc = sprintf
      " Set to true to generate file for publication. Default \
        is %b, which generates dev version of file."
      default
    in
    flag "-production" (optional_with_default default bool) ~doc

  let chapter =
    let doc = "N Build HTML version of chapter N." in
    flag "-chapter" (required int) ~doc

  let out_dir =
    let default = "_build" in
    let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
    flag "-o" (optional_with_default default file) ~doc

  let file =
    anon ("file" %: file)

  let pygmentize =
    flag "-pygmentize" no_arg
      ~doc:" Syntax highlight code with pygmentize. \
             By default, this is not done."

end


(******************************************************************************)
(* `build` command                                                            *)
(******************************************************************************)
let build_chapter : Command.t = Command.async
  ~summary:"build chapter"
  Command.Spec.(
    empty
    +> Param.pygmentize
    +> Param.repo_root
    +> Param.out_dir
    +> Param.file
  )
  (fun pygmentize repo_root out_dir file () ->
    Book.make ~pygmentize ~repo_root ~out_dir (`Chapter file)
  )

let build_frontpage : Command.t = Command.async
  ~summary:"build frontpage"
  Command.Spec.(
    empty
    +> Param.repo_root
    +> Param.out_dir
  )
  (fun repo_root out_dir () ->
    Book.make ~repo_root ~out_dir `Frontpage
  )

let build_toc_page : Command.t = Command.async
  ~summary:"build TOC page"
  Command.Spec.(
    empty
    +> Param.repo_root
    +> Param.out_dir
  )
  (fun repo_root out_dir () ->
    Book.make ~repo_root ~out_dir `Toc_page
  )

let build_faqs_page : Command.t = Command.async
  ~summary:"build FAQs page"
  Command.Spec.(
    empty
    +> Param.repo_root
    +> Param.out_dir
  )
  (fun repo_root out_dir () ->
    Book.make ~repo_root ~out_dir `FAQs
  )

let build_install_page : Command.t = Command.async
  ~summary:"build install page"
  Command.Spec.(
    empty
    +> Param.repo_root
    +> Param.out_dir
  )
  (fun repo_root out_dir () ->
    Book.make ~repo_root ~out_dir `Install
  )

let build : Command.t = Command.group
  ~summary:"build commands"
  [
    "chapter", build_chapter;
    "frontpage", build_frontpage;
    "toc", build_toc_page;
    "faqs", build_faqs_page;
    "install",build_install_page;
  ]


(******************************************************************************)
(* `validate` command                                                         *)
(******************************************************************************)
let validate : Command.t = Command.async
  ~summary:"validate various things"
  Command.Spec.(empty +> Param.repo_root)
  (fun repo_root () ->
   let validate_chapter (x:Toc.chapter) : unit Deferred.t =
     Html.of_file ("book"/x.Toc.filename) >>| fun html ->
     Import.find_all html |>
     List.iter ~f:(fun i ->
       match Filename.split_extension i.Import.href |> snd with
       | None -> Log.Global.error "%s missing file extension" i.Import.href
       | Some x ->
	  let valid_extensions =
            Lang.to_extensions i.Import.data_code_language
	  in
          if not (List.mem valid_extensions x) then
	    Log.Global.error
	      "invalid extension %s for language %s"
	      x
	      (Lang.sexp_of_t i.Import.data_code_language |> Sexp.to_string_hum)
     )
   in
   Toc.get_chapters ~repo_root () >>=
   Deferred.List.iter ~f:validate_chapter
  )

(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main = Command.group
  ~summary:"Real World OCaml authoring and publication tools"
  [
    "build", build;
    "validate", validate;
  ]

;;
let build_info = match About.git_commit with
  | None -> "unknown"
  | Some x -> x
in
try Command.run ~build_info main
with e -> eprintf "%s\n" (Exn.to_string e)
