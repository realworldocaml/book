open Core.Std
open Async.Std
open Sexplib
open Rwo

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

end


(******************************************************************************)
(* `build` command                                                            *)
(******************************************************************************)
let build_chapter : Command.t = Command.async
  ~summary:"build chapter"
  Command.Spec.(
    empty
    +> Param.repo_root
    +> Param.out_dir
    +> Param.file
  )
  (fun repo_root out_dir file () ->
    Book.make ~repo_root ~out_dir (`Chapter file)
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

let build : Command.t = Command.group
  ~summary:"build commands"
  [
    "chapter", build_chapter;
    "frontpage", build_frontpage;
  ]


(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main = Command.group
  ~summary:"Real World OCaml authoring and publication tools"
  [
    "build", build;
  ]

;;
let build_info = match About.git_commit with
  | None -> "unknown"
  | Some x -> x
in
try Command.run ~build_info main
with e -> eprintf "%s\n" (Exn.to_string e)
