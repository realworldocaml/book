open Core
open Async
open Rwo
let (/) = Filename.concat

module Params = struct
  open Command.Param

  let repo_root =
    let default = "./" in
    let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
    flag "-repo-root" (optional_with_default default string) ~doc

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
    flag "-o" (optional_with_default default string) ~doc

  let include_wip =
    let doc =
      "Whether to include WIP chapters to the generated rules, website or PDF"
    in
    flag "-include-wip" no_arg ~doc

  let file =
    anon ("file" %: string)

  let run_nondeterministic =
    flag "-run-nondeterministic" no_arg
      ~doc:" In .mlt files, run code marked [%%expect.nondeterministic ...]. \
            By default, they are skipped."

end


(******************************************************************************)
(* `build` command                                                            *)
(******************************************************************************)
open Command.Let_syntax

let build_chapter : Command.t =
  Command.async ~summary:"build chapter"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      and file = Params.file
      in
      fun () ->
        Book.make ~repo_root ~out_dir (`Chapter file) ]

let build_frontpage : Command.t =
  Command.async ~summary:"build frontpage"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      and include_wip = Params.include_wip
      in fun () -> Book.make ~repo_root ~include_wip ~out_dir `Frontpage ]

let build_toc_page : Command.t =
  Command.async ~summary:"build TOC page"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      and include_wip = Params.include_wip
      in fun () -> Book.make ~repo_root ~include_wip ~out_dir `Toc_page ]

let build_faqs_page : Command.t =
  Command.async ~summary:"build FAQs page"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      in fun () -> Book.make ~repo_root ~out_dir `FAQs ]

let build_install_page : Command.t =
  Command.async ~summary:"build install page"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      in fun () -> Book.make ~repo_root ~out_dir `Install ]

let build_tex_inputs_page : Command.t =
  Command.async ~summary:"build content page"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      and include_wip = Params.include_wip
      in fun () -> Book.make ~repo_root ~include_wip ~out_dir `Latex ]

let build : Command.t =
  Command.group ~summary:"build commands"
    [ "chapter", build_chapter
    ; "index", build_frontpage
    ; "toc", build_toc_page
    ; "faqs", build_faqs_page
    ; "install", build_install_page
    ; "inputs", build_tex_inputs_page
    ]

let rules_web : Command.t =
  Command.async ~summary:"generate dune rules for website generation"
    [%map_open
      let repo_root = Params.repo_root
      and include_wip = Params.include_wip
      in
      fun () -> Rules.print_web ~include_wip ~repo_root ]

let rules : Command.t =
  Command.group ~summary:"generate dune rules"
    [ "web", rules_web
    ]

(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main =
  Command.group
    ~summary:"Real World OCaml authoring and publication tools"
    [ "build", build
    ; "rules", rules
    ]

let () =
  let build_info =
    match About.git_commit with
    | None -> "unknown"
    | Some x -> x
  in
  Command.run ~build_info main
