open Core
open Async
open Rwo
let (/) = Filename.concat

module Params = struct
  open Command.Param

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

  let code_dir =
    let default = "examples" in
    let doc =
      sprintf "DIR Directory with code examples. Default: \"%s\""
        default
    in
    flag "-code" (optional_with_default default file) ~doc

  let file =
    anon ("file" %: file)

  let pygmentize =
    flag "-pygmentize" no_arg
      ~doc:" Syntax highlight code with pygmentize. \
            By default, this is not done."

  let run_nondeterministic =
    flag "-run-nondeterministic" no_arg
      ~doc:" In topscripts, run code marked [%%expect.nondeterministic ...]. \
            By default, they are skipped."

end


(******************************************************************************)
(* `build` command                                                            *)
(******************************************************************************)
open Command.Let_syntax

let build_chapter : Command.t =
  Command.async ~summary:"build chapter"
    [%map_open
      let pygmentize = Params.pygmentize
      and repo_root = Params.repo_root
      and code_dir = Params.code_dir
      and out_dir = Params.out_dir
      and file = Params.file
      in
      fun () ->
        Book.make ~code_dir ~pygmentize
          ~repo_root ~out_dir (`Chapter file) ]

let build_frontpage : Command.t =
  Command.async ~summary:"build frontpage"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      in fun () -> Book.make ~repo_root ~out_dir `Frontpage ]

let build_toc_page : Command.t =
  Command.async ~summary:"build TOC page"
    [%map_open
      let repo_root = Params.repo_root
      and out_dir = Params.out_dir
      in fun () -> Book.make ~repo_root ~out_dir `Toc_page ]

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

let build : Command.t =
  Command.group ~summary:"build commands"
    [ "chapter", build_chapter
    ; "index", build_frontpage
    ; "toc", build_toc_page
    ; "faqs", build_faqs_page
    ; "install", build_install_page
    ]


(******************************************************************************)
(* `validate` command                                                         *)
(******************************************************************************)
let validate : Command.t =
  Command.async ~summary:"validate various things"
    [%map_open
      let repo_root = Params.repo_root in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind imported_files = Toc.imported_files ~repo_root () in
        let%map code_files = Toc.code_files ~repo_root () in
        let imported_files = String.Set.of_list imported_files in
        let code_files = String.Set.of_list code_files in
        let diff = Set.diff code_files imported_files |> Set.to_list in
        if List.length diff <> 0 then
          Log.Global.error
            "following files are not used:%s"
            (List.map diff ~f:(fun x -> "\n  "^x) |> String.concat ~sep:"")
    ]

(******************************************************************************)
(* `eval` command                                                             *)
(******************************************************************************)

let eval : Command.t =
  Command.async
    ~summary:"Evaluate a file into a sexp, to serve as a cache."
    [%map_open
      let filename = Params.file in
      fun () ->
        let open Deferred.Let_syntax in
        let lang = Rwo.Lang.of_filename filename |> Or_error.ok_exn in
        let%map sexp = Rwo.Scripts.eval_script_to_sexp lang ~filename in
        print_endline (Sexp.to_string_hum (Or_error.ok_exn sexp))]

(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main =
  Command.group
    ~summary:"Real World OCaml authoring and publication tools"
    [ "build", build
    ; "validate", validate
    ; "eval", eval
    ]

let () =
  let build_info =
    match About.git_commit with
    | None -> "unknown"
    | Some x -> x
  in
  Command.run ~build_info main
