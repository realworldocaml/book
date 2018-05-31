open Core
open Async
open Rwo
let (/) = Filename.concat


(******************************************************************************)
(* `build` command                                                            *)
(******************************************************************************)
let build_chapter : Command.t = 
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"build chapter"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      and production =
        let default = false in
        let doc = sprintf
            " Set to true to generate file for publication. Default \
             is %b, which generates dev version of file."
            default in
        flag "-production" (optional_with_default default bool) ~doc
      and chapter =
        let doc = "N Build HTML version of chapter N." in
        flag "-chapter" (required int) ~doc
      and out_dir =
        let default = "_build" in
        let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
        flag "-o" (optional_with_default default file) ~doc
      and code_dir =
        let default = "examples" in
        let doc = sprintf "DIR Directory with code examples. Default: \"%s\"" default in
        flag "-code" (optional_with_default default file) ~doc
      and pygmentize =
        flag "-pygmentize" no_arg
          ~doc:" Syntax highlight code with pygmentize. \
                By default, this is not done."
      and run_nondeterministic =
        flag "-run-nondeterministic" no_arg
          ~doc:" In topscripts, run code marked [%%expect.nondeterministic ...]. \
                By default, they are skipped."
      and file =
        anon ("file" %: file)
      in
      fun () ->
        Book.make ~code_dir ~run_nondeterministic ~pygmentize ~repo_root ~out_dir
          (`Chapter file)
    ]


let build_frontpage : Command.t = 
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"build frontpage"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      and out_dir =
        let default = "_build" in
        let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
        flag "-o" (optional_with_default default file) ~doc
      in 
      fun () -> Book.make ~repo_root ~out_dir `Frontpage
    ]

let build_toc_page : Command.t = 
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"build TOC page"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      and out_dir =
        let default = "_build" in
        let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
        flag "-o" (optional_with_default default file) ~doc
      in
      fun () ->
        Book.make ~repo_root ~out_dir `Toc_page
    ]

let build_faqs_page : Command.t =
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"build FAQs page"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      and out_dir =
        let default = "_build" in
        let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
        flag "-o" (optional_with_default default file) ~doc
      in
      fun () ->
        Book.make ~repo_root ~out_dir `FAQs
    ]

let build_install_page : Command.t =
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"build install page"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      and out_dir =
        let default = "_build" in
        let doc = sprintf "DIR Output directory. Default: \"%s\"" default in
        flag "-o" (optional_with_default default file) ~doc
      in
      fun () ->
        Book.make ~repo_root ~out_dir `Install
    ]

let build : Command.t = Command.group
    ~summary:"build commands"
    [
      "chapter", build_chapter;
      "frontpage", build_frontpage;
      "toc", build_toc_page;
      "faqs", build_faqs_page;
      "install", build_install_page;
    ]


(******************************************************************************)
(* `validate` command                                                         *)
(******************************************************************************)
let validate : Command.t =
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"validate various things"
    [%map_open
      let repo_root =
        let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc
      in
      (fun () ->
         let validate_code_files () =
           Toc.imported_files ~repo_root () >>= fun imported_files ->
           Toc.code_files ~repo_root () >>| fun code_files ->
           let imported_files = String.Set.of_list imported_files in
           let code_files = String.Set.of_list code_files in
           let diff = Set.diff code_files imported_files |> Set.to_list in
           if List.length diff <> 0 then
             Log.Global.error
               "following files are not used:%s"
               (List.map diff ~f:(fun x -> "\n  "^x) |> String.concat ~sep:"")
         in
         validate_code_files() >>= Deferred.return
      )]

(******************************************************************************)
(* `eval` command                                                             *)
(******************************************************************************)

let eval : Command.t =
  let open Async.Command.Let_syntax in
  Command.async
    ~summary:"evaluate a file into an sexp. This sexp can be used as a memoization cache in future runs."
    [%map_open
      let filename =
        anon ("file" %: file)
      in (fun () ->
          let lang = Rwo.Lang.of_filename filename |> Or_error.ok_exn in
          let run_nondeterministic = false in
          Rwo.Scripts.eval_script_to_sexp lang ~run_nondeterministic ~filename >>= fun sexp ->
          print_endline (Sexp.to_string_hum (Or_error.ok_exn sexp));
          Deferred.return ()
        )]

(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main = Command.group
    ~summary:"Real World OCaml authoring and publication tools"
    [
      "build", build;
      "validate", validate;
      "eval", eval
    ]

;;
let build_info = match About.git_commit with
  | None -> "unknown"
  | Some x -> x
in
try Command.run ~build_info main
with e -> eprintf "%s\n" (Exn.to_string e)
