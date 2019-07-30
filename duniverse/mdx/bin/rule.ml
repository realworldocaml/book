(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

open Astring

let src = Logs.Src.create "cram.rule"

module Log = (val Logs.src_log src : Logs.LOG)

let prelude_file f =
  match String.cut ~sep:":" f with
  | None -> f
  | Some (_, f) -> f

let prepend_root r b = match r with
  | None   -> b
  | Some r -> Filename.concat r b

let findlib_conf_gen_rule =
  let target = "findlib.conf" in
  let action =
    {|(write-file %{target} "path=\"%{project_root}/_build/install/%{context_name}/lib\"")|}
  in
  Printf.sprintf
    ("(rule\n"
     ^^ " (target %s)\n"
     ^^ " (action %s))\n")
    target action

let print_rule ~nd ~prelude ~md_file ~ml_files ~dirs ~root ~requires ~duniverse_mode options =
  let ml_files = String.Set.elements ml_files in
  let ml_files = List.map (prepend_root root) ml_files in
  let dirs = match root with
    | None      -> String.Set.elements dirs
    | Some root ->
      (* only keep absolute dirs *)
      let dirs = String.Set.filter (fun x -> not (Filename.is_relative x)) dirs in
      let dirs = String.Set.add root dirs in
      String.Set.elements dirs
  in
  let var_names =
    let f (cpt, acc) _ = cpt + 1, ("y" ^ string_of_int cpt) :: acc in
    List.fold_left f (0, []) ml_files |> snd
  in
  let requires = String.Set.elements requires in
  let pp_package_deps fmt name =
    Fmt.pf fmt "\         (package %s)" name
  in
  let pp_ml_deps fmt (var_name, ml_file) =
    Fmt.pf fmt "\         (:%s %s)" var_name ml_file
  in
  let pp_dir_deps fmt dir =
    Fmt.pf fmt "\         (source_tree %s)" dir
  in
  let pp_ml_diff fmt var =
    Fmt.pf fmt "\           (diff? %%{%s} %%{%s}.corrected)" var var
  in
  let prelude =
    let files = String.Set.of_list (List.map prelude_file prelude) in
    String.Set.elements files
    |> List.map (fun f -> Fmt.strf "         %s" f)
  in
  let root = match root with None -> "" | Some r -> Fmt.strf "--root=%s " r in
  let deps =
    let require_deps = if duniverse_mode then requires else [] in
    let x =
      List.map (Fmt.to_to_string pp_package_deps) require_deps @
      List.map (Fmt.to_to_string pp_ml_deps) (List.combine var_names ml_files) @
      List.map (Fmt.to_to_string pp_dir_deps) dirs @
      prelude
    in
    match x with
    | [] -> ""
    | s  -> String.concat ~sep:"\n" ("" :: s)
  in
  let run_action arg =
    let run =
      Fmt.strf "(run ocaml-mdx test %a %s%s%%{x})"
        Fmt.(list ~sep:(unit " ") string) options
        arg root
    in
    if duniverse_mode then
      Fmt.strf "(setenv OCAMLFIND_CONF %%{conf} %s)" run
    else
      run
  in
  let conf_dep = if duniverse_mode then "\n         (:conf findlib.conf)" else "" in
  let pp name arg =
    Fmt.pr
      "\
(alias\n\
\ (name   %s)\n\
\ (deps   (:x %s)%s\n\
\         (package mdx)%s)\n\
\ (action (progn\n\
\           %s\n%a\n\
\           (diff? %%{x} %%{x}.corrected))))\n"
      name
      md_file
      conf_dep
      deps
      (run_action arg)
      (Fmt.list ~sep:Fmt.cut pp_ml_diff) var_names
  in
  pp "runtest" "";
  if nd then pp "runtest-all" "--non-deterministic "

let pp_direction fmt = function
  | `Infer_timestamp -> Fmt.pf fmt "--direction=infer-timestamp"
  | `To_md -> Fmt.pf fmt "--direction=to-md"
  | `To_ml -> Fmt.pf fmt "--direction=to-ml"

let pp_prelude fmt s = Fmt.pf fmt "--prelude=%s" s
let pp_prelude_str fmt s = Fmt.pf fmt "--prelude-str=%S" s

let add_opt e s = match e with None -> s | Some e -> String.Set.add e s

let aggregate_requires ~require_from l =
  let open Rresult.R.Infix in
  Mdx.Util.Result.List.fold
    ~f:(fun acc x -> require_from x >>| Mdx.Library.Set.union acc)
    ~init:Mdx.Library.Set.empty
    l

let requires_from_prelude_str prelude_str_list =
  let require_from prelude_str =
    let (_, prelude) = Mdx.Prelude.env_and_file prelude_str in
    let lines = String.fields ~is_sep:(function '\n' | '\r' -> true | _ -> false) prelude in
    Mdx.Block.require_from_lines lines
  in
  aggregate_requires ~require_from prelude_str_list

let requires_from_prelude prelude_list =
  let require_from prelude =
    let (_, prelude) = Mdx.Prelude.env_and_file prelude in
    let lines = Mdx.Util.File.read_lines prelude in
    Mdx.Block.require_from_lines lines
  in
  aggregate_requires ~require_from prelude_list

let requires_to_packages library_set =
  Mdx.Library.Set.fold
    (fun {base_name; _} acc -> String.Set.add base_name acc)
    library_set
    String.Set.empty

let run () md_file section direction prelude prelude_str root duniverse_mode =
  let open Rresult.R.Infix in
  let section = match section with
    | None   -> None
    | Some p -> Some (Re.Perl.compile_pat p)
  in
  let active b = match section, Mdx.Block.section b with
    | None   , _      -> true
    | Some re, None   -> Re.execp re ""
    | Some re, Some s -> Re.execp re (snd s)
  in
  let on_item acc = function
    | Mdx.Section _ | Text _ -> Ok acc
    | Block b when active b ->
      let files, dirs, nd, requires = acc in
      let nd = nd || match Mdx.Block.mode b with
        | `Non_det _ -> true
        | _          -> false
      in
      let source_trees = String.Set.of_list (Mdx.Block.source_trees b) in
      let dirs =
        dirs
        |> add_opt (Mdx.Block.directory b)
        |> String.Set.union source_trees
      in
      let files = add_opt (Mdx.Block.file b) files in
      Mdx.Block.required_libraries b >>| fun libs ->
      let requires = Mdx.Library.Set.union requires libs in
      files, dirs, nd, requires
    | Block _ -> Ok acc
  in
  let on_file file_contents items =
    let empty = String.Set.empty in
    let req_res =
      requires_from_prelude prelude >>= fun prelude_requires ->
      requires_from_prelude_str prelude_str >>= fun prelude_str_requires ->
      let requires = Mdx.Library.Set.union prelude_requires prelude_str_requires in
      Mdx.Util.Result.List.fold ~f:on_item ~init:(empty, empty, false, requires) items
    in
    match req_res with
    | Error s ->
      Printf.eprintf "Fatal error while parsing block: %s" s;
      exit 1
    | Ok (ml_files, dirs, nd, requires) ->
      let options =
        List.map (Fmt.to_to_string pp_prelude) prelude @
        List.map (Fmt.to_to_string pp_prelude_str) prelude_str @
        [Fmt.to_to_string pp_direction direction]
      in
      let requires = requires_to_packages requires in
      if duniverse_mode then
        ( Fmt.pr "%s" findlib_conf_gen_rule;
          Fmt.pr "\n"
        );
      print_rule ~md_file ~prelude ~nd ~ml_files ~dirs ~root ~requires
        ~duniverse_mode options;
      file_contents
  in
  Mdx.run md_file ~f:on_file;
  0

open Cmdliner

let duniverse_mode =
  let doc =
    "Run mdx in a duniverse-compatible mode. \
     Expect all toplevel dependencies to be available in your duniverse folder."
  in
  Arg.(value & flag & info ["duniverse-mode"] ~doc)

let cmd =
  let doc = "Produce dune rules to synchronize markdown and OCaml files." in
  Term.(pure run
        $ Cli.setup $ Cli.file $ Cli.section $ Cli.direction
        $ Cli.prelude $ Cli.prelude_str $ Cli.root $ duniverse_mode),
  Term.info "rule" ~doc
