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
open Result

let src = Logs.Src.create "cram.rule"

module Log = (val Logs.src_log src : Logs.LOG)

let prelude_file f =
  match String.cut ~sep:":" f with None -> f | Some (_, f) -> f

let prepend_root r b = match r with None -> b | Some r -> Filename.concat r b

let pp_dep fmt = function
  | `Named (name, path) -> Fmt.pf fmt "(:%s %s)" name path
  | `Source_tree path -> Fmt.pf fmt "(source_tree %s)" path
  | `Package p -> Fmt.pf fmt "(package %s)" p
  | `Path path -> Fmt.pf fmt "%s" path

let pp_action fmt = function
  | `Diff_corrected var -> Fmt.pf fmt "(diff? %%{%s} %%{%s}.corrected)" var var
  | `Run args -> Fmt.pf fmt "(run @[<hov 2>%a@])" Fmt.(list ~sep:sp string) args

let pp_locks_field fmt dirs_and_files =
  match dirs_and_files with
  | [] -> ()
  | locks ->
      Fmt.pf fmt " (locks @[%a@])\n" Fmt.(list ~sep:(unit "@\n") string) locks

let pp_rules ~nd ~prelude ~md_file ~ml_files ~dirs ~root ~packages ~locks fmt
    options =
  let ml_files = List.map (prepend_root root) (String.Set.elements ml_files) in
  let dirs =
    match root with
    | None -> String.Set.elements dirs
    | Some root ->
        (* only keep absolute dirs *)
        let dirs =
          String.Set.filter (fun x -> not (Filename.is_relative x)) dirs
        in
        let dirs = String.Set.add root dirs in
        String.Set.elements dirs
  in
  let prelude_files =
    let files = String.Set.of_list (List.map prelude_file prelude) in
    String.Set.elements files
  in
  let var_names =
    let f (cpt, acc) _ = (cpt + 1, ("y" ^ string_of_int cpt) :: acc) in
    List.fold_left f (0, []) ml_files |> snd
  in
  let root = match root with None -> [] | Some r -> [ "--root=" ^ r ] in
  let deps =
    let packages = List.map (fun p -> `Package p) (String.Set.elements packages)
    and ml_files = List.map2 (fun name p -> `Named (name, p)) var_names ml_files
    and dirs = List.map (fun p -> `Source_tree p) dirs
    and prelude = List.map (fun p -> `Path p) prelude_files in
    (`Named ("x", md_file) :: packages) @ ml_files @ dirs @ prelude
  in
  let actions arg =
    `Run (("ocaml-mdx" :: "test" :: options) @ arg @ root @ [ "%{x}" ])
    :: `Diff_corrected "x"
    :: List.map (fun v -> `Diff_corrected v) var_names
  in
  let pp fmt name arg =
    Fmt.pf fmt
      "(alias@\n\
      \ (name   %s)@\n\
      \ (deps   @[<v>%a@])@\n\
       %a (action @[<hv 2>(progn@ %a)@]))@\n"
      name
      Fmt.(list ~sep:sp pp_dep)
      deps pp_locks_field locks
      Fmt.(list ~sep:cut pp_action)
      (actions arg)
  in
  pp fmt "runtest" [];
  if nd then pp fmt "runtest-all" [ "--non-deterministic" ]

let print_format_dune_rules pp_rules =
  let open Unix in
  let read_pipe, write_pipe = pipe () in
  set_close_on_exec read_pipe;
  set_close_on_exec write_pipe;
  let dune_file_format =
    create_process "dune"
      [| "dune"; "format-dune-file" |]
      read_pipe stdout stderr
  in
  close read_pipe;
  let oc = out_channel_of_descr write_pipe in
  let fmt = Format.formatter_of_out_channel oc in
  pp_rules fmt ();
  close_out oc;
  match waitpid [] dune_file_format with
  | _, WEXITED 0 -> ()
  | _, WEXITED _ ->
      let msg =
        Format.asprintf
          "Failed to format the following rules with 'dune format-dune-file':\n\
           %a"
          pp_rules ()
      in
      failwith msg
  | _, (WSIGNALED _ | WSTOPPED _) ->
      failwith "Child process 'dune format-dune-file' was interrupted"

let options_of_syntax = function
  | Some Mdx.Normal -> [ "--syntax=normal" ]
  | Some Mdx.Cram -> [ "--syntax=cram" ]
  | Some Mdx.Mli -> [ "--syntax=mli" ]
  | None -> []

let options_of_section = function
  | Some s -> [ Fmt.to_to_string (Fmt.fmt "--section %S") s ]
  | None -> []

let pp_prelude fmt s = Fmt.pf fmt "--prelude=%s" s

let pp_prelude_str fmt s = Fmt.pf fmt "--prelude-str %S" s

let add_opt e s = match e with None -> s | Some e -> String.Set.add e s

let aggregate_requires ~require_from l =
  let open Mdx.Util.Result.Infix in
  Mdx.Util.Result.List.fold
    ~f:(fun acc x -> require_from x >>| Mdx.Library.Set.union acc)
    ~init:Mdx.Library.Set.empty l
  >>| Mdx.Library.Set.to_package_set

let requires_from_prelude_str prelude_str_list =
  let require_from prelude_str =
    let _, prelude = Mdx.Prelude.env_and_file prelude_str in
    let lines =
      String.fields
        ~is_sep:(function '\n' | '\r' -> true | _ -> false)
        prelude
    in
    Mdx.Block.require_from_lines lines
  in
  aggregate_requires ~require_from prelude_str_list

let requires_from_prelude prelude_list =
  let require_from prelude =
    let _, prelude = Mdx.Prelude.env_and_file prelude in
    let lines = Mdx.Util.File.read_lines prelude in
    Mdx.Block.require_from_lines lines
  in
  aggregate_requires ~require_from prelude_list

let run (`Setup ()) (`File md_file) (`Section section) (`Syntax syntax)
    (`Prelude prelude) (`Prelude_str prelude_str) (`Root root)
    (`Duniverse_mode duniverse_mode) (`Locks locks) =
  let open Mdx.Util.Result.Infix in
  let active =
    let section =
      match section with None -> None | Some p -> Some (Re.Perl.compile_pat p)
    in
    fun b ->
      match (section, Mdx.Block.section b) with
      | None, _ -> true
      | Some re, None -> Re.execp re ""
      | Some re, Some s -> Re.execp re (snd s)
  in
  let on_item acc = function
    | Mdx.Document.Section _ | Text _ -> Ok acc
    | Block b when active b ->
        let files, dirs, nd, packages = acc in
        let nd =
          nd || match Mdx.Block.non_det b with Some _ -> true | None -> false
        in
        let source_trees = String.Set.of_list (Mdx.Block.source_trees b) in
        let dirs =
          dirs
          |> add_opt (Mdx.Block.directory b)
          |> String.Set.union source_trees
        in
        let files = add_opt (Mdx.Block.file b) files in
        let explicit_requires =
          String.Set.of_list (Mdx.Block.explicit_required_packages b)
        in
        let requires_from_statement =
          if duniverse_mode then
            Mdx.Block.required_libraries b >>| Mdx.Library.Set.to_package_set
          else Ok String.Set.empty
        in
        requires_from_statement >>| fun requires_from_statement ->
        let ( ++ ) = String.Set.union in
        let packages =
          packages ++ explicit_requires ++ requires_from_statement
        in
        (files, dirs, nd, packages)
    | Block _ -> Ok acc
  in
  let on_file file_contents items =
    let empty = String.Set.empty in
    let req_res =
      let packages =
        if duniverse_mode then
          requires_from_prelude prelude >>= fun prelude_requires ->
          requires_from_prelude_str prelude_str >>= fun prelude_str_requires ->
          Ok (String.Set.union prelude_requires prelude_str_requires)
        else Ok String.Set.empty
      in
      packages >>= fun packages ->
      Mdx.Util.Result.List.fold ~f:on_item
        ~init:(empty, empty, false, packages)
        items
    in
    match req_res with
    | Error s ->
        Printf.eprintf "[mdx] Fatal error while parsing block: %s\n" s;
        exit 1
    | Ok (ml_files, dirs, nd, packages) ->
        let packages =
          if duniverse_mode then String.Set.add "mdx" packages else packages
        in
        let options =
          List.map (Fmt.to_to_string pp_prelude) prelude
          @ List.map (Fmt.to_to_string pp_prelude_str) prelude_str
          @ options_of_syntax syntax @ options_of_section section
        in
        let pp_rules fmt () =
          pp_rules ~md_file ~prelude ~nd ~ml_files ~dirs ~root ~packages ~locks
            fmt options
        in
        print_format_dune_rules pp_rules;
        file_contents
  in
  Mdx.Deprecated.warn "ocaml-mdx rule" ~since:"1.7.0"
    ~replacement:"the mdx stanza";
  Mdx.run ?syntax md_file ~f:on_file >>! fun () -> 0

open Cmdliner

let duniverse_mode =
  let doc =
    "Run mdx in a duniverse-compatible mode. Expect all toplevel dependencies \
     to be available in your duniverse folder."
  in
  Cli.named
    (fun x -> `Duniverse_mode x)
    Arg.(value & flag & info [ "duniverse-mode" ] ~doc)

let locks =
  let docv = "LOCK[,LOCKS]" in
  let doc =
    "Explicitly specify a list of locks to add to the generated dune rule"
  in
  Cli.named
    (fun x -> `Locks x)
    Arg.(value & opt (list ~sep:',' string) [] & info [ "locks" ] ~doc ~docv)

let cmd =
  let doc =
    "Produce dune rules to synchronize markdown and OCaml files. $(b,ocaml-mdx \
     rule) is deprecated since 1.7.0 and will be removed in 2.0.0. Please use \
     the mdx stanza instead."
  in
  ( Term.(
      pure run $ Cli.setup $ Cli.file $ Cli.section $ Cli.syntax $ Cli.prelude
      $ Cli.prelude_str $ Cli.root $ duniverse_mode $ locks),
    Term.info "rule" ~doc )
