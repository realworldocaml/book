open Import
open Dune_file
open Memo.O

let ( ++ ) = Path.Build.relative

let find_project_by_key =
  let memo =
    let make_map projects =
      Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
          (Dune_project.file_key project, project))
      |> Memo.return
    in
    let module Input = struct
      type t = Dune_project.t list

      let equal = List.equal Dune_project.equal

      let hash = List.hash Dune_project.hash

      let to_dyn = Dyn.list Dune_project.to_dyn
    end in
    Memo.create "project-by-keys" ~input:(module Input) make_map
  in
  fun key ->
    let* { projects; _ } = Dune_load.load () in
    let+ map = Memo.exec memo projects in
    Dune_project.File_key.Map.find_exn map key

module Scope_key : sig
  val of_string : Context.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t

  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      (Lib_name.parse_string_exn (Loc.none, s), public_libs)
    | Some (lib, key) ->
      let+ scope =
        let key = Dune_project.File_key.of_string key in
        find_project_by_key key >>= Scope.DB.find_by_project context
      in
      (Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope)

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib)
      (Dune_project.File_key.to_string key)
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project

let pkg_or_lnu lib =
  match Lib_info.package (Lib.info lib) with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t

type source =
  | Module
  | Mld

type odoc_artefact =
  { odoc_file : Path.Build.t
  ; odocl_file : Path.Build.t
  ; html_dir : Path.Build.t
  ; html_file : Path.Build.t
  ; source : source  (** source of the [odoc_file], either module or mld *)
  }

let add_rule sctx =
  let dir = (Super_context.context sctx).build_dir in
  Super_context.add_rule sctx ~dir

module Paths = struct
  let root (context : Context.t) =
    Path.Build.relative context.Context.build_dir "_doc"

  let odocs ctx = function
    | Lib lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      Obj_dir.odoc_dir obj_dir
    | Pkg pkg -> root ctx ++ sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)

  let html_root ctx = root ctx ++ "_html"

  let odocl_root ctx = root ctx ++ "_odocls"

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)

  let html ctx m = add_pkg_lnu (html_root ctx) m

  let odocl ctx m = add_pkg_lnu (odocl_root ctx) m

  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg

  let css_file ctx = html_root ctx ++ "odoc.css"

  let highlight_pack_js ctx = html_root ctx ++ "highlight.pack.js"

  let toplevel_index ctx = html_root ctx ++ "index.html"
end

module Dep : sig
  (** [html_alias ctx target] returns the alias that depends on all html targets
      produced by odoc for [target] *)
  val html_alias : Context.t -> target -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps :
       Context.t
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> target -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias ctx m = Alias.doc ~dir:(Paths.html ctx m)

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p ->
           Dep.Set.singleton (Dep.alias (alias ~dir:(Paths.odocs ctx (Pkg p))))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
           match Lib.Local.of_lib lib with
           | None -> acc
           | Some lib ->
             let dir = Paths.odocs ctx (Lib lib) in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias)))

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
end

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.Build.t -> t

  val odoc_file : doc_dir:Path.Build.t -> t -> Path.Build.t

  val odoc_input : t -> Path.Build.t
end = struct
  type t = Path.Build.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.chop_extension (Path.Build.basename t) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input t = t
end

let odoc_base_flags sctx build_dir =
  let open Memo.O in
  let+ conf = Super_context.odoc sctx ~dir:build_dir in
  match conf.Env_node.Odoc.warnings with
  | Fatal -> Command.Args.A "--warn-error"
  | Nonfatal -> S []

let run_odoc sctx ~dir command ~flags_for args =
  let build_dir = (Super_context.context sctx).build_dir in
  let open Memo.O in
  let* program =
    Super_context.resolve_program sctx ~dir:build_dir "odoc" ~loc:None
      ~hint:"opam install odoc"
  in
  let+ base_flags =
    match flags_for with
    | None -> Memo.return Command.Args.empty
    | Some path -> odoc_base_flags sctx path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run ~dir program [ A command; base_flags; S args ]

let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
    let+ deps =
      if Module.has m ~ml_kind:Intf then Dep_graph.deps_of dep_graphs.intf m
      else
        (* When a module has no .mli, use the dependencies for the .ml *)
        Dep_graph.deps_of dep_graphs.impl m
    in
    List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))

let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags)
    ~dep_graphs ~pkg_or_lnu =
  let odoc_file = Obj_dir.Module.odoc obj_dir m in
  let open Memo.O in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.build (Obj_dir.odoc_dir obj_dir) in
      let+ run_odoc =
        run_odoc sctx ~dir:doc_dir "compile" ~flags_for:(Some odoc_file)
          [ A "-I"
          ; Path doc_dir
          ; iflags
          ; As [ "--pkg"; pkg_or_lnu ]
          ; A "-o"
          ; Target odoc_file
          ; Dep
              (Path.build
                 (Obj_dir.Module.cmti_file ~cm_kind:(Ocaml Cmi) obj_dir m))
          ]
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  (m, odoc_file)

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let open Memo.O in
  let odoc_file = Mld.odoc_file m ~doc_dir in
  let odoc_input = Mld.odoc_input m in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build doc_dir) "compile"
      ~flags_for:(Some odoc_input)
      [ Command.Args.dyn includes
      ; As [ "--pkg"; Package.Name.to_string pkg ]
      ; A "-o"
      ; Target odoc_file
      ; Dep (Path.build odoc_input)
      ]
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file

let odoc_include_flags ctx pkg requires =
  Resolve.args
    (let open Resolve.O in
    let+ libs = requires in
    let paths =
      List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
          match Lib.Local.of_lib lib with
          | None -> paths
          | Some lib ->
            Path.Set.add paths (Path.build (Paths.odocs ctx (Lib lib))))
    in
    let paths =
      match pkg with
      | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
      | None -> paths
    in
    Command.Args.S
      (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
           [ Command.Args.A "-I"; Path dir ])))

let link_odoc_rules sctx (odoc_file : odoc_artefact) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx pkg requires in
  let open Memo.O in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "link" ~flags_for:(Some odoc_file.odoc_file)
      [ odoc_include_flags ctx pkg requires
      ; A "-o"
      ; Target odoc_file.odocl_file
      ; Dep (Path.build odoc_file.odoc_file)
      ]
  in
  add_rule sctx
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps >>> run_odoc)

let setup_library_odoc_rules cctx (local_lib : Lib.Local.t) =
  let open Memo.O in
  (* Using the proper package name doesn't actually work since odoc assumes that
     a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu (Lib.Local.to_lib local_lib) in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let* requires = Compilation_context.requires_compile cctx in
  let info = Lib.Local.info local_lib in
  let package = Lib_info.package info in
  let odoc_include_flags =
    Command.Args.memo (odoc_include_flags ctx package requires)
  in
  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let includes = (Dep.deps ctx package requires, odoc_include_flags) in
  let modules_and_odoc_files =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        let compiled =
          compile_module sctx ~includes
            ~dep_graphs:(Compilation_context.dep_graphs cctx)
            ~obj_dir ~pkg_or_lnu m
        in
        compiled :: acc)
  in
  let* modules_and_odoc_files = Memo.all_concurrently modules_and_odoc_files in
  Dep.setup_deps ctx (Lib local_lib)
    (Path.Set.of_list_map modules_and_odoc_files ~f:(fun (_, p) -> Path.build p))

let setup_html sctx (odoc_file : odoc_artefact) =
  let ctx = Super_context.context sctx in
  let to_remove, dummy =
    match odoc_file.source with
    | Mld -> (odoc_file.html_file, [])
    | Module ->
      (* Dummy target so that the below rule as at least one target. We do this
         because we don't know the targets of odoc in this case. The proper way
         to support this would be to have directory targets. *)
      let dummy = Action_builder.create_file (odoc_file.html_dir ++ ".dummy") in
      (odoc_file.html_dir, [ dummy ])
  in
  let open Memo.O in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "html-generate" ~flags_for:None
      [ A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; Dep (Path.build odoc_file.odocl_file)
      ; Hidden_targets [ odoc_file.html_file ]
      ]
  in
  add_rule sctx
    (Action_builder.progn
       (Action_builder.with_no_targets
          (Action_builder.return
             (Action.Full.make
                (Action.Progn
                   [ Action.Remove_tree to_remove
                   ; Action.Mkdir odoc_file.html_dir
                   ])))
       :: run_odoc :: dummy))

let setup_css_rule sctx =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build ctx.build_dir) "support-files"
      ~flags_for:None
      [ A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; Hidden_targets [ Paths.css_file ctx; Paths.highlight_pack_js ctx ]
      ]
  in
  add_rule sctx run_odoc

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let* list_items =
    let+ packages = Only_packages.get () in
    Package.Name.Map.to_list packages
    |> List.filter_map ~f:(fun (name, pkg) ->
           let name = Package.Name.to_string name in
           let link = sp {|<a href="%s/index.html">%s</a>|} name name in
           let version_suffix =
             match pkg.Package.version with
             | None -> ""
             | Some v -> sp {| <span class="version">%s</span>|} v
           in
           Some (sp "<li>%s%s</li>" link version_suffix))
    |> String.concat ~sep:"\n      "
  in
  let html =
    sp
      {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <main class="content">
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      %s
      </ol>
      </div>
    </main>
  </body>
</html>|}
      list_items
  in
  let ctx = Super_context.context sctx in
  add_rule sctx (Action_builder.write_file (Paths.toplevel_index ctx) html)

let libs_of_pkg ctx ~pkg =
  let+ entries = Scope.DB.lib_entries_of_package ctx pkg in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map entries ~f:(fun (entry : Scope.DB.Lib_entry.t) ->
      match entry with
      | Library lib ->
        let is_impl =
          Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements
          |> Option.is_some
        in
        Option.some_if (not is_impl) lib
      | Deprecated_library_name _ -> None)

let load_all_odoc_rules_pkg sctx ~pkg =
  let* pkg_libs = libs_of_pkg sctx ~pkg in
  let+ () =
    Memo.parallel_iter
      (Pkg pkg :: List.map pkg_libs ~f:(fun lib -> Lib lib))
      ~f:(fun _ -> Memo.return ())
  in
  pkg_libs

let entry_modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let name = Lib.name (Lib.Local.to_lib lib) in
  Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
  >>| Ml_sources.modules ~for_:(Library name)
  >>| Modules.entry_modules

let entry_modules sctx ~pkg =
  let* l =
    libs_of_pkg (Super_context.context sctx) ~pkg
    >>| List.filter ~f:(fun lib ->
            Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private
            |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
        let+ m = entry_modules_by_lib sctx l in
        (l, m))
  in
  Lib.Local.Map.of_list_exn l

let create_odoc ctx ~target odoc_file =
  let html_base = Paths.html ctx target in
  let odocl_base = Paths.odocl ctx target in
  let basename = Path.Build.basename odoc_file |> Filename.chop_extension in
  let odocl_file = odocl_base ++ (basename ^ ".odocl") in
  match target with
  | Lib _ ->
    let html_dir = html_base ++ Stdune.String.capitalize basename in
    { odoc_file
    ; odocl_file
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; source = Module
    }
  | Pkg _ ->
    { odoc_file
    ; odocl_file
    ; html_dir = html_base
    ; html_file =
        html_base
        ++ sprintf "%s.html"
             (basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn)
    ; source = Mld
    }

let static_html ctx =
  let open Paths in
  [ css_file ctx; highlight_pack_js ctx; toplevel_index ctx ]

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld ->
        (Filename.chop_extension (Path.Build.basename mld), mld))
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]

let odoc_artefacts sctx target =
  let ctx = Super_context.context sctx in
  let dir = Paths.odocs ctx target in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds = Packages.mlds sctx pkg in
      let mlds = check_mlds_no_dupes ~pkg ~mlds in
      if String.Map.mem mlds "index" then mlds
      else
        let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
        String.Map.add_exn mlds "index" gen_mld
    in
    String.Map.values mlds
    |> List.map ~f:(fun mld ->
           Mld.create mld |> Mld.odoc_file ~doc_dir:dir
           |> create_odoc ctx ~target)
  | Lib lib ->
    let info = Lib.Local.info lib in
    let obj_dir = Lib_info.obj_dir info in
    let* modules = entry_modules_by_lib sctx lib in
    List.map
      ~f:(fun m ->
        let odoc_file = Obj_dir.Module.odoc obj_dir m in
        create_odoc ctx ~target odoc_file)
      modules
    |> Memo.return

let setup_lib_odocl_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t * Lib.t list Resolve.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.Local.equal l1 l2
      && Resolve.equal (List.equal Lib.equal) r1 r2

    let hash (sc, l, r) =
      Poly.hash
        ( Super_context.hash sc
        , Lib.Local.hash l
        , Resolve.hash (List.hash Lib.hash) r )

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, lib, requires) =
    let* odocs = odoc_artefacts sctx (Lib lib) in
    let pkg = Lib_info.package (Lib.Local.info lib) in
    Memo.parallel_iter odocs ~f:(fun odoc ->
        link_odoc_rules sctx ~pkg ~requires odoc)
  in
  Memo.With_implicit_output.create "setup_library_odocls_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_lib_odocl_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_odocl_rules_def (sctx, lib, requires)

let setup_pkg_rules_def memo_name f =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t * Lib.Local.t list

    let equal (s1, p1, l1) (s2, p2, l2) =
      Package.Name.equal p1 p2
      && List.equal Lib.Local.equal l1 l2
      && Super_context.equal s1 s2

    let hash (sctx, p, ls) =
      Poly.hash
        ( Super_context.hash sctx
        , Package.Name.hash p
        , List.hash Lib.Local.hash ls )

    let to_dyn (_, package, libs) =
      let open Dyn in
      Tuple
        [ Package.Name.to_dyn package
        ; List (List.map ~f:Lib.Local.to_dyn libs)
        ]
  end in
  Memo.With_implicit_output.create memo_name
    ~input:(module Input)
    ~implicit_output:Rules.implicit_output f

let setup_pkg_odocl_rules_def =
  let f (sctx, pkg, (libs : Lib.Local.t list)) =
    let* requires =
      let libs = (libs :> Lib.t list) in
      Lib.closure libs ~linking:false
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_odocl_rules sctx ~requires)
    and* _ =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let pkg = Some pkg in
      let+ () =
        Memo.parallel_iter pkg_odocs ~f:(fun odoc ->
            link_odoc_rules sctx ~pkg ~requires odoc)
      in
      pkg_odocs
    and* _ =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    Memo.return ()
  in
  setup_pkg_rules_def "setup-package-odocls-rules" f

let setup_pkg_odocl_rules sctx ~pkg ~libs : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_odocl_rules_def (sctx, pkg, libs)

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t

    let equal (sc1, l1) (sc2, l2) =
      Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2

    let hash (sc, l) = Poly.hash (Super_context.hash sc, Lib.Local.hash l)

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, lib) =
    let ctx = Super_context.context sctx in
    let* odocs = odoc_artefacts sctx (Lib lib) in
    let* () = Memo.parallel_iter odocs ~f:(fun odoc -> setup_html sctx odoc) in
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx (Lib lib))
      (Action_builder.paths (List.rev_append static_html html_files))
  in
  Memo.With_implicit_output.create "setup-library-html-rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_lib_html_rules sctx lib =
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib)

let setup_pkg_html_rules_def =
  let f (sctx, pkg, (libs : Lib.Local.t list)) =
    let ctx = Super_context.context sctx in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_html_rules sctx)
    and* pkg_odocs =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let+ () = Memo.parallel_iter pkg_odocs ~f:(fun o -> setup_html sctx o) in
      pkg_odocs
    and* lib_odocs =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    let odocs = List.concat (pkg_odocs :: lib_odocs) in
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx (Pkg pkg))
      (Action_builder.paths (List.rev_append static_html html_files))
  in
  setup_pkg_rules_def "setup-package-html-rules" f

let setup_pkg_html_rules sctx ~pkg ~libs : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg, libs)

let setup_package_aliases sctx (pkg : Package.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source ctx.build_dir pkg_dir in
    Alias.doc ~dir
  in
  let* libs =
    libs_of_pkg ctx ~pkg:name
    >>| List.map ~f:(fun lib -> Dep.html_alias ctx (Lib lib))
  in
  Dep.html_alias ctx (Pkg name) :: libs
  |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
  |> Action_builder.deps
  |> Rules.Produce.Alias.add_deps alias

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Lib.Local.Map.to_list entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) ->
         let name lib = Lib.name (Lib.Local.to_lib lib) in
         Lib_name.compare (name x) (name y))
  |> List.iter ~f:(fun (lib, modules) ->
         let lib = Lib.Local.to_lib lib in
         Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
         Buffer.add_string b
           (match modules with
           | [ x ] ->
             sprintf
               "The entry point of this library is the module:\n{!module-%s}.\n"
               (Module_name.to_string (Module.name x))
           | _ ->
             sprintf
               "This library exposes the following toplevel modules:\n\
                {!modules:%s}\n"
               (modules
               |> List.filter ~f:(fun m ->
                      Module.visibility m = Visibility.Public)
               |> List.sort ~compare:(fun x y ->
                      Module_name.compare (Module.name x) (Module.name y))
               |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
               |> String.concat ~sep:" ")));
  Buffer.contents b

let package_mlds =
  let memo =
    Memo.create "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package)
      (fun (sctx, pkg) ->
        Rules.collect (fun () ->
            (* CR-someday jeremiedimino: it is weird that we drop the
               [Package.t] and go back to a package name here. Need to try and
               change that one day. *)
            let pkg = Package.name pkg in
            let* mlds = Packages.mlds sctx pkg in
            let mlds = check_mlds_no_dupes ~pkg ~mlds in
            let ctx = Super_context.context sctx in
            if String.Map.mem mlds "index" then Memo.return mlds
            else
              let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
              let* entry_modules = entry_modules sctx ~pkg in
              let+ () =
                add_rule sctx
                  (Action_builder.write_file gen_mld
                     (default_index ~pkg entry_modules))
              in
              String.Map.set mlds "index" gen_mld))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)

let setup_package_odoc_rules sctx ~pkg =
  let* mlds = package_mlds sctx ~pkg >>| fst in
  let ctx = Super_context.context sctx in
  (* CR-someday jeremiedimino: it is weird that we drop the [Package.t] and go
     back to a package name here. Need to try and change that one day. *)
  let pkg = Package.name pkg in
  let* odocs =
    Memo.parallel_map (String.Map.values mlds) ~f:(fun mld ->
        compile_mld sctx (Mld.create mld) ~pkg
          ~doc_dir:(Paths.odocs ctx (Pkg pkg))
          ~includes:(Action_builder.return []))
  in
  Dep.setup_deps ctx (Pkg pkg) (Path.set_of_build_paths_list odocs)

let gen_project_rules sctx project =
  let* packages = Only_packages.packages_of_project project in
  Package.Name.Map_traversals.parallel_iter packages
    ~f:(fun _ (pkg : Package.t) ->
      (* setup @doc to build the correct html for the package *)
      setup_package_aliases sctx pkg)

let setup_private_library_doc_alias sctx ~scope ~dir (l : Dune_file.Library.t) =
  match l.visibility with
  | Public _ -> Memo.return ()
  | Private _ ->
    let ctx = Super_context.context sctx in
    let* lib =
      Lib.DB.find_even_when_hidden (Scope.libs scope) (Library.best_name l)
      >>| Option.value_exn
    in
    let lib = Lib (Lib.Local.of_lib_exn lib) in
    Rules.Produce.Alias.add_deps (Alias.private_doc ~dir)
      (lib |> Dep.html_alias ctx |> Dune_engine.Dep.alias |> Action_builder.dep)

let has_rules m =
  let rules = Rules.collect_unit (fun () -> m) in
  Memo.return
    (Build_config.Rules
       { rules
       ; build_dir_only_sub_dirs = Subdir_set.empty
       ; directory_targets = Path.Build.Map.empty
       })

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Only_packages.get () in
  match Package.Name.Map.find packages pkg with
  | None ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs = Subdir_set.empty
         ; directory_targets = Path.Build.Map.empty
         })
  | Some pkg -> has_rules (f pkg)

let gen_rules sctx ~dir:_ rest =
  match rest with
  | [] ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs = Subdir_set.All
         ; directory_targets = Path.Build.Map.empty
         })
  | [ "_html" ] ->
    has_rules (setup_css_rule sctx >>> setup_toplevel_index_rule sctx)
  | [ "_mlds"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
        let* _mlds, rules = package_mlds sctx ~pkg in
        Rules.produce rules)
  | [ "_odoc"; "pkg"; pkg ] ->
    with_package pkg ~f:(fun pkg -> setup_package_odoc_rules sctx ~pkg)
  | [ "_odocls"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string ctx lib_unique_name_or_pkg in
       let setup_pkg_odocl_rules pkg =
         let* pkg_libs = load_all_odoc_rules_pkg ctx ~pkg in
         setup_pkg_odocl_rules sctx ~pkg ~libs:pkg_libs
       in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib -> (
           match Lib_info.package (Lib.Local.info lib) with
           | None ->
             let* requires =
               Lib.closure [ Lib.Local.to_lib lib ] ~linking:false
             in
             setup_lib_odocl_rules sctx lib ~requires
           | Some pkg -> setup_pkg_odocl_rules pkg)
       and+ () =
         let* packages = Only_packages.get () in
         match
           Package.Name.Map.find packages
             (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_odocl_rules name
       in
       ())
  | [ "_html"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string ctx lib_unique_name_or_pkg in
       let setup_pkg_html_rules pkg =
         let* pkg_libs =
           load_all_odoc_rules_pkg (Super_context.context sctx) ~pkg
         in
         setup_pkg_html_rules sctx ~pkg ~libs:pkg_libs
       in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib -> (
           match Lib_info.package (Lib.Local.info lib) with
           | None -> setup_lib_html_rules sctx lib
           | Some pkg -> setup_pkg_html_rules pkg)
       and+ () =
         let* packages = Only_packages.get () in
         match
           Package.Name.Map.find packages
             (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_html_rules name
       in
       ())
  | _ -> Memo.return Build_config.Redirect_to_parent
