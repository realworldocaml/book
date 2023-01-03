open Import
open Memo.O
module Buildable = Dune_file.Buildable
module Library = Dune_file.Library
module Mode_conf = Dune_file.Mode_conf

let msvc_hack_cclibs =
  List.map ~f:(fun lib ->
      let lib =
        match String.drop_prefix lib ~prefix:"-l" with
        | None -> lib
        | Some l -> l ^ ".lib"
      in
      Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))

(* Build an OCaml library. *)
let build_lib (lib : Library.t) ~native_archives ~sctx ~expander ~flags ~dir
    ~mode ~cm_files =
  let ctx = Super_context.context sctx in
  Memo.Result.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        let lib_archive = Library.stubs_archive lib in
        let foreign_archives = Library.foreign_archives lib in
        let make_args ~stub_mode archive =
          let lname =
            "-l"
            ^ Foreign.Archive.(name ~mode:stub_mode archive |> Name.to_string)
          in
          let cclib = [ "-cclib"; lname ] in
          let dllib = [ "-dllib"; lname ] in
          match mode with
          | Native -> cclib
          | Byte -> dllib @ cclib
        in
        let stub_mode =
          if Buildable.has_mode_dependent_foreign_stubs lib.buildable then
            Mode.Select.Only mode
          else Mode.Select.All
        in
        let foreign_archives =
          List.concat_map foreign_archives
            ~f:(make_args ~stub_mode:Mode.Select.All)
        in
        match lib_archive with
        | Some lib_archive ->
          make_args ~stub_mode lib_archive @ foreign_archives
        | None -> foreign_archives
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        match ctx.lib_config.ccomp_type with
        | Msvc -> msvc_hack_cclibs
        | Other _ -> Fun.id
      in
      let obj_deps =
        Action_builder.paths (Cm_files.unsorted_objects_and_cms cm_files ~mode)
      in
      let ocaml_flags = Ocaml_flags.get flags mode in
      let* standard =
        let+ project = Scope.DB.find_by_dir dir |> Memo.map ~f:Scope.project in
        match Dune_project.use_standard_c_and_cxx_flags project with
        | Some true when Buildable.has_foreign_cxx lib.buildable ->
          Cxx_flags.get_flags ~for_:Link ctx
        | _ -> Action_builder.return []
      in
      let cclibs =
        Expander.expand_and_eval_set expander lib.c_library_flags ~standard
      in
      let standard = Action_builder.return [] in
      let library_flags =
        Expander.expand_and_eval_set expander lib.library_flags ~standard
      in
      let ctypes_cclib_flags =
        Ctypes_rules.ctypes_cclib_flags sctx ~expander ~buildable:lib.buildable
      in
      Super_context.add_rule ~dir sctx ~loc:lib.buildable.loc
        (let open Action_builder.With_targets.O in
        Action_builder.with_no_targets obj_deps
        >>> Command.run (Ok compiler) ~dir:(Path.build ctx.build_dir)
              [ Command.Args.dyn ocaml_flags
              ; A "-a"
              ; A "-o"
              ; Target target
              ; As stubs_flags
              ; Dyn
                  (Action_builder.map cclibs ~f:(fun x ->
                       Command.quote_args "-cclib" (map_cclibs x)))
              ; Command.Args.dyn library_flags
              ; As
                  (match lib.kind with
                  | Normal -> []
                  | Ppx_deriver _ | Ppx_rewriter _ -> [ "-linkall" ])
              ; Dyn
                  (Cm_files.top_sorted_cms cm_files ~mode
                  |> Action_builder.map ~f:(fun x -> Command.Args.Deps x))
              ; Hidden_targets
                  (match mode with
                  | Byte -> []
                  | Native -> native_archives)
              ; Dyn
                  (Action_builder.map ctypes_cclib_flags ~f:(fun x ->
                       Command.quote_args "-cclib" (map_cclibs x)))
              ; Deps
                  (Foreign.Objects.build_paths lib.buildable.extra_objects
                     ~ext_obj:ctx.lib_config.ext_obj ~dir)
              ]))

let gen_wrapped_compat_modules (lib : Library.t) cctx =
  let modules = Compilation_context.modules cctx in
  let wrapped_compat = Modules.wrapped_compat modules in
  let transition_message =
    lazy
      (match Modules.wrapped modules with
      | Simple _ -> assert false
      | Yes_with_transition r -> r)
  in
  Module_name.Map_traversals.parallel_iter wrapped_compat ~f:(fun name m ->
      let main_module_name =
        match Library.main_module_name lib with
        | This (Some mmn) -> Module_name.to_string mmn
        | _ -> assert false
      in
      let contents =
        let name = Module_name.to_string name in
        let hidden_name = sprintf "%s__%s" main_module_name name in
        let real_name = sprintf "%s.%s" main_module_name name in
        sprintf {|[@@@deprecated "%s. Use %s instead."] include %s|}
          (Lazy.force transition_message)
          real_name hidden_name
      in
      let source_path = Option.value_exn (Module.file m ~ml_kind:Impl) in
      let loc = lib.buildable.loc in
      let sctx = Compilation_context.super_context cctx in
      Action_builder.write_file (Path.as_in_build_dir_exn source_path) contents
      |> Super_context.add_rule sctx ~loc ~dir:(Compilation_context.dir cctx))

(* Rules for building static and dynamic libraries using [ocamlmklib]. *)
let ocamlmklib ~loc ~c_library_flags ~sctx ~dir ~o_files ~archive_name
    ~stubs_mode ~build_targets_together =
  let ctx = Super_context.context sctx in
  let { Lib_config.ext_lib; ext_dll; _ } = ctx.lib_config in
  let static_target =
    Foreign.Archive.Name.lib_file archive_name ~dir ~ext_lib ~mode:stubs_mode
  in
  let cclibs =
    Action_builder.map c_library_flags ~f:(fun cclibs ->
        (* https://github.com/ocaml/dune/issues/119 *)
        let cclibs =
          match ctx.lib_config.ccomp_type with
          | Msvc -> msvc_hack_cclibs cclibs
          | Other _ -> cclibs
        in
        Command.quote_args "-ldopt" cclibs)
  in
  let build ~custom ~sandbox targets =
    Super_context.add_rule sctx ~dir ~loc
      (let open Action_builder.With_targets.O in
      let ctx = Super_context.context sctx in
      Command.run ~dir:(Path.build ctx.build_dir) ctx.ocamlmklib
        [ A "-g"
        ; (if custom then A "-custom" else Command.Args.empty)
        ; A "-o"
        ; Path
            (Path.build
               (Foreign.Archive.Name.path ~dir archive_name ~mode:stubs_mode))
        ; Deps o_files
          (* The [c_library_flags] is needed only for the [dynamic_target] case,
             but we pass them unconditionally for simplicity. *)
        ; Dyn cclibs
        ; Hidden_targets targets
        ]
      >>| Action.Full.add_sandbox sandbox)
  in
  let dynamic_target =
    Foreign.Archive.Name.dll_file archive_name ~dir ~ext_dll ~mode:stubs_mode
  in
  if build_targets_together then
    (* Build both the static and dynamic targets in one [ocamlmklib] invocation,
       unless dynamically linked foreign archives are disabled. *)
    build ~sandbox:Sandbox_config.no_special_requirements ~custom:false
      (if ctx.dynamically_linked_foreign_archives then
       [ static_target; dynamic_target ]
      else [ static_target ])
  else
    let open Memo.O in
    (* Build the static target only by passing the [-custom] flag. *)
    let* () =
      build ~sandbox:Sandbox_config.no_special_requirements ~custom:true
        [ static_target ]
    in
    (* The second rule (below) may fail on some platforms, but the build will
       succeed as long as the resulting dynamic library isn't actually needed
       (the rule will not fire in that case). We can't tell ocamlmklib to build
       only the dynamic target, so it will actually build *both* and we
       therefore sandbox the action to avoid overwriting the static archive.

       TODO: Figure out how to avoid duplicating work in the case when both
       rules fire. It seems like this might require introducing the notion of
       "optional targets", allowing us to run [ocamlmklib] with the [-failsafe]
       flag, which always produces the static target and sometimes produces the
       dynamic target too. *)
    Memo.when_ ctx.dynamically_linked_foreign_archives (fun () ->
        build ~sandbox:Sandbox_config.needs_sandboxing ~custom:false
          [ dynamic_target ])

(* Build a static and a dynamic archive for a foreign library. Note that the
   dynamic archive can't be built on some platforms, in which case the rule that
   produces it will fail. *)
let foreign_rules (library : Foreign.Library.t) ~sctx ~expander ~dir
    ~dir_contents =
  let archive_name = library.archive_name in
  let* foreign_sources =
    Dir_contents.foreign_sources dir_contents
    >>| Foreign_sources.for_archive ~archive_name
  in
  let* o_files =
    let+ o_files_by_mode =
      Foreign_rules.build_o_files ~sctx ~dir ~expander
        ~requires:(Resolve.return []) ~dir_contents ~foreign_sources
    in
    Mode.Map.Multi.for_all_modes o_files_by_mode
  in
  let* () = Check_rules.add_files sctx ~dir o_files in
  let* standard =
    let+ project =
      let+ scope = Scope.DB.find_by_dir dir in
      Scope.project scope
    in
    let ctx = Super_context.context sctx in
    match Dune_project.use_standard_c_and_cxx_flags project with
    | Some true when Foreign.Sources.has_cxx_sources foreign_sources ->
      Cxx_flags.get_flags ~for_:Link ctx
    | _ -> Action_builder.return []
  in
  let c_library_flags =
    Expander.expand_and_eval_set expander Ordered_set_lang.Unexpanded.standard
      ~standard
  in
  ocamlmklib ~archive_name ~loc:library.stubs.loc ~c_library_flags ~sctx ~dir
    ~o_files ~build_targets_together:false ~stubs_mode:Mode.Select.All

(* Build a required set of archives for an OCaml library. *)
let build_stubs lib ~cctx ~dir ~expander ~requires ~dir_contents
    ~vlib_stubs_o_files =
  let sctx = Compilation_context.super_context cctx in
  let* foreign_sources =
    let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
    let name = Library.best_name lib in
    Foreign_sources.for_lib foreign_sources ~name
  in
  let* o_files =
    let lib_foreign_o_files =
      let { Lib_config.ext_obj; _ } = (Super_context.context sctx).lib_config in
      Foreign.Objects.build_paths lib.buildable.extra_objects ~ext_obj ~dir
    in
    let+ tbl =
      Foreign_rules.build_o_files ~sctx ~dir ~expander ~requires ~dir_contents
        ~foreign_sources
    in
    Mode.Map.Multi.add_all tbl Mode.Select.All lib_foreign_o_files
  in
  let all_o_files = Mode.Map.Multi.to_flat_list o_files in
  let* () = Check_rules.add_files sctx ~dir all_o_files in
  if List.for_all ~f:List.is_empty [ all_o_files; vlib_stubs_o_files ] then
    Memo.return ()
  else
    let ctx = Super_context.context sctx in
    let lib_name = Lib_name.Local.to_string (snd lib.name) in
    let archive_name = Foreign.Archive.Name.stubs lib_name in
    let modes = Compilation_context.modes cctx in
    let build_targets_together =
      modes.ocaml.native && modes.ocaml.byte
      && Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
    in
    let* standard =
      let+ project = Scope.DB.find_by_dir dir >>| Scope.project in
      match Dune_project.use_standard_c_and_cxx_flags project with
      | Some true when Foreign.Sources.has_cxx_sources foreign_sources ->
        Cxx_flags.get_flags ~for_:Link ctx
      | _ -> Action_builder.return []
    in
    let c_library_flags =
      Expander.expand_and_eval_set expander lib.c_library_flags ~standard
    in
    let lib_o_files_for_all_modes = Mode.Map.Multi.for_all_modes o_files in
    let for_all_modes =
      List.rev_append vlib_stubs_o_files lib_o_files_for_all_modes
    in
    if
      Mode.Dict.Set.to_list modes.ocaml
      |> List.for_all ~f:(fun mode ->
             List.is_empty
             @@ Mode.Map.Multi.for_only ~and_all:false o_files mode)
    then
      (* if stubs are not mode dependent *)
      let o_files = for_all_modes in
      ocamlmklib ~archive_name ~loc:lib.buildable.loc ~sctx ~dir ~o_files
        ~c_library_flags ~build_targets_together ~stubs_mode:Mode.Select.All
    else
      let modes =
        Mode.Dict.Set.to_list modes.ocaml
        |> List.map ~f:(fun mode ->
               let o_files_for_mode =
                 Mode.Map.Multi.for_only ~and_all:false o_files mode
               in
               ( List.rev_append for_all_modes o_files_for_mode
               , Mode.Select.Only mode ))
      in
      Memo.parallel_iter modes ~f:(fun (o_files, stubs_mode) ->
          ocamlmklib ~archive_name ~loc:lib.buildable.loc ~sctx ~dir ~o_files
            ~c_library_flags ~build_targets_together ~stubs_mode)

let build_shared lib ~native_archives ~sctx ~dir ~flags =
  let ctx = Super_context.context sctx in
  Memo.Result.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let ext_lib = ctx.lib_config.ext_lib in
      let src =
        let ext = Mode.compiled_lib_ext Native in
        Path.build (Library.archive lib ~dir ~ext)
      in
      let dst =
        let ext = Mode.plugin_ext Native in
        Library.archive lib ~dir ~ext
      in
      let include_flags_for_relative_foreign_archives =
        Command.Args.S
          (List.map lib.buildable.foreign_archives ~f:(fun (_loc, archive) ->
               let dir = Foreign.Archive.dir_path ~dir archive in
               Command.Args.S [ A "-I"; Path (Path.build dir) ]))
      in
      let open Action_builder.With_targets.O in
      let build =
        Action_builder.with_no_targets
          (Action_builder.paths
             (Library.foreign_lib_files lib ~dir ~ext_lib
                ~for_mode:(Mode.Select.Only Byte)
              @ Library.foreign_lib_files lib ~dir ~ext_lib
                  ~for_mode:Mode.Select.All
             |> List.map ~f:Path.build))
        >>> Command.run ~dir:(Path.build ctx.build_dir) (Ok ocamlopt)
              [ Command.Args.dyn (Ocaml_flags.get flags Native)
              ; A "-shared"
              ; A "-linkall"
              ; A "-I"
              ; Path (Path.build dir)
              ; include_flags_for_relative_foreign_archives
              ; A "-o"
              ; Target dst
              ; Dep src
              ]
      in
      let build =
        Action_builder.with_no_targets
          (Action_builder.paths (List.map ~f:Path.build native_archives))
        >>> build
      in
      Super_context.add_rule sctx build ~dir ~loc:lib.buildable.loc)

let setup_build_archives (lib : Dune_file.Library.t) ~top_sorted_modules ~cctx
    ~expander ~lib_info =
  let obj_dir = Compilation_context.obj_dir cctx in
  let dir = Compilation_context.dir cctx in
  let flags = Compilation_context.flags cctx in
  let modules = Compilation_context.modules cctx in
  let js_of_ocaml =
    Js_of_ocaml.In_context.make ~dir lib.buildable.js_of_ocaml
  in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Compilation_context.context cctx in
  let { Lib_config.ext_obj; natdynlink_supported; _ } = ctx.lib_config in
  let open Memo.O in
  let* () =
    Modules.exit_module modules
    |> Memo.Option.iter ~f:(fun m ->
           (* These files needs to be alongside stdlib.cma as the compiler
              implicitly adds this module. *)
           [ (Cm_kind.Cmx, Cm_kind.ext Cmx)
           ; (Cmo, Cm_kind.ext Cmo)
           ; (Cmx, ext_obj)
           ]
           |> Memo.parallel_iter ~f:(fun (kind, ext) ->
                  let src =
                    Path.build
                      (Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml kind) ~ext)
                  in
                  let obj_name = Module.obj_name m in
                  let fname =
                    Module_name.Unique.artifact_filename obj_name ~ext
                  in
                  (* XXX we should get the directory from the dir of the cma
                     file explicitly *)
                  let dst = Path.Build.relative (Obj_dir.dir obj_dir) fname in
                  Super_context.add_rule sctx ~dir ~loc:lib.buildable.loc
                    (Action_builder.copy ~src ~dst)))
  in
  let modes = Compilation_context.modes cctx in
  (* The [dir] below is used as an object directory without going through
     [Obj_dir]. That's fragile and will break if the layout of the object
     directory changes *)
  let dir = Obj_dir.dir obj_dir in
  let native_archives =
    Lib_info.eval_native_archives_exn lib_info ~modules:(Some modules)
  in
  let cm_files =
    let excluded_modules =
      (* ctypes type_gen and function_gen scripts should not be included in the
         library. Otherwise they will spew stuff to stdout on library load. *)
      match lib.buildable.ctypes with
      | Some ctypes -> Ctypes_stanza.non_installable_modules ctypes
      | None -> []
    in
    Cm_files.make ~excluded_modules ~obj_dir ~ext_obj ~modules
      ~top_sorted_modules ()
  in
  let* () =
    Mode.Dict.Set.iter_concurrently modes.ocaml ~f:(fun mode ->
        build_lib lib ~native_archives ~dir ~sctx ~expander ~flags ~mode
          ~cm_files)
  and* () =
    (* Build *.cma.js *)
    Memo.when_ modes.ocaml.byte (fun () ->
        let action_with_targets =
          let src =
            Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte)
          in
          let target =
            Path.Build.relative (Obj_dir.obj_dir obj_dir)
              (Path.Build.basename src)
            |> Path.Build.extend_basename ~suffix:".js"
          in
          Jsoo_rules.build_cm cctx ~in_context:js_of_ocaml ~src ~target
        in
        action_with_targets
        >>= Super_context.add_rule sctx ~dir ~loc:lib.buildable.loc)
  in
  Memo.when_
    (Dynlink_supported.By_the_os.get natdynlink_supported && modes.ocaml.native)
    (fun () -> build_shared ~native_archives ~sctx lib ~dir ~flags)

let cctx (lib : Library.t) ~sctx ~source_modules ~dir ~expander ~scope
    ~compile_info =
  let* flags = Super_context.ocaml_flags sctx ~dir lib.buildable.flags
  and* vimpl = Virtual_rules.impl sctx ~lib ~scope in
  let obj_dir = Library.obj_dir ~dir lib in
  let ctx = Super_context.context sctx in
  let* modules, pp =
    Buildable_rules.modules_rules sctx
      (Library (lib.buildable, snd lib.name))
      expander ~dir scope source_modules
  in
  let modules = Vimpl.impl_modules vimpl modules in
  let requires_compile = Lib.Compile.direct_requires compile_info in
  let requires_link = Lib.Compile.requires_link compile_info in
  let modes =
    let { Lib_config.has_native; _ } = ctx.lib_config in
    Dune_file.Mode_conf.Lib.Set.eval_detailed lib.modes ~has_native
  in
  let package = Dune_file.Library.package lib in
  let js_of_ocaml =
    Js_of_ocaml.In_context.make ~dir lib.buildable.js_of_ocaml
  in
  Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
    ~modules ~flags ~requires_compile ~requires_link ~preprocessing:pp
    ~opaque:Inherit_from_settings ~js_of_ocaml:(Some js_of_ocaml)
    ?stdlib:lib.stdlib ~package ?vimpl ~modes

let library_rules (lib : Library.t) ~local_lib ~cctx ~source_modules
    ~dir_contents ~compile_info =
  let source_modules =
    Modules.fold_user_written source_modules ~init:[] ~f:(fun m acc -> m :: acc)
  in
  let modules = Compilation_context.modules cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let vimpl = Compilation_context.vimpl cctx in
  let flags = Compilation_context.flags cctx in
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let scope = Compilation_context.scope cctx in
  let* requires_compile = Compilation_context.requires_compile cctx in
  let stdlib_dir = (Compilation_context.context cctx).Context.stdlib_dir in
  let top_sorted_modules =
    let impl_only = Modules.impl_only modules in
    Dep_graph.top_closed_implementations
      (Compilation_context.dep_graphs cctx).impl impl_only
  in
  let* () =
    Memo.Option.iter vimpl
      ~f:(Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir)
  in
  let* () = Check_rules.add_obj_dir sctx ~obj_dir in
  let* () = Check_rules.add_cycle_check sctx ~dir top_sorted_modules in
  let* () = gen_wrapped_compat_modules lib cctx
  and* () = Module_compilation.build_all cctx
  and* expander = Super_context.expander sctx ~dir
  and* lib_info =
    let lib_config = (Super_context.context sctx).lib_config in
    Library.to_lib_info lib ~dir ~lib_config
  in
  let+ () =
    Memo.when_
      (not (Library.is_virtual lib))
      (fun () ->
        setup_build_archives lib ~lib_info ~top_sorted_modules ~cctx ~expander)
  and+ () =
    let vlib_stubs_o_files = Vimpl.vlib_stubs_o_files vimpl in
    Memo.when_
      (Library.has_foreign lib || List.is_non_empty vlib_stubs_o_files)
      (fun () ->
        build_stubs lib ~cctx ~dir ~expander ~requires:requires_compile
          ~dir_contents ~vlib_stubs_o_files)
  and+ () = Odoc.setup_library_odoc_rules cctx local_lib
  and+ () =
    Sub_system.gen_rules
      { super_context = sctx
      ; dir
      ; stanza = lib
      ; scope
      ; source_modules
      ; compile_info
      }
  and+ preprocess =
    Resolve.Memo.read_memo
      (Preprocess.Per_module.with_instrumentation lib.buildable.preprocess
         ~instrumentation_backend:
           (Lib.DB.instrumentation_backend (Scope.libs scope)))
  in
  ( cctx
  , Merlin.make ~requires:requires_compile ~stdlib_dir ~flags ~modules
      ~preprocess ~libname:(snd lib.name) ~obj_dir
      ~dialects:(Dune_project.dialects (Scope.project scope))
      ~ident:(Lib.Compile.merlin_ident compile_info)
      ~modes:(`Lib (Lib_info.modes lib_info))
      () )

let rules (lib : Library.t) ~sctx ~dir_contents ~dir ~expander ~scope =
  let buildable = lib.buildable in
  let* local_lib, compile_info =
    Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
      ~allow_overlaps:buildable.allow_overlapping_dependencies
  in
  let local_lib = Lib.Local.of_lib_exn local_lib in
  let f () =
    let* source_modules =
      Dir_contents.ocaml dir_contents
      >>| Ml_sources.modules ~for_:(Library (Library.best_name lib))
    in
    let* cctx =
      cctx lib ~sctx ~source_modules ~dir ~scope ~expander ~compile_info
    in
    let* () =
      match buildable.ctypes with
      | None -> Memo.return ()
      | Some _ctypes ->
        Ctypes_rules.gen_rules ~loc:(fst lib.name) ~cctx ~buildable ~sctx ~scope
          ~dir
    in
    library_rules lib ~local_lib ~cctx ~source_modules ~dir_contents
      ~compile_info
  in
  let* () = Buildable_rules.gen_select_rules sctx compile_info ~dir in
  Buildable_rules.with_lib_deps
    (Super_context.context sctx)
    compile_info ~dir ~f
