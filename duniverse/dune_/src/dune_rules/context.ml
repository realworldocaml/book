open Import
open Memo.O

module Kind = struct
  module Opam = struct
    type t =
      { root : string option
      ; switch : string
      }
  end

  type t =
    | Default
    | Opam of Opam.t

  let to_dyn : t -> Dyn.t = function
    | Default -> Dyn.string "default"
    | Opam o ->
      Dyn.(
        record [ ("root", option string o.root); ("switch", string o.switch) ])
end

module Env_nodes = struct
  type t =
    { context : Dune_env.Stanza.t
    ; workspace : Dune_env.Stanza.t
    }

  let extra_env ~profile env_nodes =
    Env.extend_env (Dune_env.Stanza.find env_nodes.context ~profile).env_vars
      (Dune_env.Stanza.find env_nodes.workspace ~profile).env_vars
end

module Bin = struct
  include Bin

  let which ~path prog =
    let prog = add_exe prog in
    Memo.List.find_map path ~f:(fun dir ->
        let fn = Path.relative dir prog in
        let+ exists = Fs_memo.file_exists (Path.as_outside_build_dir_exn fn) in
        if exists then Some fn else None)
end

module Program = struct
  module Name = String

  let programs_for_which_we_prefer_opt_ext =
    [ "ocamlc"; "ocamldep"; "ocamlmklib"; "ocamlobjinfo"; "ocamlopt" ]

  let best_path dir program =
    let exe_path program =
      let fn = Path.relative dir (program ^ Bin.exe) in
      let+ exists = Fs_memo.file_exists (Path.as_outside_build_dir_exn fn) in
      if exists then Some fn else None
    in
    if List.mem programs_for_which_we_prefer_opt_ext program ~equal:String.equal
    then
      let* path = exe_path (program ^ ".opt") in
      match path with
      | None -> exe_path program
      | Some _ as path -> Memo.return path
    else exe_path program

  module rec Rec : sig
    val which : path:Path.t list -> string -> Path.t option Memo.t
  end = struct
    open Rec

    let which_impl (path, program) =
      match path with
      | [] -> Memo.return None
      | dir :: path -> (
        let* res = best_path dir program in
        match res with
        | None -> which ~path program
        | Some prog -> Memo.return (Some prog))

    let which =
      let memo =
        let module Input = struct
          type t = Path.t list * string

          let equal = Tuple.T2.equal (List.equal Path.equal) String.equal

          let hash = Tuple.T2.hash (List.hash Path.hash) String.hash

          let to_dyn = Dyn.opaque
        end in
        Memo.create "which"
          ~input:(module Input)
          ~cutoff:(Option.equal Path.equal) which_impl
      in
      fun ~path prog -> Memo.exec memo (path, prog)
  end

  include Rec
end

type t =
  { name : Context_name.t
  ; kind : Kind.t
  ; profile : Profile.t
  ; merlin : bool
  ; fdo_target_exe : Path.t option
  ; dynamically_linked_foreign_archives : bool
  ; for_host : t option
  ; implicit : bool
  ; build_dir : Path.Build.t
  ; env_nodes : Env_nodes.t
  ; path : Path.t list
  ; toplevel_path : Path.t option
  ; ocaml_bin : Path.t
  ; ocaml : Action.Prog.t
  ; ocamlc : Path.t
  ; ocamlopt : Action.Prog.t
  ; ocamldep : Action.Prog.t
  ; ocamlmklib : Action.Prog.t
  ; ocamlobjinfo : Action.Prog.t
  ; env : Env.t
  ; findlib_paths : Path.t list
  ; findlib_toolchain : Context_name.t option
  ; default_ocamlpath : Path.t list
  ; arch_sixtyfour : bool
  ; ocaml_config : Ocaml_config.t
  ; ocaml_config_vars : Ocaml_config.Vars.t
  ; version : Ocaml.Version.t
  ; stdlib_dir : Path.t
  ; supports_shared_libraries : Dynlink_supported.By_the_os.t
  ; lib_config : Lib_config.t
  ; build_context : Build_context.t
  ; make : Path.t option Memo.Lazy.t
  }

let equal x y = Context_name.equal x.name y.name

let hash t = Context_name.hash t.name

let build_context t = t.build_context

let to_dyn t : Dyn.t =
  let open Dyn in
  let path = Path.to_dyn in
  record
    [ ("name", Context_name.to_dyn t.name)
    ; ("kind", Kind.to_dyn t.kind)
    ; ("profile", Profile.to_dyn t.profile)
    ; ("merlin", Bool t.merlin)
    ; ( "for_host"
      , option Context_name.to_dyn (Option.map t.for_host ~f:(fun t -> t.name))
      )
    ; ("fdo_target_exe", option path t.fdo_target_exe)
    ; ("build_dir", Path.Build.to_dyn t.build_dir)
    ; ("toplevel_path", option path t.toplevel_path)
    ; ("ocaml_bin", path t.ocaml_bin)
    ; ("ocaml", Action.Prog.to_dyn t.ocaml)
    ; ("ocamlc", path t.ocamlc)
    ; ("ocamlopt", Action.Prog.to_dyn t.ocamlopt)
    ; ("ocamldep", Action.Prog.to_dyn t.ocamldep)
    ; ("ocamlmklib", Action.Prog.to_dyn t.ocamlmklib)
    ; ("env", Env.to_dyn (Env.diff t.env Env.initial))
    ; ("findlib_paths", list path t.findlib_paths)
    ; ("arch_sixtyfour", Bool t.arch_sixtyfour)
    ; ( "natdynlink_supported"
      , Bool (Dynlink_supported.By_the_os.get t.lib_config.natdynlink_supported)
      )
    ; ( "supports_shared_libraries"
      , Bool (Dynlink_supported.By_the_os.get t.supports_shared_libraries) )
    ; ("ocaml_config", Ocaml_config.to_dyn t.ocaml_config)
    ]

let to_dyn_concise t : Dyn.t = Context_name.to_dyn t.name

let compare a b = Poly.compare a.name b.name

(** Wrap calls to the opam binary *)
module Opam : sig
  (** Environment for this opam switch *)
  val env :
    env:Env.t -> root:string option -> switch:string -> string Env.Map.t Memo.t
end = struct
  let opam =
    Memo.Lazy.create ~name:"context-opam" (fun () ->
        Bin.which ~path:(Env.path Env.initial) "opam" >>= function
        | None -> Utils.program_not_found "opam" ~loc:None
        | Some opam -> (
          let+ version =
            Memo.of_reproducible_fiber
              (Process.run_capture_line Strict opam
                 [ "--version"; "--color=never" ])
          in
          match Scanf.sscanf version "%d.%d.%d" (fun a b c -> (a, b, c)) with
          | Ok ((a, b, c) as v) ->
            if v < (2, 0, 0) then
              User_error.raise
                [ Pp.textf
                    "The version of opam installed on your system is too old. \
                     Dune requires at least version 2.0.0, however version \
                     %d.%d.%d is installed."
                    a b c
                ];
            opam
          | Error () ->
            User_error.raise
              [ Pp.textf "`%s config --version' returned invalid output:"
                  (Path.to_string_maybe_quoted opam)
              ; Pp.verbatim version
              ]))

  let opam_binary_exn () = Memo.Lazy.force opam

  let env =
    let impl (env, root, switch) =
      let* opam = opam_binary_exn () in
      let args =
        List.concat
          [ [ "config"; "env" ]
          ; (match root with
            | None -> []
            | Some root -> [ "--root"; root ])
          ; [ "--switch"; switch; "--sexp"; "--set-switch" ]
          ]
      in
      let+ s =
        Memo.of_reproducible_fiber (Process.run_capture ~env Strict opam args)
      in
      Dune_lang.Parser.parse_string ~fname:"<opam output>" ~mode:Single s
      |> Dune_lang.Decoder.(
           parse (enter (repeat (pair string string))) Univ_map.empty)
      |> Env.Map.of_list_multi
      |> Env.Map.mapi ~f:(fun var values ->
             match List.rev values with
             | [] -> assert false
             | [ x ] -> x
             | x :: _ ->
               User_warning.emit
                 [ Pp.textf
                     "variable %S present multiple times in the output of:" var
                 ; Pp.tag User_message.Style.Details
                     (Pp.text
                        (String.concat ~sep:" "
                           (List.map ~f:String.quote_for_shell
                              (Path.to_string opam :: args))))
                 ];
               x)
    in
    let module Input = struct
      type t = Env.t * string option * string

      let equal (env_a, root_a, switch_a) (env_b, root_b, switch_b) =
        Env.equal env_a env_b
        && Option.equal String.equal root_a root_b
        && String.equal switch_a switch_b

      let hash (env, root, switch) = Poly.hash (Env.hash env, root, switch)

      let to_dyn (env, root, switch) =
        Dyn.Tuple [ Env.to_dyn env; Dyn.(option string root); String switch ]
    end in
    let memo =
      Memo.create "opam-env" impl
        ~cutoff:(Env.Map.equal ~equal:String.equal)
        ~input:(module Input)
    in
    fun ~env ~root ~switch -> Memo.exec memo (env, root, switch)
end

let ocamlpath_sep =
  if Sys.cygwin then (* because that's what ocamlfind expects *)
    ';'
  else Bin.path_sep

module Build_environment_kind = struct
  (* Heuristics to detect the current environment *)

  type t =
    | Cross_compilation_using_findlib_toolchain of Context_name.t
    | Hardcoded_path of string list
    | Opam2_environment of string (* opam switch prefix *)
    | Unknown

  let opam_switch_prefix_var_name = "OPAM_SWITCH_PREFIX"

  let query ~(kind : Kind.t) ~findlib_toolchain ~env =
    let opam_prefix = Env.get env opam_switch_prefix_var_name in
    match findlib_toolchain with
    | Some s -> Cross_compilation_using_findlib_toolchain s
    | None -> (
      match kind with
      | Opam _ -> (
        match opam_prefix with
        | Some s -> Opam2_environment s
        | None ->
          (* This is unreachable because we check in [create_for_opam] that opam
             sets this variable *)
          assert false)
      | Default -> (
        match Setup.library_path with
        | _ :: _ as l -> Hardcoded_path l
        | [] -> (
          match opam_prefix with
          | Some s -> Opam2_environment s
          | None -> Unknown)))
end

let ocamlfind_printconf_path ~env ~ocamlfind ~toolchain =
  let args =
    let args = [ "printconf"; "path" ] in
    match toolchain with
    | None -> args
    | Some s -> "-toolchain" :: Context_name.to_string s :: args
  in
  let+ l =
    Memo.of_reproducible_fiber
      (Process.run_capture_lines ~env Strict ocamlfind args)
  in
  List.map l ~f:Path.of_filename_relative_to_initial_cwd

let check_fdo_support has_native ocfg ~name =
  let version = Ocaml.Version.of_ocaml_config ocfg in
  let version_string = Ocaml_config.version_string ocfg in
  let err () =
    User_error.raise
      [ Pp.textf
          "fdo requires ocamlopt version >= 4.10, current version is %s \
           (context: %s)"
          (Context_name.to_string name)
          version_string
      ]
  in
  if not has_native then err ();
  if Ocaml_config.is_dev_version ocfg then
    ( (* Allows fdo to be invoked with any dev version of the compiler. This is
         experimental and will be removed when ocamlfdo is fully integrated into
         the toolchain. When using a dev version of ocamlopt that does not
         support the required options, fdo builds will fail because the compiler
         won't recognize the options. Normals builds won't be affected. *) )
  else if not (Ocaml.Version.supports_split_at_emit version) then
    if not (Ocaml.Version.supports_function_sections version) then err ()
    else
      User_warning.emit
        [ Pp.textf
            "fdo requires ocamlopt version >= 4.10, current version %s has \
             partial support. Some optimizations are disabled! (context: %s)"
            (Context_name.to_string name)
            version_string
        ]

let create ~(kind : Kind.t) ~path ~env ~env_nodes ~name ~merlin ~targets
    ~host_context ~host_toolchain ~profile ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let prog_not_found_in_path prog =
    Utils.program_not_found prog ~context:name ~loc:None
  in
  let which = Program.which ~path in
  let which_exn x =
    which x >>| function
    | None -> prog_not_found_in_path x
    | Some x -> x
  in
  let findlib_config_path =
    Memo.lazy_ ~cutoff:Path.equal (fun () ->
        let* fn = which_exn "ocamlfind" in
        (* When OCAMLFIND_CONF is set, "ocamlfind printconf" does print the
           contents of the variable, but "ocamlfind printconf conf" still prints
           the configuration file set at the configuration time of ocamlfind,
           sigh... *)
        (match Env.get env "OCAMLFIND_CONF" with
        | Some s -> Memo.return s
        | None ->
          Memo.of_reproducible_fiber
            (Process.run_capture_line ~env Strict fn [ "printconf"; "conf" ]))
        >>| Path.of_filename_relative_to_initial_cwd)
  in
  let create_one ~(name : Context_name.t) ~implicit ~findlib_toolchain ~host
      ~merlin =
    let* findlib_config =
      match findlib_toolchain with
      | None -> Memo.return None
      | Some toolchain ->
        let* path = Memo.Lazy.force findlib_config_path in
        let toolchain = Context_name.to_string toolchain in
        let context = Context_name.to_string name in
        let+ config = Findlib.Config.load path ~toolchain ~context in
        Some config
    in
    let get_tool_using_findlib_config prog =
      match
        Option.bind findlib_config ~f:(fun conf -> Findlib.Config.get conf prog)
      with
      | None -> Memo.return None
      | Some s -> (
        match Filename.analyze_program_name s with
        | In_path -> which s
        | Relative_to_current_dir ->
          User_error.raise
            [ Pp.textf
                "The effective Findlib configuration specifies the relative \
                 path %S for the program %S. This is currently not supported."
                s prog
            ]
        | Absolute ->
          Memo.return (Some (Path.of_filename_relative_to_initial_cwd s)))
    in
    let* ocamlc =
      get_tool_using_findlib_config "ocamlc" >>= function
      | Some x -> Memo.return x
      | None -> (
        which "ocamlc" >>| function
        | Some x -> x
        | None -> prog_not_found_in_path "ocamlc")
    in
    let dir = Path.parent_exn ocamlc in
    let get_ocaml_tool prog =
      get_tool_using_findlib_config prog >>= function
      | Some x -> Memo.return (Ok x)
      | None -> (
        Program.best_path dir prog >>| function
        | Some p -> Ok p
        | None ->
          let hint =
            sprintf "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
              (Path.to_string dir) (Path.to_string dir) prog
              (Context_name.to_string name)
          in
          Error
            (Action.Prog.Not_found.create ~context:name ~program:prog ~loc:None
               ~hint ()))
    in
    let build_dir = Context_name.build_dir name in
    let ocamlpath =
      match
        let var = "OCAMLPATH" in
        match (kind, findlib_toolchain) with
        | Default, None -> Env.get env var
        | _ -> (
          (* If we are not in the default context, we can only use the OCAMLPATH
             variable if it is specific to this build context *)
          (* CR-someday diml: maybe we should actually clear OCAMLPATH in other
             build contexts *)
          match (Env.get env var, Env.get Env.initial var) with
          | None, None -> None
          | Some s, None -> Some s
          | None, Some _ -> None
          | Some x, Some y -> Option.some_if (x <> y) x)
      with
      | None -> []
      | Some s -> Bin.parse_path s ~sep:ocamlpath_sep
    in
    let default_library_search_path () =
      match Build_environment_kind.query ~kind ~findlib_toolchain ~env with
      | Cross_compilation_using_findlib_toolchain toolchain ->
        let* ocamlfind = which_exn "ocamlfind" in
        let env = Env.remove env ~var:"OCAMLPATH" in
        ocamlfind_printconf_path ~env ~ocamlfind ~toolchain:(Some toolchain)
      | Hardcoded_path l ->
        Memo.return (List.map l ~f:Path.of_filename_relative_to_initial_cwd)
      | Opam2_environment opam_prefix ->
        let p = Path.of_filename_relative_to_initial_cwd opam_prefix in
        let p = Path.relative p "lib" in
        Memo.return [ p ]
      | Unknown -> Memo.return [ Path.relative (Path.parent_exn dir) "lib" ]
    in
    let ocaml_config_ok_exn = function
      | Ok x -> x
      | Error (Ocaml_config.Origin.Ocamlc_config, msg) ->
        User_error.raise
          [ Pp.textf "Failed to parse the output of '%s -config':"
              (Path.to_string ocamlc)
          ; Pp.text msg
          ]
      | Error (Makefile_config file, msg) ->
        User_error.raise ~loc:(Loc.in_file file) [ Pp.text msg ]
    in
    let* default_ocamlpath, (ocaml_config_vars, ocfg) =
      Memo.fork_and_join default_library_search_path (fun () ->
          let+ lines =
            Memo.of_reproducible_fiber
              (Process.run_capture_lines ~env Strict ocamlc [ "-config" ])
          in
          ocaml_config_ok_exn
            (match Ocaml_config.Vars.of_lines lines with
            | Ok vars ->
              let open Result.O in
              let+ ocfg = Ocaml_config.make vars in
              (vars, ocfg)
            | Error msg -> Error (Ocamlc_config, msg)))
    in
    let stdlib_dir = Path.of_string (Ocaml_config.standard_library ocfg) in
    let version = Ocaml.Version.of_ocaml_config ocfg in
    let default_ocamlpath =
      if Ocaml.Version.has_META_files version then
        stdlib_dir :: default_ocamlpath
      else default_ocamlpath
    in
    let findlib_paths = ocamlpath @ default_ocamlpath in
    let env =
      (* See comment in ansi_color.ml for setup_env_for_colors. For versions
         where OCAML_COLOR is not supported, but 'color' is in OCAMLPARAM, use
         the latter. If 'color' is not supported, we just don't force colors
         with 4.02. *)
      if
        !Clflags.capture_outputs
        && Lazy.force Ansi_color.stderr_supports_color
        && Ocaml.Version.supports_color_in_ocamlparam version
        && not (Ocaml.Version.supports_ocaml_color version)
      then
        let value =
          match Env.get env "OCAMLPARAM" with
          | None -> "color=always,_"
          | Some s -> "color=always," ^ s
        in
        Env.add env ~var:"OCAMLPARAM" ~value
      else env
    in
    let env =
      let extend_var var ?(path_sep = Bin.path_sep) v =
        let v = Path.to_absolute_filename (Path.build v) in
        match Env.get env var with
        | None -> (var, v)
        | Some prev -> (var, sprintf "%s%c%s" v path_sep prev)
      in
      let vars =
        let local_lib_root = Local_install_path.lib_root ~context:name in
        [ extend_var "CAML_LD_LIBRARY_PATH"
            (Path.Build.relative
               (Local_install_path.dir ~context:name)
               "lib/stublibs")
        ; extend_var "OCAMLPATH" ~path_sep:ocamlpath_sep local_lib_root
        ; ("DUNE_OCAML_STDLIB", Ocaml_config.standard_library ocfg)
        ; ( "DUNE_OCAML_HARDCODED"
          , String.concat
              ~sep:(Char.escaped ocamlpath_sep)
              (List.map ~f:Path.to_string default_ocamlpath) )
        ; extend_var "OCAMLTOP_INCLUDE_PATH"
            (Path.Build.relative local_lib_root "toplevel")
        ; extend_var "OCAMLFIND_IGNORE_DUPS_IN" ~path_sep:ocamlpath_sep
            local_lib_root
        ; extend_var "MANPATH" (Local_install_path.man_dir ~context:name)
        ; ("INSIDE_DUNE", Path.to_absolute_filename (Path.build build_dir))
        ; ( "DUNE_SOURCEROOT"
          , Path.to_absolute_filename (Path.source Path.Source.root) )
        ]
      in
      Env.extend env ~vars:(Env.Map.of_list_exn vars)
      |> Env.update ~var:"PATH" ~f:(fun _ ->
             match host with
             | None ->
               let _key, path =
                 Local_install_path.bin_dir ~context:name |> extend_var "PATH"
               in
               Some path
             | Some host -> Env.get host.env "PATH")
      |> Env.extend_env
           (Option.value ~default:Env.empty
              (Option.map findlib_config ~f:Findlib.Config.env))
      |> Env.extend_env (Env_nodes.extra_env ~profile env_nodes)
    in
    let natdynlink_supported = Ocaml_config.natdynlink_supported ocfg in
    let arch_sixtyfour = Ocaml_config.word_size ocfg = 64 in
    let* ocamlopt = get_ocaml_tool "ocamlopt" in
    let lib_config =
      { Lib_config.has_native = Result.is_ok ocamlopt
      ; ext_obj = Ocaml_config.ext_obj ocfg
      ; ext_lib = Ocaml_config.ext_lib ocfg
      ; os_type = Ocaml_config.os_type ocfg
      ; architecture = Ocaml_config.architecture ocfg
      ; system = Ocaml_config.system ocfg
      ; model = Ocaml_config.model ocfg
      ; ext_dll = Ocaml_config.ext_dll ocfg
      ; natdynlink_supported =
          Dynlink_supported.By_the_os.of_bool natdynlink_supported
      ; stdlib_dir
      ; ccomp_type = Ocaml_config.ccomp_type ocfg
      ; profile
      ; ocaml_version_string = Ocaml_config.version_string ocfg
      ; ocaml_version = Ocaml.Version.of_ocaml_config ocfg
      ; instrument_with
      ; context_name = name
      }
    in
    if Option.is_some fdo_target_exe then
      check_fdo_support lib_config.has_native ocfg ~name;
    let* ocaml =
      let program = "ocaml" in
      which program >>| function
      | Some s -> Ok s
      | None ->
        Error (Action.Prog.Not_found.create ~context:name ~program ~loc:None ())
    and* ocamldep = get_ocaml_tool "ocamldep"
    and* ocamlmklib = get_ocaml_tool "ocamlmklib"
    and* ocamlobjinfo = get_ocaml_tool "ocamlobjinfo" in
    let ocaml_bin = dir in
    let supports_shared_libraries =
      Ocaml_config.supports_shared_libraries ocfg
    in
    let dynamically_linked_foreign_archives =
      supports_shared_libraries && dynamically_linked_foreign_archives
    in
    let make =
      let make = Memo.lazy_ (fun () -> which "make") in
      match Sys.unix with
      | false -> make
      | true ->
        Memo.lazy_ (fun () ->
            let* res = which "gmake" in
            match res with
            | Some _ as s -> Memo.return s
            | None -> Memo.Lazy.force make)
    in
    let t =
      let build_context =
        Build_context.create ~name ~host:(Option.map host ~f:(fun c -> c.name))
      in
      { name
      ; implicit
      ; kind
      ; profile
      ; merlin
      ; fdo_target_exe
      ; dynamically_linked_foreign_archives
      ; env_nodes
      ; for_host = host
      ; build_dir
      ; path
      ; toplevel_path =
          Option.map
            (Env.get env "OCAML_TOPLEVEL_PATH")
            ~f:Path.of_filename_relative_to_initial_cwd
      ; ocaml_bin
      ; ocaml
      ; ocamlc
      ; ocamlopt
      ; ocamldep
      ; ocamlmklib
      ; ocamlobjinfo
      ; env
      ; findlib_paths
      ; findlib_toolchain
      ; default_ocamlpath
      ; arch_sixtyfour
      ; stdlib_dir
      ; ocaml_config = ocfg
      ; ocaml_config_vars
      ; version
      ; supports_shared_libraries =
          Dynlink_supported.By_the_os.of_bool supports_shared_libraries
      ; lib_config
      ; build_context
      ; make
      }
    in
    if Ocaml.Version.supports_response_file version then (
      let set prog =
        Response_file.set ~prog (Zero_terminated_strings "-args0")
      in
      Result.iter t.ocaml ~f:set;
      set t.ocamlc;
      Result.iter t.ocamlopt ~f:set;
      Result.iter t.ocamldep ~f:set;
      if Ocaml.Version.ocamlmklib_supports_response_file version then
        Result.iter ~f:set t.ocamlmklib);
    Memo.return t
  in
  let implicit =
    not
      (List.mem targets ~equal:Workspace.Context.Target.equal
         Workspace.Context.Target.Native)
  in
  let* native =
    create_one ~host:host_context ~findlib_toolchain:host_toolchain ~implicit
      ~name ~merlin
  in
  let+ others =
    Memo.parallel_map targets ~f:(function
      | Native -> Memo.return None
      | Named findlib_toolchain ->
        let name = Context_name.target name ~toolchain:findlib_toolchain in
        create_one ~implicit:false ~name ~host:(Some native) ~merlin:false
          ~findlib_toolchain:(Some findlib_toolchain)
        >>| Option.some)
  in
  native :: List.filter_opt others

let which t fname = Program.which ~path:t.path fname

let extend_paths t ~env =
  let t =
    let f (var, t) =
      let parse ~loc:_ s = s in
      let standard = Env.path env |> List.map ~f:Path.to_string in
      (var, Ordered_set_lang.eval t ~parse ~standard ~eq:String.equal)
    in
    List.map ~f t
  in
  let vars =
    let to_absolute_filename s =
      Path.of_string s |> Path.to_absolute_filename
    in
    let sep = String.make 1 Bin.path_sep in
    let env = Env.Map.of_list_exn t in
    let f l = String.concat ~sep (List.map ~f:to_absolute_filename l) in
    Env.Map.map ~f env
  in
  Env.extend ~vars env

let default ~merlin ~env_nodes ~env ~targets ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let path = Env.path env in
  create ~kind:Default ~path ~env ~env_nodes ~merlin ~targets ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with

let create_for_opam ~loc ~root ~env ~env_nodes ~targets ~profile ~switch ~name
    ~merlin ~host_context ~host_toolchain ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let* vars = Opam.env ~env ~root ~switch in
  if not (Env.Map.mem vars Build_environment_kind.opam_switch_prefix_var_name)
  then
    User_error.raise ~loc
      [ Pp.textf
          "opam doesn't set the environment variable %s. I cannot create an \
           opam build context without opam setting this variable."
          Build_environment_kind.opam_switch_prefix_var_name
      ];
  let path =
    match Env.Map.find vars "PATH" with
    | None -> Env.path env
    | Some s -> Bin.parse_path s
  in
  let env = Env.extend env ~vars in
  create
    ~kind:(Opam { root; switch })
    ~profile ~targets ~path ~env ~env_nodes ~name ~merlin ~host_context
    ~host_toolchain ~fdo_target_exe ~dynamically_linked_foreign_archives
    ~instrument_with

module rec Instantiate : sig
  val instantiate : Context_name.t -> t list Memo.t
end = struct
  let instantiate_impl name : t list Memo.t =
    let env = Global.env () in
    let* workspace = Workspace.workspace () in
    let context =
      List.find_exn workspace.contexts ~f:(fun ctx ->
          Context_name.equal (Workspace.Context.name ctx) name)
    in
    let* host_context =
      match Workspace.Context.host_context context with
      | None -> Memo.return None
      | Some context_name -> (
        let+ contexts = Instantiate.instantiate context_name in
        match contexts with
        | [ x ] -> Some x
        | [] -> assert false (* checked by workspace *)
        | _ :: _ -> assert false)
      (* target cannot be host *)
    in
    let env_nodes =
      let context = Workspace.Context.env context in
      { Env_nodes.context; workspace = workspace.env }
    in
    match context with
    | Default
        { targets
        ; name
        ; host_context = _
        ; profile
        ; env = _
        ; toolchain
        ; paths
        ; loc = _
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        ; merlin = _
        } ->
      let merlin =
        workspace.merlin_context = Some (Workspace.Context.name context)
      in
      let host_toolchain : Context_name.t option =
        match toolchain with
        | Some _ -> toolchain
        | None ->
          let open Option.O in
          let+ name = Env.get env "OCAMLFIND_TOOLCHAIN" in
          Context_name.parse_string_exn (Loc.none, name)
      in
      let env = extend_paths ~env paths in
      default ~env ~env_nodes ~profile ~targets ~name ~merlin ~host_context
        ~host_toolchain ~fdo_target_exe ~dynamically_linked_foreign_archives
        ~instrument_with
    | Opam
        { base =
            { targets
            ; name
            ; host_context = _
            ; profile
            ; env = _
            ; toolchain
            ; paths
            ; loc
            ; fdo_target_exe
            ; dynamically_linked_foreign_archives
            ; instrument_with
            ; merlin
            }
        ; switch
        ; root
        } ->
      let env = extend_paths ~env paths in
      create_for_opam ~loc ~root ~env_nodes ~env ~profile ~switch ~name ~merlin
        ~targets ~host_context ~host_toolchain:toolchain ~fdo_target_exe
        ~dynamically_linked_foreign_archives ~instrument_with

  let memo =
    Memo.create "instantiate-context"
      ~input:(module Context_name)
      instantiate_impl

  let instantiate name = Memo.exec memo name
end

module DB = struct
  let all =
    let impl () =
      let* workspace = Workspace.workspace () in
      let+ contexts =
        Memo.parallel_map workspace.contexts ~f:(fun c ->
            Instantiate.instantiate (Workspace.Context.name c))
      in
      let all = List.concat contexts in
      List.iter all ~f:(fun t ->
          let open Pp.O in
          Log.info
            [ Pp.box ~indent:1
                (Pp.text "Dune context:" ++ Pp.cut ++ Dyn.pp (to_dyn t))
            ]);
      all
    in
    let memo = Memo.lazy_ ~name:"build-contexts" impl in
    fun () -> Memo.Lazy.force memo

  let get =
    let memo =
      Memo.create "context-db-get"
        ~input:(module Context_name)
        (fun name ->
          let+ contexts = all () in
          List.find_exn contexts ~f:(fun c -> Context_name.equal name c.name))
    in
    Memo.exec memo

  let by_dir dir =
    let context =
      match Dune_engine.Dpath.analyse_dir (Path.build dir) with
      | Build
          ( Install (With_context (name, _))
          | Regular (With_context (name, _))
          | Anonymous_action (With_context (name, _)) ) -> name
      | _ ->
        Code_error.raise "directory does not have an associated context"
          [ ("dir", Path.Build.to_dyn dir) ]
    in
    get context
end

let compiler t (mode : Ocaml.Mode.t) =
  match mode with
  | Byte -> Ok t.ocamlc
  | Native -> t.ocamlopt

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Ok _ -> Native
  | Error _ -> Byte

let cc_g (ctx : t) =
  match ctx.lib_config.ccomp_type with
  | Msvc -> []
  | Other _ -> [ "-g" ]

let name t = t.name

let has_native t = Result.is_ok t.ocamlopt

let lib_config t = t.lib_config

let map_exe (context : t) =
  match context.for_host with
  | None -> fun exe -> exe
  | Some (host : t) -> (
    fun exe ->
      match Path.extract_build_context_dir exe with
      | Some (dir, exe) when Path.equal dir (Path.build context.build_dir) ->
        Path.append_source (Path.build host.build_dir) exe
      | _ -> exe)

let host t = Option.value ~default:t t.for_host

let roots t =
  let module Roots = Install.Section.Paths.Roots in
  let prefix_roots =
    match Env.get t.env Build_environment_kind.opam_switch_prefix_var_name with
    | None ->
      { Roots.lib_root = None
      ; libexec_root = None
      ; bin = None
      ; sbin = None
      ; etc_root = None
      ; doc_root = None
      ; share_root = None
      ; man = None
      }
    | Some prefix ->
      let prefix = Path.of_filename_relative_to_initial_cwd prefix in
      Roots.opam_from_prefix prefix |> Roots.map ~f:(fun s -> Some s)
  in
  match t.kind with
  | Default ->
    let setup_roots = Roots.map ~f:(Option.map ~f:Path.of_string) Setup.roots in
    Roots.first_has_priority setup_roots prefix_roots
  | Opam _ -> prefix_roots

let dot_dune_dir t = Path.Build.relative t.build_dir ".dune"

let configurator_v1 t = Path.Build.relative (dot_dune_dir t) "configurator"

let configurator_v2 t = Path.Build.relative (dot_dune_dir t) "configurator.v2"

(* We store this so that library such as dune-configurator can read things
   runtime. Ideally, this should be created on-demand if we run a program linked
   against configurator, however we currently don't support this kind of
   "runtime dependencies" so we just do it eagerly. *)
let gen_configurator_rules t =
  let ocamlc = Path.to_absolute_filename t.ocamlc in
  let ocaml_config_vars = Ocaml_config.Vars.to_list t.ocaml_config_vars in
  let* () =
    let fn = configurator_v1 t in
    Rules.Produce.rule
      (Rule.make ~context:None ~targets:(Targets.File.create fn)
         (let open Action_builder.O in
         let+ () = Action_builder.return () in
         Action.Full.make
           (Action.write_file fn
              (List.map
                 ~f:(fun x -> Dune_lang.to_string x ^ "\n")
                 (let open Dune_lang.Encoder in
                 record_fields
                   [ field "ocamlc" string ocamlc
                   ; field_l "ocaml_config_vars" (pair string string)
                       ocaml_config_vars
                   ])
              |> String.concat ~sep:""))))
  in
  let fn = configurator_v2 t in
  Rules.Produce.rule
    (Rule.make ~context:None ~targets:(Targets.File.create fn)
       (let open Action_builder.O in
       let+ () = Action_builder.return () in
       Action.Full.make
         (Action.write_file fn
            (Csexp.to_string
               (let open Sexp in
               let ocaml_config_vars =
                 Sexp.List
                   (List.map ocaml_config_vars ~f:(fun (k, v) ->
                        List [ Atom k; Atom v ]))
               in
               List
                 [ List [ Atom "ocamlc"; Atom ocamlc ]
                 ; List [ Atom "ocaml_config_vars"; ocaml_config_vars ]
                 ])))))

let force_configurator_files =
  Memo.lazy_ (fun () ->
      let* ctxs = DB.all () in
      let files =
        List.concat_map ctxs ~f:(fun t ->
            [ Path.build (configurator_v1 t); Path.build (configurator_v2 t) ])
      in
      Memo.parallel_iter files ~f:Build_system.build_file)

let make t = Memo.Lazy.force t.make
