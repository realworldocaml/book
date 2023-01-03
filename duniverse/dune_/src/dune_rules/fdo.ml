open Import
module CC = Compilation_context

type phase =
  | All
  | Compile
  | Emit

let linear_ext = ".cmir-linear"

let linear_fdo_ext = linear_ext ^ "-fdo"

let fdo_profile s = Path.extend_basename s ~suffix:".fdo-profile"

let linker_script s = Path.extend_basename s ~suffix:".linker-script"

let phase_flags = function
  | None -> []
  | Some All -> [ "-g"; "-function-sections" ]
  | Some Compile ->
    [ "-g"; "-stop-after"; "scheduling"; "-save-ir-after"; "scheduling" ]
  | Some Emit -> [ "-g"; "-start-from"; "emit"; "-function-sections" ]

(* CR-soon gyorsh: add a rule to use profile with c/cxx profile if available,
   similarly to opt_rule for ocaml modules. The profile will have to be
   generated externally from perf data for example using google's autofdo
   toolset: create_gcov for gcc or create_llvm_prof for llvm. *)
let c_flags (ctx : Context.t) =
  match ctx.fdo_target_exe with
  | None -> []
  | Some _ -> [ "-ffunction-sections" ]

let cxx_flags = c_flags

(* Location of ocamlfdo binary tool is independent of the module, but may depend
   on the context. *)
let ocamlfdo_binary sctx dir =
  Super_context.resolve_program sctx ~dir ~loc:None "ocamlfdo"
    ~hint:"opam install ocamlfdo"

(* FDO flags are context dependent. *)
let get_flags var =
  let f (ctx : Context.t) =
    Env.get ctx.env var |> Option.value ~default:""
    |> String.extract_blank_separated_words |> Memo.return
  in
  let memo =
    Memo.create var ~input:(module Context) ~cutoff:(List.equal String.equal) f
  in
  Memo.exec memo

let ocamlfdo_flags = get_flags "OCAMLFDO_FLAGS"

module Mode = struct
  type t =
    | If_exists
    | Always
    | Never

  let to_string = function
    | If_exists -> "if-exists"
    | Always -> "always"
    | Never -> "never"

  let default = If_exists

  let all = [ If_exists; Never; Always ]

  let var = "OCAMLFDO_USE_PROFILE"

  let of_context (ctx : Context.t) =
    match Env.get ctx.env var with
    | None -> default
    | Some v -> (
      match List.find_opt ~f:(fun s -> String.equal v (to_string s)) all with
      | Some v -> v
      | None ->
        User_error.raise
          [ Pp.textf
              "Failed to parse environment variable: %s=%s\n\
               Permitted values: if-exists always never\n\
               Default: %s" var v (to_string default)
          ])
end

let get_profile =
  (* The dependency on the existence of profile file in source should be
     detected automatically by Memo. *)
  let f (ctx : Context.t) =
    let path = ctx.fdo_target_exe |> Option.value_exn |> fdo_profile in
    let profile_exists =
      Memo.lazy_ (fun () ->
          match Path.as_in_source_tree path with
          | None -> Memo.return false
          | Some path -> Source_tree.file_exists path)
    in
    let open Memo.O in
    let+ use_profile =
      match Mode.of_context ctx with
      | If_exists -> Memo.Lazy.force profile_exists
      | Always -> (
        let+ profile_exists = Memo.Lazy.force profile_exists in
        match profile_exists with
        | true -> true
        | false ->
          User_error.raise
            [ Pp.textf "%s=%s but profile file %s does not exist." Mode.var
                (Mode.to_string Always) (Path.to_string path)
            ])
      | Never -> Memo.return false
    in
    if use_profile then Some path else None
  in
  let memo =
    Memo.create Mode.var f ~cutoff:(Option.equal Path.equal)
      ~input:(module Context)
  in
  Memo.exec memo

let opt_rule cctx m =
  let sctx = CC.super_context cctx in
  let ctx = CC.context cctx in
  let dir = CC.dir cctx in
  let obj_dir = CC.obj_dir cctx in
  let linear =
    Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx) ~ext:linear_ext
  in
  let linear_fdo =
    Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx) ~ext:linear_fdo_ext
  in
  let open Memo.O in
  let flags () =
    let open Command.Args in
    let+ get_profile = get_profile ctx in
    match get_profile with
    | Some fdo_profile_path ->
      S
        [ A "-fdo-profile"
        ; Dep fdo_profile_path
        ; As [ "-md5-unit"; "-reorder-blocks"; "opt"; "-q" ]
        ]
    | None -> As [ "-md5-unit"; "-extra-debug"; "-q" ]
  in
  let* ocamlfdo_binary = ocamlfdo_binary sctx dir
  and* ocamlfdo_flags = ocamlfdo_flags ctx in
  Super_context.add_rule sctx ~dir
    (Command.run ~dir:(Path.build dir) ocamlfdo_binary
       [ A "opt"
       ; Hidden_targets [ linear_fdo ]
       ; Dep (Path.build linear)
       ; As ocamlfdo_flags
       ; Dyn (Action_builder.of_memo (Memo.return () >>= flags))
       ])

module Linker_script = struct
  type t = Path.t Memo.t option

  let ocamlfdo_linker_script_flags = get_flags "OCAMLFDO_LINKER_SCRIPT_FLAGS"

  let linker_script_rule cctx fdo_target_exe =
    let sctx = CC.super_context cctx in
    let ctx = CC.context cctx in
    let dir = CC.dir cctx in
    let linker_script = linker_script fdo_target_exe in
    let linker_script_path =
      Path.Build.(relative ctx.build_dir (Path.to_string linker_script))
    in
    let open Memo.O in
    let flags () =
      let open Command.Args in
      let+ get_profile = get_profile ctx in
      match get_profile with
      | Some fdo_profile_path -> S [ A "-fdo-profile"; Dep fdo_profile_path ]
      | None -> As []
    in
    let* ocamlfdo_binary = ocamlfdo_binary sctx dir
    and* ocamlfdo_linker_script_flags = ocamlfdo_linker_script_flags ctx in
    let+ () =
      Super_context.add_rule sctx ~dir
        (Command.run ~dir:(Path.build ctx.build_dir) ocamlfdo_binary
           [ A "linker-script"
           ; A "-o"
           ; Target linker_script_path
           ; Dyn (Action_builder.of_memo (Memo.return () >>= flags))
           ; A "-q"
           ; As ocamlfdo_linker_script_flags
           ])
    in
    linker_script

  let create cctx name =
    let ctx = CC.context cctx in
    match ctx.fdo_target_exe with
    | None -> None
    | Some fdo_target_exe ->
      if
        Path.equal name fdo_target_exe
        && (Ocaml.Version.supports_function_sections ctx.version
           || Ocaml_config.is_dev_version ctx.ocaml_config)
      then Some (linker_script_rule cctx fdo_target_exe)
      else None

  let flags t =
    let open Memo.O in
    let open Command.Args in
    match t with
    | None -> Memo.return (As [])
    | Some linker_script ->
      let+ linker_script = linker_script in
      S
        [ A "-ccopt"
        ; Concat ("", [ A "-Xlinker --script="; Dep linker_script ])
        ]
end
