(*$ open Ppxlib_cinaps_helpers $*)
open Import
open Utils

module Arg = Caml.Arg

let exe_name = Caml.Filename.basename Caml.Sys.executable_name

let args = ref []

let add_arg key spec ~doc = args := (key, spec, doc) :: !args

let perform_checks = ref Options.perform_checks
let perform_checks_on_extensions = ref Options.perform_checks_on_extensions
let debug_attribute_drop = ref false
let apply_list = ref None
let preprocessor = ref None
let no_merge = ref false
let request_print_passes = ref false
let request_print_transformations = ref false
let use_color = ref true
let diff_command = ref Options.diff_command
let pretty = ref false
let styler = ref None
let output_metadata_filename = ref None
let corrected_suffix = ref ".ppx-corrected"

module Lint_error = struct
  type t = Location.t * string

  let of_string loc s = (loc, s)
end

module Cookies = struct
  type t = Migrate_parsetree.Driver.cookies

  let get t name pattern =
    Option.map (Migrate_parsetree.Driver.get_cookie t name (module Ppxlib_ast.Selected_ast))
      ~f:(fun e ->
        Ast_pattern.parse pattern e.pexp_loc e Fn.id)

  let set t name expr =
    Migrate_parsetree.Driver.set_cookie t name (module Ppxlib_ast.Selected_ast) expr

  let handlers = ref []
  let add_handler f = handlers := !handlers @ [f]

  let add_simple_handler name pattern ~f =
    add_handler (fun t -> f (get t name pattern))

  let acknoledge_cookies t =
    List.iter !handlers ~f:(fun f -> f t)

  let post_handlers = ref []
  let add_post_handler f = post_handlers := !post_handlers @ [f]

  let call_post_handlers t =
    List.iter !post_handlers ~f:(fun f -> f t)
end

module Transform = struct
  type t =
    { name            : string
    ; aliases         : string list
    ; impl            : (Parsetree.structure -> Parsetree.structure) option
    ; intf            : (Parsetree.signature -> Parsetree.signature) option
    ; lint_impl       : (Parsetree.structure -> Lint_error.t list) option
    ; lint_intf       : (Parsetree.signature -> Lint_error.t list) option
    ; preprocess_impl : (Parsetree.structure -> Parsetree.structure) option
    ; preprocess_intf : (Parsetree.signature -> Parsetree.signature) option
    ; enclose_impl    : (Location.t option -> Parsetree.structure * Parsetree.structure) option
    ; enclose_intf    : (Location.t option -> Parsetree.signature * Parsetree.signature) option
    ; rules           : Context_free.Rule.t list
    ; registered_at   : Caller_id.t
    }

  let has_name t name =
    (String.equal name t.name) || (List.exists ~f:(String.equal name) t.aliases)

  let all : t list ref = ref []

  let print_caller_id oc (caller_id : Caller_id.t) =
    match caller_id with
    | None -> Out_channel.output_string oc "<unknown location>"
    | Some loc -> Out_channel.fprintf oc "%s:%d" loc.filename loc.line_number
  ;;

  let register ?(extensions=[]) ?(rules=[]) ?enclose_impl ?enclose_intf
        ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf
        ?(aliases=[]) name =
    let rules =
      List.map extensions ~f:Context_free.Rule.extension @ rules
    in
    let caller_id = Caller_id.get ~skip:[Caml.__FILE__] in
    begin match List.filter !all ~f:(fun ct -> has_name ct name) with
    | [] -> ()
    | ct :: _ ->
      eprintf "Warning: code transformation %s registered twice.\n" name;
      eprintf "  - first time was at %a\n" print_caller_id ct.registered_at;
      eprintf "  - second time is at %a\n" print_caller_id caller_id;
    end;
    let ct =
      { name
      ; aliases
      ; rules
      ; enclose_impl
      ; enclose_intf
      ; impl
      ; intf
      ; lint_impl
      ; preprocess_impl
      ; preprocess_intf
      ; lint_intf
      ; registered_at = caller_id
      }
    in
    all := ct :: !all
  ;;

  let rec last prev l =
    match l with
    | [] -> prev
    | x :: l -> last x l
  ;;

  let loc_of_list ~get_loc l =
    match l with
    | [] -> None
    | x :: l ->
      let first : Location.t = get_loc x in
      let last = get_loc (last x l) in
      Some { first with loc_end = last.loc_end }
  ;;

  let merge_into_generic_mappers t ~hook ~expect_mismatch_handler ~omp_config =
    let { rules; enclose_impl; enclose_intf; impl; intf; _ } = t in
    let map =
      new Context_free.map_top_down rules
        ~generated_code_hook:hook
        ~expect_mismatch_handler
    in
    let gen_header_and_footer context whole_loc f =
      let header, footer = f whole_loc in
      (match whole_loc with
      | Some (loc : Location.t) ->
        let loc_header = { loc with loc_end   = loc.loc_start } in
        let loc_footer = { loc with loc_start = loc.loc_end   } in
        (match header with [] -> () | _ -> hook.f context loc_header (Many header));
        (match footer with [] -> () | _ -> hook.f context loc_footer (Many footer))
      | None ->
        match header @ footer with
        | [] -> ()
        | l ->
          let pos =
            { Lexing.
              pos_fname = ""
            ; pos_lnum  = 1
            ; pos_bol   = 0
            ; pos_cnum  = 0
            }
          in
          let loc = { Location. loc_start = pos; loc_end = pos; loc_ghost = false } in
          hook.f context loc (Many l));
      (header, footer)
    in
    let map_impl st =
      let st =
        let header, footer =
          match enclose_impl with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list st ~get_loc:(fun st -> st.Parsetree.pstr_loc) in
            gen_header_and_footer Structure_item whole_loc f
        in
        let file_path = File_path.get_default_path_str st in
        let base_ctxt = Expansion_context.Base.top_level ~omp_config ~file_path in
        let st = map#structure base_ctxt st in
        match header, footer with
        | [], [] -> st
        | _      -> List.concat [ header; st; footer ]
      in
      match impl with
      | None -> st
      | Some f -> f st
    in
    let map_intf sg =
      let sg =
        let header, footer =
          match enclose_intf with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list sg ~get_loc:(fun sg -> sg.Parsetree.psig_loc) in
            gen_header_and_footer Signature_item whole_loc f
        in
        let file_path = File_path.get_default_path_sig sg in
        let base_ctxt = Expansion_context.Base.top_level ~omp_config ~file_path in
        let sg = map#signature base_ctxt sg in
        match header, footer with
        | [], [] -> sg
        | _      -> List.concat [ header; sg; footer ]
      in
      match intf with
      | None -> sg
      | Some f -> f sg
    in
    { t with
      impl = Some map_impl
    ; intf = Some map_intf
    }

  let builtin_of_context_free_rewriters ~hook ~rules ~enclose_impl ~enclose_intf =
    merge_into_generic_mappers ~hook
      { name = "<builtin:context-free>"
      ; aliases = []
      ; impl = None
      ; intf = None
      ; lint_impl = None
      ; lint_intf = None
      ; preprocess_impl = None
      ; preprocess_intf = None
      ; enclose_impl
      ; enclose_intf
      ; rules
      ; registered_at = Caller_id.get ~skip:[]
      }

  let partition_transformations ts =
    (`Linters
       (List.filter_map ts ~f:(fun t ->
          if Option.is_some t.lint_impl || Option.is_some t.lint_intf then
            Some
              { name = Printf.sprintf "<lint:%s>" t.name
              ; aliases = []
              ; impl = None
              ; intf = None
              ; lint_impl = t.lint_impl
              ; lint_intf = t.lint_intf
              ; enclose_impl = None
              ; enclose_intf = None
              ; preprocess_impl = None
              ; preprocess_intf = None
              ; rules = []
              ; registered_at = t.registered_at
              }
          else
            None)),
     `Preprocess
       (List.filter_map ts ~f:(fun t ->
          if Option.is_some t.preprocess_impl || Option.is_some t.preprocess_impl
          then
            Some
              { name = Printf.sprintf "<preprocess:%s>" t.name
              ; aliases = []
              ; impl = t.preprocess_impl
              ; intf = t.preprocess_intf
              ; lint_impl = None
              ; lint_intf = None
              ; enclose_impl = None
              ; enclose_intf = None
              ; preprocess_impl = None
              ; preprocess_intf = None
              ; rules = []
              ; registered_at = t.registered_at
              }
          else
            None)),
     `Rest
       (List.map ts ~f:(fun t ->
          { t with
            lint_impl = None
          ; lint_intf = None
          ; preprocess_impl = None
          ; preprocess_intf = None
          })))
end

let register_transformation = Transform.register

let register_code_transformation ~name ?(aliases=[]) ~impl ~intf =
  register_transformation name ~impl ~intf ~aliases
;;

let register_transformation_using_ocaml_current_ast ?impl ?intf ?aliases name =
  let impl = Option.map impl ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Structure) in
  let intf = Option.map intf ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Signature) in
  register_transformation ?impl ?intf ?aliases name

let debug_dropped_attribute name ~old_dropped ~new_dropped =
  let print_diff what a b =
    let diff =
      List.filter a ~f:(fun (name : _ Loc.t) ->
        not (List.exists b ~f:(fun (name' : _ Location.loc) -> phys_equal name.txt name'.txt)))
    in
    if not (List.is_empty diff) then begin
      eprintf "The following attributes %s after applying %s:\n"
        what name;
      List.iter diff ~f:(fun { Location. txt; loc } ->
        Caml.Format.eprintf "- %a: %s\n" Location.print loc txt);
      Caml.Format.eprintf "@."
    end
  in
  print_diff "disappeared" new_dropped old_dropped;
  print_diff "reappeared"  old_dropped new_dropped
;;

let get_whole_ast_passes ~hook ~expect_mismatch_handler ~omp_config =
  let cts =
    match !apply_list with
    | None -> List.rev !Transform.all
    | Some names ->
      List.map names ~f:(fun name ->
        List.find_exn !Transform.all ~f:(fun (ct : Transform.t) ->
          Transform.has_name ct name))
  in
  let (`Linters linters, `Preprocess preprocess, `Rest cts) =
    Transform.partition_transformations cts in
  (* Allow only one preprocessor to assure deterministic order *)
  if (List.length preprocess) > 1 then begin
    let pp = String.concat ~sep:", " (List.map preprocess ~f:(fun t -> t.name)) in
    let err = Printf.sprintf "At most one preprocessor is allowed, while got: %s" pp in
    failwith err
  end;
  let cts =
    if !no_merge then
      List.map cts ~f:(Transform.merge_into_generic_mappers ~hook ~omp_config
                         ~expect_mismatch_handler)
    else begin
      let get_enclosers ~f =
        List.filter_map cts ~f:(fun (ct : Transform.t) ->
          match f ct with
          | None -> None
          | Some x -> Some (ct.name, x))
        (* Sort them to ensure deterministic ordering *)
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
        |> List.map ~f:snd
      in

      let rules =
        List.map cts ~f:(fun (ct : Transform.t) -> ct.rules) |> List.concat
      and impl_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_impl)
      and intf_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_intf)
      in
      match rules, impl_enclosers, intf_enclosers with
      | [], [], [] -> cts
      | _              ->
        let merge_encloser = function
          | [] -> None
          | enclosers -> Some (fun loc ->
            let headers, footers =
              List.map enclosers ~f:(fun f -> f loc)
              |> List.unzip
            in
            let headers = List.concat headers in
            let footers = List.concat (List.rev footers) in
            (headers, footers))
        in
        Transform.builtin_of_context_free_rewriters ~rules ~hook ~expect_mismatch_handler
          ~enclose_impl:(merge_encloser impl_enclosers)
          ~enclose_intf:(merge_encloser intf_enclosers)
          ~omp_config
        :: cts
    end
  in linters @ preprocess @ List.filter cts ~f:(fun (ct : Transform.t) ->
    match ct.impl, ct.intf with
    | None, None -> false
    | _          -> true)
;;

let apply_transforms
      ~omp_config ~field ~lint_field ~dropped_so_far ~hook ~expect_mismatch_handler x =
  let cts = get_whole_ast_passes ~omp_config ~hook ~expect_mismatch_handler in
  let x, _dropped, lint_errors =
  List.fold_left cts ~init:(x, [], [])
    ~f:(fun (x, dropped, lint_errors) (ct : Transform.t) ->
      let lint_errors =
        match lint_field ct with
        | None -> lint_errors
        | Some f -> lint_errors @ f x
      in
      match field ct with
      | None -> (x, dropped, lint_errors)
      | Some f ->
        let x = f x in
        let dropped =
          if !debug_attribute_drop then begin
            let new_dropped = dropped_so_far x in
            debug_dropped_attribute ct.name ~old_dropped:dropped ~new_dropped;
            new_dropped
          end else
            []
        in
        (x, dropped, lint_errors))
  in
  (x, List.map lint_errors ~f:(fun (loc, s) -> Common.attribute_of_warning loc s))
;;

(* +-----------------------------------------------------------------+
   | Actual rewriting of structure/signatures                        |
   +-----------------------------------------------------------------+ *)

(* We want driver registered plugins to work with omp driver and vice-versa. To
   simplify things we do as follow:

   - we register driver as a single omp driver plugin
   - driver calls the omp driver rewriting functions, which will apply everything

   The registration with omp driver is at the end of the file.
*)

module C = struct
  type t =
    { hook                    : Context_free.Generated_code_hook.t
    ; expect_mismatch_handler : Context_free.Expect_mismatch_handler.t
    }

  type Migrate_parsetree.Driver.extra += T of t

  let default =
    { hook                    = Context_free.Generated_code_hook.nop
    ; expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop
    }

  let find (config : Migrate_parsetree.Driver.config) =
    List.find_map config.extras ~f:(function
      | T config -> Some config
      | _ -> None)
    |> Option.value ~default
end

let config ~hook ~expect_mismatch_handler =
  Migrate_parsetree.Driver.make_config ()
    ~tool_name:"ppxlib_driver"
    ~extras:[C.T { hook
                 ; expect_mismatch_handler
                 }]

let as_ppx_config () =
  Migrate_parsetree.Driver.make_config ()
    ~tool_name:(Ocaml_common.Ast_mapper.tool_name ())
    ~include_dirs:!Ocaml_common.Clflags.include_dirs
    ~load_path:(Compiler_specifics.get_load_path ())
    ~debug:!Ocaml_common.Clflags.debug
    ?for_package:!Ocaml_common.Clflags.for_package

let print_passes () =
  let hook = Context_free.Generated_code_hook.nop in
  let expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop in
  let omp_config = config ~hook ~expect_mismatch_handler in
  let cts = get_whole_ast_passes ~hook ~expect_mismatch_handler ~omp_config in
  if !perform_checks then
    printf "<builtin:freshen-and-collect-attributes>\n";
  List.iter cts ~f:(fun ct -> printf "%s\n" ct.Transform.name);
  if !perform_checks then
    begin
      printf "<builtin:check-unused-attributes>\n";
      if !perform_checks_on_extensions
      then printf "<builtin:check-unused-extensions>\n"
    end
;;

(*$*)
let real_map_structure config cookies st =
  let { C. hook; expect_mismatch_handler } = C.find config in
  Cookies.acknoledge_cookies cookies;
  if !perform_checks then begin
    Attribute.reset_checks ();
    Attribute.collect#structure st
  end;
  let st, lint_errors =
    apply_transforms st
      ~omp_config:config
      ~field:(fun (ct : Transform.t) -> ct.impl)
      ~lint_field:(fun (ct : Transform.t) -> ct.lint_impl)
      ~dropped_so_far:Attribute.dropped_so_far_structure ~hook ~expect_mismatch_handler
  in
  let st =
    match lint_errors with
    | [] -> st
    | _  ->
      List.map lint_errors ~f:(fun ({ attr_name = { loc; _ }; _} as attr) ->
        Ast_builder.Default.pstr_attribute ~loc attr)
      @ st
  in
  Cookies.call_post_handlers cookies;
  if !perform_checks then begin
    (* TODO: these two passes could be merged, we now have more passes for
       checks than for actual rewriting. *)
    Attribute.check_unused#structure st;
    if !perform_checks_on_extensions then Extension.check_unused#structure st;
    Attribute.check_all_seen ();
  end;
  st
;;

let map_structure_gen st ~config : Migrate_parsetree.Driver.some_structure =
  Migrate_parsetree.Driver.rewrite_structure
    config
    (module Ppxlib_ast.Selected_ast)
    st

let map_structure st =
  map_structure_gen st ~config:(as_ppx_config ())

(*$ str_to_sig _last_text_block *)
let real_map_signature config cookies sg =
  let { C. hook; expect_mismatch_handler } = C.find config in
  Cookies.acknoledge_cookies cookies;
  if !perform_checks then begin
    Attribute.reset_checks ();
    Attribute.collect#signature sg
  end;
  let sg, lint_errors =
    apply_transforms sg
      ~omp_config:config
      ~field:(fun (ct : Transform.t) -> ct.intf)
      ~lint_field:(fun (ct : Transform.t) -> ct.lint_intf)
      ~dropped_so_far:Attribute.dropped_so_far_signature ~hook ~expect_mismatch_handler
  in
  let sg =
    match lint_errors with
    | [] -> sg
    | _  ->
      List.map lint_errors ~f:(fun ({ attr_name = { loc; _ }; _} as attr) ->
        Ast_builder.Default.psig_attribute ~loc attr)
      @ sg
  in
  Cookies.call_post_handlers cookies;
  if !perform_checks then begin
    (* TODO: these two passes could be merged, we now have more passes for
       checks than for actual rewriting. *)
    Attribute.check_unused#signature sg;
    if !perform_checks_on_extensions then Extension.check_unused#signature sg;
    Attribute.check_all_seen ();
  end;
  sg
;;

let map_signature_gen sg ~config : Migrate_parsetree.Driver.some_signature =
  Migrate_parsetree.Driver.rewrite_signature
    config
    (module Ppxlib_ast.Selected_ast)
    sg

let map_signature sg =
  map_signature_gen sg ~config:(as_ppx_config ())

(*$*)

(* +-----------------------------------------------------------------+
   | Entry points                                                    |
   +-----------------------------------------------------------------+ *)

let mapper =
  let module Js = Ppxlib_ast.Selected_ast in
  (*$*)
  let structure _ st =
    Js.of_ocaml Structure st
    |> map_structure
    |> Migrate_parsetree.Driver.migrate_some_structure
         (module Migrate_parsetree.OCaml_current)
  in
  (*$ str_to_sig _last_text_block *)
  let signature _ sg =
    Js.of_ocaml Signature sg
    |> map_signature
    |> Migrate_parsetree.Driver.migrate_some_signature
         (module Migrate_parsetree.OCaml_current)
  in
  (*$*)
  { Ocaml_common.Ast_mapper.default_mapper with structure; signature }
;;

let as_ppx_rewriter_main argv =
  let argv = Caml.Sys.executable_name :: argv in
  let usage =
    Printf.sprintf "%s [extra_args] <infile> <outfile>" exe_name
  in
  match
    Arg.parse_argv (Array.of_list argv) (Arg.align (List.rev !args))
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      usage
  with
  | exception Arg.Bad  msg -> eprintf "%s" msg; Caml.exit 2
  | exception Arg.Help msg -> eprintf "%s" msg; Caml.exit 0
  | () -> mapper

let run_as_ppx_rewriter () =
  perform_checks := false;
  Ocaml_common.Ast_mapper.run_main as_ppx_rewriter_main;
  Caml.exit 0

let string_contains_binary_ast s =
  let test magic_number =
    String.is_prefix s ~prefix:(String.sub magic_number ~pos:0 ~len:9)
  in
  test Ast_magic.ast_intf_magic_number ||
  test Ast_magic.ast_impl_magic_number

type pp_error = { filename : string; command_line : string }
exception Pp_error of pp_error

let report_pp_error e =
  let buff = Buffer.create 128 in
  let ppf = Caml.Format.formatter_of_buffer buff in
  Caml.Format.fprintf ppf "Error while running external preprocessor@.\
                           Command line: %s@." e.command_line;
  Caml.Format.pp_print_flush ppf ();
  Buffer.contents buff

let () =
  Location.Error.register_error_of_exn
    (function
      | Pp_error e ->
        Some (Location.Error.make ~loc:(Location.in_file e.filename) ~sub:[]
                (report_pp_error e))
      | _ -> None)

let remove_no_error fn =
  try Caml.Sys.remove fn with Sys_error _ -> ()

let protectx x ~f ~finally =
  match f x with
  | v -> finally x; v
  | exception e -> finally x; raise e
;;

let with_preprocessed_file fn ~f =
  match !preprocessor with
  | None -> f fn
  | Some pp ->
    protectx (Caml.Filename.temp_file "ocamlpp" "")
      ~finally:remove_no_error
      ~f:(fun tmpfile ->
        let comm =
          Printf.sprintf "%s %s > %s"
            pp (if String.equal fn "-" then "" else Caml.Filename.quote fn)
            (Caml.Filename.quote tmpfile)
        in
        if Caml.Sys.command comm <> 0 then
          raise (Pp_error { filename = fn
                          ; command_line = comm
                          });
        f tmpfile)

let with_preprocessed_input fn ~f =
  with_preprocessed_file fn ~f:(fun fn ->
    if String.equal fn "-" then
      f stdin
    else
      In_channel.with_file fn ~f)
;;

let relocate_mapper = object
  inherit [string * string] Ast_traverse.map_with_context

  method! position (old_fn, new_fn) pos =
    if String.equal pos.pos_fname old_fn then
      { pos with pos_fname = new_fn }
    else
      pos
end

(* Set the input name globally. This is used by some ppx rewriters
   such as bisect_ppx. *)
let set_input_name name =
  Ocaml_common.Location.input_name := name

let load_input (kind : Kind.t) fn input_name ~relocate ic =
  set_input_name input_name;
  match Migrate_parsetree.Ast_io.from_channel ic with
  | Ok (ast_input_name, ast) ->
    let ast = Intf_or_impl.of_ast_io ast in
    if not (Kind.equal kind (Intf_or_impl.kind ast)) then
      Location.raise_errorf ~loc:(Location.in_file fn)
        "File contains a binary %s AST but an %s was expected"
        (Kind.describe (Intf_or_impl.kind ast))
        (Kind.describe kind);
    if String.equal ast_input_name input_name || not relocate then begin
      set_input_name ast_input_name;
      (ast_input_name, ast)
    end else
      (input_name,
       Intf_or_impl.map_with_context ast relocate_mapper
         (ast_input_name, input_name))

  | Error (Unknown_version _) ->
    Location.raise_errorf ~loc:(Location.in_file fn)
      "File is a binary ast for an unknown version of OCaml"
  | Error (Not_a_binary_ast prefix_read_from_file) ->
    (* To test if a file is an AST file, we have to read the first few bytes of the
       file. If it is not, we have to parse these bytes and the rest of the file as
       source code.

       The compiler just does [seek_on 0] in this case, however this doesn't work when
       the input is a pipe.

       What we do instead is create a lexing buffer from the input channel and pre-fill
       it with what we read to do the test. *)
    let lexbuf = Lexing.from_channel ic in
    let len = String.length prefix_read_from_file in
    Bytes.From_string.blit ~src:prefix_read_from_file ~src_pos:0 ~dst:lexbuf.lex_buffer ~dst_pos:0
      ~len;
    lexbuf.lex_buffer_len <- len;
    lexbuf.lex_curr_p <-
      { pos_fname = input_name
      ; pos_lnum  = 1
      ; pos_bol   = 0
      ; pos_cnum  = 0
      };
    Lexer.skip_hash_bang lexbuf;
    match kind with
    | Intf -> input_name, Intf (Parse.interface      lexbuf)
    | Impl -> input_name, Impl (Parse.implementation lexbuf)
;;

let load_source_file fn =
  let s = In_channel.read_all fn in
  if string_contains_binary_ast s then
    Location.raise_errorf ~loc:(Location.in_file fn)
      "ppxlib_driver: cannot use -reconcile with binary AST files";
  s
;;

type output_mode =
  | Pretty_print
  | Dump_ast
  | Dparsetree
  | Reconcile of Reconcile.mode
  | Null

(*$*)
let extract_cookies_str st =
  match st with
  | { pstr_desc = Pstr_attribute {attr_name={txt = "ocaml.ppx.context"; _}; _}; _ } as prefix
    :: st ->
    let prefix = Ppxlib_ast.Selected_ast.to_ocaml Structure [prefix] in
    assert (List.is_empty
              (Ocaml_common.Ast_mapper.drop_ppx_context_str ~restore:true prefix));
    st
  | _ -> st

let add_cookies_str st =
  let prefix =
    Ocaml_common.Ast_mapper.add_ppx_context_str ~tool_name:"ppxlib_driver" []
    |> Ppxlib_ast.Selected_ast.of_ocaml Structure
  in
  prefix @ st

(*$ str_to_sig _last_text_block *)
let extract_cookies_sig sg =
  match sg with
  | { psig_desc = Psig_attribute {attr_name={txt = "ocaml.ppx.context"; _}; _}; _ } as prefix
    :: sg ->
    let prefix = Ppxlib_ast.Selected_ast.to_ocaml Signature [prefix] in
    assert (List.is_empty
              (Ocaml_common.Ast_mapper.drop_ppx_context_sig ~restore:true prefix));
    sg
  | _ -> sg

let add_cookies_sig sg =
  let prefix =
    Ocaml_common.Ast_mapper.add_ppx_context_sig ~tool_name:"ppxlib_driver" []
    |> Ppxlib_ast.Selected_ast.of_ocaml Signature
  in
  prefix @ sg

(*$*)

let extract_cookies (ast : Intf_or_impl.t) : Intf_or_impl.t =
  match ast with
  | Intf x -> Intf (extract_cookies_sig x)
  | Impl x -> Impl (extract_cookies_str x)

let add_cookies (ast : Intf_or_impl.t) : Intf_or_impl.t =
  match ast with
  | Intf x -> Intf (add_cookies_sig x)
  | Impl x -> Impl (add_cookies_str x)

let corrections = ref []

let add_to_list r x = r := x :: !r

let register_correction ~loc ~repl =
  add_to_list corrections
    (Reconcile.Replacement.make_text ()
       ~start:loc.loc_start
       ~stop:loc.loc_end
       ~repl)

let process_file_hooks = ref []

let register_process_file_hook f =
  add_to_list process_file_hooks f

module File_property = struct
  type 'a t =
    { name         : string
    ; mutable data : 'a option
    ; sexp_of_t    : 'a -> Sexp.t
    }

  type packed = T : _ t -> packed

  let all = ref []

  let register t = add_to_list all (T t)

  let reset_all () =
    List.iter !all ~f:(fun (T t) -> t.data <- None)

  let dump_and_reset_all () =
    List.filter_map (List.rev !all) ~f:(fun (T t) ->
      match t.data with
      | None -> None
      | Some v ->
        t.data <- None;
        Some (t.name, t.sexp_of_t v))
end

module Create_file_property(Name : sig val name : string end)(T : Sexpable.S) = struct
  let t : _ File_property.t =
    { name      = Name.name
    ; data      = None
    ; sexp_of_t = T.sexp_of_t
    }

  let () = File_property.register t

  let set x = t.data <- Some x
end

let process_file (kind : Kind.t) fn ~input_name ~relocate ~output_mode ~embed_errors ~output =
  File_property.reset_all ();
  List.iter (List.rev !process_file_hooks) ~f:(fun f -> f ());
  corrections := [];
  let replacements = ref [] in
  let hook : Context_free.Generated_code_hook.t =
    match output_mode with
    | Reconcile (Using_line_directives | Delimiting_generated_blocks) ->
      { f = fun context (loc : Location.t) generated ->
          add_to_list replacements
            (Reconcile.Replacement.make ()
               ~context:(Extension context)
               ~start:loc.loc_start
               ~stop:loc.loc_end
               ~repl:generated)
      }
    | _ ->
      Context_free.Generated_code_hook.nop
  in
  let expect_mismatch_handler : Context_free.Expect_mismatch_handler.t =
    { f = fun context (loc : Location.t) generated ->
        add_to_list corrections
          (Reconcile.Replacement.make ()
             ~context:(Floating_attribute context)
             ~start:loc.loc_start
             ~stop:loc.loc_end
             ~repl:(Many generated))
    }
  in

  let input_name, ast =
    try
      let input_name, ast =
        with_preprocessed_input fn ~f:(load_input kind fn input_name ~relocate)
      in
      let ast = extract_cookies ast in
      let config = config ~hook ~expect_mismatch_handler in
      match ast with
      | Intf x -> input_name, Some_intf_or_impl.Intf (map_signature_gen x ~config)
      | Impl x -> input_name, Some_intf_or_impl.Impl (map_structure_gen x ~config)
    with exn when embed_errors ->
    match Location.Error.of_exn exn with
    | None -> raise exn
    | Some error ->
      let loc = Location.none in
      let ext = Location.Error.to_extension error in
      let open Ast_builder.Default in
      let ast = match kind with
        | Intf ->
           Some_intf_or_impl.Intf
             (Sig ((module Ppxlib_ast.Selected_ast),
                   [ psig_extension ~loc ext [] ]))
        | Impl ->
           Some_intf_or_impl.Impl
             (Str ((module Ppxlib_ast.Selected_ast),
                   [ pstr_extension ~loc ext [] ]))
      in
      input_name, ast
    in

    Option.iter !output_metadata_filename ~f:(fun fn ->
      let metadata = File_property.dump_and_reset_all () in
      Out_channel.write_all fn
        ~data:(
          List.map metadata ~f:(fun (s, sexp) ->
            Sexp.to_string_hum (Sexp.List [Atom s; sexp]) ^ "\n")
          |> String.concat ~sep:""));

    let input_contents = lazy (load_source_file fn) in
    let corrected = fn ^ !corrected_suffix in
    let mismatches_found =
      match !corrections with
      | [] ->
        if Caml.Sys.file_exists corrected then Caml.Sys.remove corrected;
        false
      | corrections ->
        Reconcile.reconcile corrections ~contents:(Lazy.force input_contents)
          ~output:(Some corrected) ~input_filename:fn ~input_name ~target:Corrected
          ?styler:!styler ~kind;
        true
    in

    (match output_mode with
     | Null -> ()
     | Pretty_print ->
       with_output output ~binary:false ~f:(fun oc ->
         let ppf = Caml.Format.formatter_of_out_channel oc in
         let ast = Intf_or_impl.of_some_intf_or_impl ast in
         (match ast with
          | Intf ast -> Pprintast.signature ppf ast
          | Impl ast -> Pprintast.structure ppf ast);
         let null_ast =
           match ast with
           | Intf [] | Impl [] -> true
           | _ -> false
         in
         if not null_ast then Caml.Format.pp_print_newline ppf ())
     | Dump_ast ->
       with_output output ~binary:true ~f:(fun oc ->
         let ast = Some_intf_or_impl.to_ast_io ast ~add_ppx_context:true in
         Migrate_parsetree.Ast_io.to_channel oc input_name ast)
     | Dparsetree ->
       with_output output ~binary:false ~f:(fun oc ->
         let ppf = Caml.Format.formatter_of_out_channel oc in
         let ast = Intf_or_impl.of_some_intf_or_impl ast in
         let ast = add_cookies ast in
         (match ast with
          | Intf ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#signature ast)
          | Impl ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#structure ast));
         Caml.Format.pp_print_newline ppf ())
     | Reconcile mode ->
       Reconcile.reconcile !replacements ~contents:(Lazy.force input_contents) ~output
         ~input_filename:fn ~input_name ~target:(Output mode) ?styler:!styler
         ~kind);

    if mismatches_found &&
       (match !diff_command with
        | Some  "-" -> false
        | _ -> true) then begin
      Ppxlib_print_diff.print () ~file1:fn ~file2:corrected ~use_color:!use_color
        ?diff_command:!diff_command;
      Caml.exit 1
    end
;;

let loc_fname = ref None
let output_mode = ref Pretty_print
let output = ref None
let kind = ref None
let input = ref None
let embed_errors = ref false
let set_input fn =
  match !input with
  | None -> input := Some fn
  | Some _ -> raise (Arg.Bad "too many input files")

let set_kind k =
  match !kind with
  | Some k' when not (Kind.equal k k') ->
    raise (Arg.Bad "must specify at most one of -impl or -intf")
  | _ -> kind := Some k
;;

let set_output_mode mode =
  match !output_mode, mode with
  | Pretty_print, _ -> output_mode := mode
  | _, Pretty_print -> assert false
  | Dump_ast   , Dump_ast
  | Dparsetree , Dparsetree -> ()
  | Reconcile a, Reconcile b when Poly.equal a b -> ()
  | x, y ->
    let arg_of_output_mode = function
      | Pretty_print -> assert false
      | Dump_ast                              -> "-dump-ast"
      | Dparsetree                            -> "-dparsetree"
      | Reconcile Using_line_directives       -> "-reconcile"
      | Reconcile Delimiting_generated_blocks -> "-reconcile-with-comments"
      | Null                                  -> "-null"
    in
    raise (Arg.Bad (Printf.sprintf
                      "%s and %s are incompatible"
                      (arg_of_output_mode x) (arg_of_output_mode y)))
;;

let print_transformations () =
  List.iter !Transform.all ~f:(fun (ct : Transform.t) ->
    printf "%s\n" ct.name);
;;

let parse_apply_list s =
  let names = if String.equal s "" then [] else String.split s ~on:',' in
  List.iter names ~f:(fun name ->
    if not (List.exists !Transform.all ~f:(fun (ct : Transform.t) ->
      Transform.has_name ct name)) then
      raise (Caml.Arg.Bad (Printf.sprintf "code transformation '%s' does not exist" name)));
  names

type mask =
  { mutable apply      : string list option
  ; mutable dont_apply : string list option
  }

let mask =
  { apply      = None
  ; dont_apply = None
  }

let handle_apply s =
  if Option.is_some mask.apply then
    raise (Arg.Bad "-apply called too many times");
  (* This is not strictly necessary but it's more intuitive *)
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply must be called before -dont-apply");
  mask.apply <- Some (parse_apply_list s)

let handle_dont_apply s =
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply called too many times");
  mask.dont_apply <- Some (parse_apply_list s)

let interpret_mask () =
  if Option.is_some mask.apply || Option.is_some mask.dont_apply then begin
    let selected_transform_name ct =
      let is_candidate =
        match mask.apply with
        | None -> true
        | Some names -> List.exists names ~f:(Transform.has_name ct)
      in
      let is_selected =
        match mask.dont_apply with
        | None -> is_candidate
        | Some names ->
          is_candidate
          && not (List.exists names ~f:(Transform.has_name ct))
      in
      if is_selected then
        Some ct.name
      else
        None
    in
    apply_list := Some (List.filter_map !Transform.all ~f:selected_transform_name)
  end

let shared_args =
  [ "-loc-filename", Arg.String (fun s -> loc_fname := Some s),
    "<string> File name to use in locations"
  ; "-reserve-namespace", Arg.String Name.Reserved_namespaces.reserve,
    "<string> Mark the given namespace as reserved"
  ; "-no-check", Arg.Clear perform_checks,
    " Disable checks (unsafe)"
  ; "-check", Arg.Set perform_checks,
    " Enable checks"
  ; "-no-check-on-extensions", Arg.Clear perform_checks_on_extensions,
    " Disable checks on extension point only"
  ; "-check-on-extensions", Arg.Set perform_checks_on_extensions,
    " Enable checks on extension point only"
  ; "-apply", Arg.String handle_apply,
    "<names> Apply these transformations in order (comma-separated list)"
  ; "-dont-apply", Arg.String handle_dont_apply,
    "<names> Exclude these transformations"
  ; "-no-merge", Arg.Set no_merge,
    " Do not merge context free transformations (better for debugging rewriters)"
  ]

let () =
  List.iter shared_args ~f:(fun (key, spec, doc) -> add_arg key spec ~doc)

let set_cookie s =
  match String.lsplit2 s ~on:'=' with
  | None ->
    raise (Arg.Bad "invalid cookie, must be of the form \"<name>=<expr>\"")
  | Some (name, value) ->
    let lexbuf = Lexing.from_string value in
    lexbuf.Lexing.lex_curr_p <-
      { Lexing.
        pos_fname = "<command-line>"
      ; pos_lnum  = 1
      ; pos_bol   = 0
      ; pos_cnum  = 0
      };
    let expr = Parse.expression lexbuf in
    Migrate_parsetree.Driver.set_global_cookie name
      (module Ppxlib_ast.Selected_ast) expr

let as_pp () =
  set_output_mode Dump_ast;
  embed_errors := true

let standalone_args =
  [ "-as-ppx", Arg.Unit (fun () -> raise (Arg.Bad "-as-ppx must be the first argument")),
    " Run as a -ppx rewriter (must be the first argument)"
  ; "--as-ppx", Arg.Unit (fun () -> raise (Arg.Bad "--as-ppx must be the first argument")),
    " Same as -as-ppx"
  ; "-as-pp", Arg.Unit as_pp,
    " Shorthand for: -dump-ast -embed-errors"
  ; "--as-pp", Arg.Unit as_pp,
    " Same as -as-pp"
  ; "-o", Arg.String (fun s -> output := Some s),
    "<filename> Output file (use '-' for stdout)"
  ; "-", Arg.Unit (fun () -> set_input "-"),
    " Read input from stdin"
  ; "-dump-ast", Arg.Unit (fun () -> set_output_mode Dump_ast),
    " Dump the marshaled ast to the output file instead of pretty-printing it"
  ; "--dump-ast", Arg.Unit (fun () -> set_output_mode Dump_ast),
    " Same as -dump-ast"
  ; "-dparsetree", Arg.Unit (fun () -> set_output_mode Dparsetree),
    " Print the parsetree (same as ocamlc -dparsetree)"
  ; "-embed-errors", Arg.Set embed_errors,
    " Embed errors in the output AST (default: true when -dump-ast, false otherwise)"
  ; "-null", Arg.Unit (fun () -> set_output_mode Null),
    " Produce no output, except for errors"
  ; "-impl", Arg.Unit (fun () -> set_kind Impl),
    "<file> Treat the input as a .ml file"
  ; "--impl", Arg.Unit (fun () -> set_kind Impl),
    "<file> Same as -impl"
  ; "-intf", Arg.Unit (fun () -> set_kind Intf),
    "<file> Treat the input as a .mli file"
  ; "--intf", Arg.Unit (fun () -> set_kind Intf),
    "<file> Same as -intf"
  ; "-debug-attribute-drop", Arg.Set debug_attribute_drop,
    " Debug attribute dropping"
  ; "-print-transformations", Arg.Set request_print_transformations,
    " Print linked-in code transformations, in the order they are applied"
  ; "-print-passes", Arg.Set request_print_passes,
    " Print the actual passes over the whole AST in the order they are applied"
  ; "-ite-check",
    Arg.Unit (fun () ->
      eprintf "Warning: the -ite-check flag is deprecated \
               and has no effect.\n%!";
      Extra_warnings.care_about_ite_branch := true),
    " (no effect -- kept for compatibility)"
  ; "-pp", Arg.String (fun s -> preprocessor := Some s),
    "<command>  Pipe sources through preprocessor <command> (incompatible with -as-ppx)"
  ; "-reconcile", Arg.Unit (fun () -> set_output_mode (Reconcile Using_line_directives)),
    " (WIP) Pretty print the output using a mix of the input source \
     and the generated code"
  ; "-reconcile-with-comments",
    Arg.Unit (fun () -> set_output_mode (Reconcile Delimiting_generated_blocks)),
    " (WIP) same as -reconcile but uses comments to enclose the generated code"
  ; "-no-color", Arg.Clear use_color,
    " Don't use colors when printing errors"
  ; "-diff-cmd", Arg.String (fun s -> diff_command := Some s),
    " Diff command when using code expectations (use - to disable diffing)"
  ; "-pretty", Arg.Set pretty,
    " Instruct code generators to improve the prettiness of the generated code"
  ; "-styler", Arg.String (fun s -> styler := Some s),
    " Code styler"
  ; "-cookie", Arg.String set_cookie,
    "NAME=EXPR Set the cookie NAME to EXPR"
  ; "--cookie", Arg.String set_cookie,
    " Same as -cookie"
  ; "-output-metadata", Arg.String (fun s -> output_metadata_filename := Some s),
    "FILE Where to store the output metadata"
  ; "-corrected-suffix", Arg.Set_string corrected_suffix,
    "SUFFIX Suffix to happend to corrected files"
  ]
;;

let get_args ?(standalone_args=standalone_args) () =
  let args = standalone_args @ List.rev !args in
  let my_arg_names =
    List.rev_map args ~f:(fun (name, _, _) -> name)
    |> Set.of_list (module String)
  in
  let omp_args =
    (* Filter out arguments that we override *)
    List.filter (Migrate_parsetree.Driver.registered_args ())
      ~f:(fun (name, _, _) ->
          not (Set.mem my_arg_names name))
  in
  args @ omp_args
;;

let standalone_main () =
  let usage =
    Printf.sprintf "%s [extra_args] [<files>]" exe_name
  in
  let args = get_args () in
  Migrate_parsetree.Driver.reset_args ();
  Arg.parse (Arg.align args) set_input usage;
  interpret_mask ();
  if !request_print_transformations then begin
    print_transformations ();
    Caml.exit 0;
  end;
  if !request_print_passes then begin
    print_passes ();
    Caml.exit 0;
  end;
  match !input with
  | None    ->
    eprintf "%s: no input file given\n%!" exe_name;
    Caml.exit 2
  | Some fn ->
    let kind =
      match !kind with
      | Some k -> k
      | None ->
        match Kind.of_filename fn with
        | Some k -> k
        | None ->
          eprintf "%s: don't know what to do with '%s', use -impl or -intf.\n"
            exe_name fn;
          Caml.exit 2
    in
    let input_name, relocate =
      match !loc_fname with
      | None    -> fn, false
      | Some fn -> fn, true
    in
    process_file kind fn ~input_name ~relocate ~output_mode:!output_mode ~output:!output
      ~embed_errors:!embed_errors
;;

let standalone_run_as_ppx_rewriter () =
  let n = Array.length Caml.Sys.argv in
  let usage = Printf.sprintf "%s -as-ppx [extra_args] <infile> <outfile>" exe_name in
  if n < 4 then begin
    eprintf "Usage: %s\n%!" usage;
    Caml.exit 2
  end;
  let argv = Array.create ~len:(n - 3) "" in
  argv.(0) <- Caml.Sys.argv.(0);
  for i = 1 to (n - 4) do
    argv.(i) <- Caml.Sys.argv.(i + 1)
  done;
  let standalone_args =
    List.map standalone_args ~f:(fun (arg, spec, _doc) ->
      (arg, spec, " Unused with -as-ppx"))
  in
  let args = get_args ~standalone_args () in
  Migrate_parsetree.Driver.reset_args ();
  match
    Arg.parse_argv argv (Arg.align args)
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      usage
  with
  | exception Arg.Bad  msg -> eprintf "%s" msg; Caml.exit 2
  | exception Arg.Help msg -> eprintf "%s" msg; Caml.exit 0
  | () ->
    interpret_mask ();
    Ocaml_common.Ast_mapper.apply
      ~source:Caml.Sys.argv.(n - 2) ~target:Caml.Sys.argv.(n - 1) mapper
;;

let standalone () =
  Compiler_specifics.read_clflags_from_env ();
  try
    if Array.length Caml.Sys.argv >= 2 &&
       match Caml.Sys.argv.(1) with
       | "-as-ppx" | "--as-ppx" -> true
       | _ -> false
    then
      standalone_run_as_ppx_rewriter ()
    else
      standalone_main ();
    Caml.exit 0
  with exn ->
    Location.report_exception Caml.Format.err_formatter exn;
    Caml.exit 1
;;

let pretty () = !pretty

let () =
  Migrate_parsetree.Driver.register
    ~name:"ppxlib_driver"
    (* This doesn't take arguments registered by rewriters. It's not worth supporting
       them, since [--cookie] is a much better replacement for passing parameters to
       individual rewriters. *)
    ~args:shared_args
    (module Ppxlib_ast.Selected_ast)
    (fun config cookies ->
       let module A = Ppxlib_ast.Selected_ast.Ast.Ast_mapper in
       let structure _ st = real_map_structure config cookies st in
       let signature _ sg = real_map_signature config cookies sg in
       { A.default_mapper with structure; signature })

let enable_checks () =
  perform_checks := true;
  perform_checks_on_extensions := true
