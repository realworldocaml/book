(*$ open Ppxlib_cinaps_helpers $*)
open Import
open Utils

module Arg = Caml.Arg

let exe_name = Caml.Filename.basename Caml.Sys.executable_name

let args = ref []

let add_arg key spec ~doc = args := (key, spec, doc) :: !args

let loc_fname = ref None
let perform_checks = ref Options.perform_checks
let perform_checks_on_extensions = ref Options.perform_checks_on_extensions
let perform_locations_check = ref Options.perform_locations_check
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
  type t = T

  let given_through_cli = ref []

  let get T name pattern =
    Option.map (Ocaml_common.Ast_mapper.get_cookie name)
      ~f:(fun e ->
        let e = Selected_ast.of_ocaml Expression e in
        Ast_pattern.parse pattern e.pexp_loc e Fn.id)

  let set T name expr =
    Ocaml_common.Ast_mapper.set_cookie name
      (Selected_ast.to_ocaml Expression expr)

  let handlers = ref []
  let add_handler f = handlers := !handlers @ [f]

  let add_simple_handler name pattern ~f =
    add_handler (fun T -> f (get T name pattern))

  let acknowledge_cookies T =
    List.iter !handlers ~f:(fun f -> f T)

  let post_handlers = ref []
  let add_post_handler f = post_handlers := !post_handlers @ [f]

  let call_post_handlers T =
    List.iter !post_handlers ~f:(fun f -> f T)
end

module Instrument = struct
  type pos = Before | After

  type t = { transformation : Expansion_context.Base.t -> Parsetree.structure -> Parsetree.structure ; position : pos}

  module V2 = struct
    let make transformation ~position = { transformation ; position }
  end

  let make transformation ~position =
    let transformation _ st = transformation st in
    V2.make transformation ~position
end

module Transform = struct
  type t =
    { name            : string
    ; aliases         : string list
    ; impl            : (Expansion_context.Base.t -> Parsetree.structure -> Parsetree.structure) option
    ; intf            : (Expansion_context.Base.t -> Parsetree.signature -> Parsetree.signature) option
    ; lint_impl       : (Expansion_context.Base.t -> Parsetree.structure -> Lint_error.t list) option
    ; lint_intf       : (Expansion_context.Base.t -> Parsetree.signature -> Lint_error.t list) option
    ; preprocess_impl : (Expansion_context.Base.t -> Parsetree.structure -> Parsetree.structure) option
    ; preprocess_intf : (Expansion_context.Base.t -> Parsetree.signature -> Parsetree.signature) option
    ; enclose_impl    : (Expansion_context.Base.t -> Location.t option -> Parsetree.structure * Parsetree.structure) option
    ; enclose_intf    : (Expansion_context.Base.t -> Location.t option -> Parsetree.signature * Parsetree.signature) option
    ; instrument      : Instrument.t option
    ; rules           : Context_free.Rule.t list
    ; registered_at   : Caller_id.t
    }

  let has_name t name =
    (String.equal name t.name) || (List.exists ~f:(String.equal name) t.aliases)

  let all : t list ref = ref []

  let print_caller_id oc (caller_id : Caller_id.t) =
    match caller_id with
    | None -> output_string oc "<unknown location>"
    | Some loc -> Printf.fprintf oc "%s:%d" loc.filename loc.line_number
  ;;

  let register ?(extensions=[]) ?(rules=[]) ?enclose_impl ?enclose_intf
        ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf
        ?instrument ?(aliases=[]) name =
    let rules =
      List.map extensions ~f:Context_free.Rule.extension @ rules
    in
    let caller_id = Caller_id.get ~skip:[Caml.__FILE__] in
    begin match List.filter !all ~f:(fun ct -> has_name ct name) with
    | [] -> ()
    | ct :: _ ->
      Printf.eprintf "Warning: code transformation %s registered twice.\n" name;
      Printf.eprintf "  - first time was at %a\n" print_caller_id ct.registered_at;
      Printf.eprintf "  - second time is at %a\n" print_caller_id caller_id;
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
      ; instrument
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

  let merge_into_generic_mappers t ~hook ~expect_mismatch_handler ~tool_name ~input_name =
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
    let input_name =
      match input_name with Some input_name -> input_name | None -> "_none_"
    in
    let map_impl ctxt st_with_attrs =
      let st =
        let attrs, st =
          List.split_while st_with_attrs ~f:(function
            | { pstr_desc = Pstr_attribute _; _ } -> true
            | _ -> false)
        in
        let file_path = File_path.get_default_path_str st in
        let base_ctxt = Expansion_context.Base.top_level ~tool_name ~file_path ~input_name in
        let header, footer =
          match enclose_impl with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list st ~get_loc:(fun st -> st.Parsetree.pstr_loc) in
            gen_header_and_footer Structure_item whole_loc (f base_ctxt)
        in
        let attrs = map#structure base_ctxt attrs in
        let st = map#structure base_ctxt st in
        List.concat [ attrs; header; st; footer ]
      in
      match impl with
      | None -> st
      | Some f -> f ctxt st
    in
    let map_intf ctxt sg_with_attrs =
      let sg =
        let attrs, sg =
          List.split_while sg_with_attrs ~f:(function
            | { psig_desc = Psig_attribute _; _ } -> true
            | _ -> false)
        in
        let file_path = File_path.get_default_path_sig sg in
        let base_ctxt = Expansion_context.Base.top_level ~tool_name ~file_path ~input_name in
        let header, footer =
          match enclose_intf with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list sg ~get_loc:(fun sg -> sg.Parsetree.psig_loc) in
            gen_header_and_footer Signature_item whole_loc (f base_ctxt)
        in
        let attrs = map#signature base_ctxt attrs in
        let sg = map#signature base_ctxt sg in
        List.concat [ attrs; header; sg; footer ]
      in
      match intf with
      | None -> sg
      | Some f -> f ctxt sg
    in
    { t with
      impl = Some map_impl
    ; intf = Some map_intf
    }

  let builtin_of_context_free_rewriters ~hook ~rules ~enclose_impl ~enclose_intf ~input_name =
    merge_into_generic_mappers ~hook ~input_name
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
      ; instrument = None
      ; rules
      ; registered_at = Caller_id.get ~skip:[]
      }

  let partition_transformations ts =
    let before_instrs, after_instrs, rest =
      List.fold_left ts ~init:([], [], []) ~f:(fun (bef_i, aft_i, rest) t ->
        let reduced_t =
          { t with
            lint_impl = None
          ; lint_intf = None
          ; preprocess_impl = None
          ; preprocess_intf = None
          } in
        let f instr = (instr.Instrument.position, instr.Instrument.transformation) in
        match Option.map t.instrument ~f with
        | Some (Before, transf) -> { reduced_t with impl = Some transf }::bef_i, aft_i, rest
        | Some (After, transf) -> bef_i, { reduced_t with impl = Some transf }::aft_i, rest
        | None -> bef_i, aft_i, reduced_t::rest)
    in
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
              ; instrument = None
              ; rules = []
              ; registered_at = t.registered_at
              }
          else
            None)),
     `Preprocess
       (List.filter_map ts ~f:(fun t ->
          if Option.is_some t.preprocess_impl || Option.is_some t.preprocess_intf
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
              ; instrument = None
              ; rules = []
              ; registered_at = t.registered_at
              }
          else
            None)),
     `Before_instrs before_instrs,
     `After_instrs after_instrs,
     `Rest rest)
end

module V2 = struct
  let register_transformation = Transform.register

  let register_transformation_using_ocaml_current_ast ?impl ?intf ?aliases name =
    let impl = Option.map impl ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Structure) in
    let intf = Option.map intf ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Signature) in
    register_transformation ?impl ?intf ?aliases name
end

let add_ctxt_arg (f : 'a -> 'b) : (Expansion_context.Base.t -> 'a -> 'b) = fun _ x -> f x

let register_transformation ?extensions ?rules ?enclose_impl ?enclose_intf ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf =
  let impl = Option.map impl ~f:add_ctxt_arg in
  let intf = Option.map intf ~f:add_ctxt_arg in
  let preprocess_impl = Option.map preprocess_impl ~f:add_ctxt_arg in
  let preprocess_intf = Option.map preprocess_intf ~f:add_ctxt_arg in
  let lint_impl = Option.map lint_impl ~f:add_ctxt_arg in
  let lint_intf = Option.map lint_intf ~f:add_ctxt_arg in
  let enclose_impl = Option.map enclose_impl ~f:add_ctxt_arg in
  let enclose_intf = Option.map enclose_intf ~f:add_ctxt_arg in
  V2.register_transformation ?extensions ?rules ?enclose_impl ?enclose_intf ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf

let register_code_transformation ~name ?(aliases=[]) ~impl ~intf =
  register_transformation name ~impl ~intf ~aliases
[@@warning "-16"] (* This function triggers a warning 16 as of ocaml 4.12 *)
;;

let register_transformation_using_ocaml_current_ast ?impl ?intf =
  let impl = Option.map impl ~f:add_ctxt_arg in
  let intf = Option.map intf ~f:add_ctxt_arg in
  V2.register_transformation_using_ocaml_current_ast ?impl ?intf

let debug_dropped_attribute name ~old_dropped ~new_dropped =
  let print_diff what a b =
    let diff =
      List.filter a ~f:(fun (name : _ Loc.t) ->
        not (List.exists b ~f:(fun (name' : _ Location.loc) -> name.txt == name'.txt)))
    in
    if not (List.is_empty diff) then begin
      Printf.eprintf "The following attributes %s after applying %s:\n"
        what name;
      List.iter diff ~f:(fun { Location. txt; loc } ->
        Caml.Format.eprintf "- %a: %s\n" Location.print loc txt);
      Caml.Format.eprintf "@."
    end
  in
  print_diff "disappeared" new_dropped old_dropped;
  print_diff "reappeared"  old_dropped new_dropped
;;

let get_whole_ast_passes ~hook ~expect_mismatch_handler ~tool_name ~input_name =
  let cts =
    match !apply_list with
    | None -> List.rev !Transform.all
    | Some names ->
      List.map names ~f:(fun name ->
        List.find !Transform.all ~f:(fun (ct : Transform.t) ->
          Transform.has_name ct name))
  in
  let (`Linters linters, `Preprocess preprocess, `Before_instrs before_instrs, `After_instrs after_instrs, `Rest cts) =
    Transform.partition_transformations cts in
  (* Allow only one preprocessor to assure deterministic order *)
  if (List.length preprocess) > 1 then begin
    let pp = String.concat ~sep:", " (List.map preprocess ~f:(fun t -> t.name)) in
    let err = Printf.sprintf "At most one preprocessor is allowed, while got: %s" pp in
    failwith err
  end;
  let make_generic transforms =
    if !no_merge then
      List.map transforms ~f:(Transform.merge_into_generic_mappers ~hook ~tool_name
                                ~expect_mismatch_handler ~input_name)
    else begin
      let get_enclosers ~f =
        List.filter_map transforms ~f:(fun (ct : Transform.t) ->
          match f ct with
          | None -> None
          | Some x -> Some (ct.name, x))
        (* Sort them to ensure deterministic ordering *)
        |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
        |> List.map ~f:snd
      in

      let rules =
        List.map transforms ~f:(fun (ct : Transform.t) -> ct.rules) |> List.concat
      and impl_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_impl)
      and intf_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_intf)
      in
      match rules, impl_enclosers, intf_enclosers with
      | [], [], [] -> transforms
      | _              ->
        let merge_encloser = function
          | [] -> None
          | enclosers -> Some (fun ctxt loc ->
            let headers, footers =
              List.map enclosers ~f:(fun f -> f ctxt loc)
              |> List.split
            in
            let headers = List.concat headers in
            let footers = List.concat (List.rev footers) in
            (headers, footers))
        in
        Transform.builtin_of_context_free_rewriters ~rules ~hook ~expect_mismatch_handler
          ~enclose_impl:(merge_encloser impl_enclosers)
          ~enclose_intf:(merge_encloser intf_enclosers)
          ~tool_name
          ~input_name
        :: transforms
    end
         |> List.filter ~f:(fun (ct : Transform.t) ->
           match ct.impl, ct.intf with
           | None, None -> false
           | _          -> true)
  in linters @ preprocess @ make_generic before_instrs @ make_generic cts @ make_generic after_instrs
;;

let apply_transforms
      ~tool_name ~file_path ~field ~lint_field ~dropped_so_far ~hook ~expect_mismatch_handler ~input_name x =
  let cts = get_whole_ast_passes ~tool_name ~hook ~expect_mismatch_handler ~input_name in
  let x, _dropped, lint_errors =
    List.fold_left cts ~init:(x, [], [])
      ~f:(fun (x, dropped, lint_errors) (ct : Transform.t) ->
        let input_name =
          match input_name with Some input_name -> input_name | None -> "_none_"
        in
        let ctxt = Expansion_context.Base.top_level ~tool_name ~file_path ~input_name in
        let lint_errors =
          match lint_field ct with
          | None -> lint_errors
          | Some f -> lint_errors @ f ctxt x
        in
        match field ct with
        | None -> (x, dropped, lint_errors)
        | Some f ->
          let x = f ctxt x in
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

let print_passes () =
  let tool_name = "ppxlib_driver" in
  let hook = Context_free.Generated_code_hook.nop in
  let expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop in
  let cts = get_whole_ast_passes ~hook ~expect_mismatch_handler ~tool_name ~input_name:None in
  if !perform_checks then
    Printf.printf "<builtin:freshen-and-collect-attributes>\n";
  List.iter cts ~f:(fun ct -> Printf.printf "%s\n" ct.Transform.name);
  if !perform_checks then
    begin
      Printf.printf "<builtin:check-unused-attributes>\n";
      if !perform_checks_on_extensions
      then Printf.printf "<builtin:check-unused-extensions>\n"
    end
;;

(*$*)
let map_structure_gen st ~tool_name ~hook ~expect_mismatch_handler ~input_name =
  Cookies.acknowledge_cookies T;
  if !perform_checks then begin
    Attribute.reset_checks ();
    Attribute.collect#structure st
  end;
  let st, lint_errors =
    let file_path = File_path.get_default_path_str st in
    apply_transforms st
      ~tool_name
      ~file_path
      ~field:(fun (ct : Transform.t) -> ct.impl)
      ~lint_field:(fun (ct : Transform.t) -> ct.lint_impl)
      ~dropped_so_far:Attribute.dropped_so_far_structure ~hook ~expect_mismatch_handler
      ~input_name
  in
  let st =
    match lint_errors with
    | [] -> st
    | _  ->
      List.map lint_errors ~f:(fun ({ attr_name = { loc; _ }; _} as attr) ->
        Ast_builder.Default.pstr_attribute ~loc attr)
      @ st
  in
  Cookies.call_post_handlers T;
  if !perform_checks then begin
    (* TODO: these two passes could be merged, we now have more passes for
       checks than for actual rewriting. *)
    Attribute.check_unused#structure st;
    if !perform_checks_on_extensions then Extension.check_unused#structure st;
    Attribute.check_all_seen ();
    if !perform_locations_check then
      let open Location_check in
      ignore (
        (enforce_invariants !loc_fname)#structure
          st Non_intersecting_ranges.empty : Non_intersecting_ranges.t)
  end;
  st
;;

let map_structure st =
  map_structure_gen st
    ~tool_name:(Ocaml_common.Ast_mapper.tool_name ())
    ~hook:Context_free.Generated_code_hook.nop
    ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop
    ~input_name:None

(*$ str_to_sig _last_text_block *)
let map_signature_gen sg ~tool_name ~hook ~expect_mismatch_handler ~input_name =
  Cookies.acknowledge_cookies T;
  if !perform_checks then begin
    Attribute.reset_checks ();
    Attribute.collect#signature sg
  end;
  let sg, lint_errors =
    let file_path = File_path.get_default_path_sig sg in
    apply_transforms sg
      ~tool_name
      ~file_path
      ~field:(fun (ct : Transform.t) -> ct.intf)
      ~lint_field:(fun (ct : Transform.t) -> ct.lint_intf)
      ~dropped_so_far:Attribute.dropped_so_far_signature ~hook ~expect_mismatch_handler
      ~input_name
  in
  let sg =
    match lint_errors with
    | [] -> sg
    | _  ->
      List.map lint_errors ~f:(fun ({ attr_name = { loc; _ }; _} as attr) ->
        Ast_builder.Default.psig_attribute ~loc attr)
      @ sg
  in
  Cookies.call_post_handlers T;
  if !perform_checks then begin
    (* TODO: these two passes could be merged, we now have more passes for
       checks than for actual rewriting. *)
    Attribute.check_unused#signature sg;
    if !perform_checks_on_extensions then Extension.check_unused#signature sg;
    Attribute.check_all_seen ();
    if !perform_locations_check then
      let open Location_check in
      ignore (
        (enforce_invariants !loc_fname)#signature
          sg Non_intersecting_ranges.empty : Non_intersecting_ranges.t)
  end;
  sg
;;

let map_signature sg =
  map_signature_gen sg
    ~tool_name:(Ocaml_common.Ast_mapper.tool_name ())
    ~hook:Context_free.Generated_code_hook.nop
    ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop
    ~input_name:None

(*$*)

(* +-----------------------------------------------------------------+
   | Entry points                                                    |
   +-----------------------------------------------------------------+ *)

let string_contains_binary_ast s =
  let test magic_number =
    String.is_prefix s ~prefix:(String.sub magic_number ~pos:0 ~len:9)
  in
  test Ast_magic.ast_intf_magic_number ||
  test Ast_magic.ast_impl_magic_number

let versioned_errorf input_version input_file_name =
  Printf.ksprintf (fun msg ->
    let err =
      Location.Error.make ~loc:(Location.in_file input_file_name) msg ~sub:[]
    in
    Error (err, input_version))

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
        match System.run_preprocessor ~pp ~input:fn ~output:tmpfile with
          | Ok () -> f tmpfile
          | Error (failed_command, fall_back_version) ->
            versioned_errorf fall_back_version fn
              "Error while running external preprocessor\n\
               Command line: %s\n" failed_command
      )
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

let load_input ~(kind : Kind.t) ~input_name ~relocate fn =
  set_input_name input_name;
  let input_source = if String.equal fn "-" then Ast_io.Stdin else File fn in
  let input_kind = Ast_io.Possibly_source (kind, input_name) in
  match Ast_io.read input_source ~input_kind with
  | Ok { input_name = ast_input_name; input_version; ast } ->
    let ast_kind = Intf_or_impl.kind ast in
    if not (Kind.equal kind ast_kind) then
      versioned_errorf input_version fn
        "File contains a binary %s AST but an %s was expected"
        (Kind.describe ast_kind) (Kind.describe kind)
    else if String.equal ast_input_name input_name || not relocate then (
      set_input_name ast_input_name;
      Ok (ast_input_name, input_version, ast) )
    else
      Ok
        ( input_name,
          input_version,
          Intf_or_impl.map_with_context ast relocate_mapper
            (ast_input_name, input_name) )
  | Error (Unknown_version (unknown_magic, fall_back_version)) ->
    versioned_errorf fall_back_version fn
      "File is a binary ast for an unknown version of OCaml with magic \
       number '%s'" unknown_magic
  | Error (System_error (error, fall_back_version))
  | Error (Source_parse_error (error, fall_back_version)) ->
    Error (error, fall_back_version)
  | Error Not_a_binary_ast -> assert false
;;

let load_input_run_as_ppx fn =
  (* If there's an error while loading in run_as_ppx mode, the kind of AST (impl/intf) is still unknown.
     That's why, as opposed to load_input, this function raises errors instead of returning a result:
     handling an error by returning an AST with the error packed as extension node wouldn't be possible. *)
  match Ast_io.read (File fn) ~input_kind:Ast_io.Necessarily_binary with
  | Ok {input_name = ast_input_name; input_version; ast} ->
      let ast =
        match !loc_fname with
        | None ->
            set_input_name ast_input_name;
            ast
        | Some input_name ->
            set_input_name input_name;
            if String.equal ast_input_name input_name then ast
            else
              Intf_or_impl.map_with_context ast relocate_mapper
                (ast_input_name, input_name)
      in
      (* With `--as-ppx`, ocaml calls the standalone separately for every structure/signature item
         with the filename as metadata that it gets from the previous call. relocate_mapper only
         relocates positions whose position filename coincides with that metadata filename.
         So always return the metadata filename itself, even if `-loc-filename` is provided. *)
      (ast_input_name, input_version, ast)
  | Error (Unknown_version (unknown_magic, _)) ->
      Location.raise_errorf
        ~loc:(Location.in_file fn)
        "The input is a binary ast for an unknown version of OCaml with magic number '%s'" unknown_magic
  | Error Not_a_binary_ast ->
      Location.raise_errorf
        ~loc:(Location.in_file fn)
        "Expected a binary AST as input"
  | Error (System_error (error, _)) | Error (Source_parse_error (error, _)) ->
      let open Location.Error in
      Location.set_filename (get_location error) fn
      |> update_loc error
      |> raise
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
  let st = match st with
  | { pstr_desc = Pstr_attribute {attr_name={txt = "ocaml.ppx.context"; _}; _}; _ } as prefix
    :: st ->
    let prefix = Ppxlib_ast.Selected_ast.to_ocaml Structure [prefix] in
    assert (List.is_empty
              (Ocaml_common.Ast_mapper.drop_ppx_context_str ~restore:true prefix));
    st
  | _ -> st
  in
  (* The cli cookies have to be set after restoring the ppx context,
     since restoring the ppx context resets the cookies *)
  List.iter !Cookies.given_through_cli ~f:(fun (name, expr) ->
    Cookies.set T name expr);
  st

let add_cookies_str st =
  let prefix =
    Ocaml_common.Ast_mapper.add_ppx_context_str ~tool_name:"ppxlib_driver" []
    |> Ppxlib_ast.Selected_ast.of_ocaml Structure
  in
  prefix @ st

(*$ str_to_sig _last_text_block *)
let extract_cookies_sig sg =
  let sg = match sg with
  | { psig_desc = Psig_attribute {attr_name={txt = "ocaml.ppx.context"; _}; _}; _ } as prefix
    :: sg ->
    let prefix = Ppxlib_ast.Selected_ast.to_ocaml Signature [prefix] in
    assert (List.is_empty
              (Ocaml_common.Ast_mapper.drop_ppx_context_sig ~restore:true prefix));
    sg
  | _ -> sg
  in
  (* The cli cookies have to be set after restoring the ppx context,
     since restoring the ppx context resets the cookies *)
  List.iter !Cookies.given_through_cli ~f:(fun (name, expr) ->
    Cookies.set T name expr);
  sg

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


let error_to_extension error ~(kind : Kind.t) =
  let loc = Location.none in
  let ext = Location.Error.to_extension error in
  let open Ast_builder.Default in
  let ast = match kind with
    | Intf -> Intf_or_impl.Intf [ psig_extension ~loc ext [] ]
    | Impl -> Intf_or_impl.Impl [ pstr_extension ~loc ext [] ]
  in
  ast
;;

let exn_to_extension exn ~(kind : Kind.t) =
  match Location.Error.of_exn exn with
  | None -> raise exn
  | Some error -> error_to_extension error ~kind
;;

let process_ast (ast : Intf_or_impl.t) ~input_name ~tool_name ~hook ~expect_mismatch_handler =
  match ast with
  | Intf x ->
    Intf_or_impl.Intf
      (map_signature_gen x
         ~tool_name ~hook ~expect_mismatch_handler ~input_name:(Some input_name))
  | Impl x ->
    Intf_or_impl.Impl
      (map_structure_gen x
         ~tool_name ~hook ~expect_mismatch_handler ~input_name:(Some input_name))
;;

let process_file (kind : Kind.t) fn ~input_name ~relocate ~output_mode ~embed_errors ~output =
  File_property.reset_all ();
  List.iter (List.rev !process_file_hooks) ~f:(fun f -> f ());
  corrections := [];
  let replacements = ref [] in
  let tool_name = "ppx_driver" in
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

  let input_name, input_version, ast =
    let preprocessed_and_loaded =
      with_preprocessed_file fn ~f:(load_input ~kind ~input_name ~relocate)
    in
    match preprocessed_and_loaded with
    | Ok (input_fname, input_version, ast) -> (
        try
          let ast =
            extract_cookies ast
            |> process_ast ~input_name ~tool_name ~hook ~expect_mismatch_handler
          in
          (input_fname, input_version, ast)
        with exn when embed_errors ->
          (input_fname, input_version, exn_to_extension exn ~kind) )
    | Error (error, input_version) when embed_errors ->
        (input_name, input_version, error_to_extension error ~kind)
    | Error (error, _) ->
        let open Location.Error in
        Location.set_filename (get_location error) fn
        |> update_loc error
        |> raise
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
       Ast_io.write oc {input_name; input_version; ast} ~add_ppx_context:true)
   | Dparsetree ->
     with_output output ~binary:false ~f:(fun oc ->
       let ppf = Caml.Format.formatter_of_out_channel oc in
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
    Printf.printf "%s\n" ct.name);
;;

let parse_apply_list s =
  let names = if String.equal s "" then [] else String.split_on_char s ~sep:',' in
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
    Cookies.given_through_cli := (name, expr) :: !Cookies.given_through_cli

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
  ; "-no-locations-check", Arg.Clear perform_locations_check,
    " Disable locations check only"
  ; "-locations-check", Arg.Set perform_locations_check,
    " Enable locations check only"
  ; "-apply", Arg.String handle_apply,
    "<names> Apply these transformations in order (comma-separated list)"
  ; "-dont-apply", Arg.String handle_dont_apply,
    "<names> Exclude these transformations"
  ; "-no-merge", Arg.Set no_merge,
    " Do not merge context free transformations (better for debugging rewriters)"
  ; "-cookie", Arg.String set_cookie,
    "NAME=EXPR Set the cookie NAME to EXPR"
  ; "--cookie", Arg.String set_cookie,
    " Same as -cookie"
  ]

let () =
  List.iter shared_args ~f:(fun (key, spec, doc) -> add_arg key spec ~doc)

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
      Printf.eprintf "Warning: the -ite-check flag is deprecated \
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
  ; "-output-metadata", Arg.String (fun s -> output_metadata_filename := Some s),
    "FILE Where to store the output metadata"
  ; "-corrected-suffix", Arg.Set_string corrected_suffix,
    "SUFFIX Suffix to append to corrected files"
  ]
;;

let get_args ?(standalone_args=standalone_args) () =
  standalone_args @ List.rev !args
;;

let standalone_main () =
  let usage =
    Printf.sprintf "%s [extra_args] [<files>]" exe_name
  in
  let args = get_args () in
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
    Printf.eprintf "%s: no input file given\n%!" exe_name;
    Caml.exit 2
  | Some fn ->
    let kind =
      match !kind with
      | Some k -> k
      | None ->
        match Kind.of_filename fn with
        | Some k -> k
        | None ->
          Printf.eprintf "%s: don't know what to do with '%s', use -impl or -intf.\n"
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

let rewrite_binary_ast_file input_fn output_fn =
  let input_name, input_version, ast =
    load_input_run_as_ppx input_fn
  in
  let ast =
    try
      let ast = extract_cookies ast in
      let tool_name = Ocaml_common.Ast_mapper.tool_name () in
      let hook = Context_free.Generated_code_hook.nop in
      let expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop in
      process_ast ast ~input_name ~tool_name ~hook ~expect_mismatch_handler
    with exn -> exn_to_extension exn ~kind:(Intf_or_impl.kind ast)
  in
  with_output (Some output_fn) ~binary:true ~f:(fun oc ->
    Ast_io.write oc {input_name; input_version; ast} ~add_ppx_context:true)
;;

let parse_input passed_in_args ~valid_args
    ~incorrect_input_msg =
  try
    Arg.parse_argv passed_in_args (Arg.align valid_args)
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      incorrect_input_msg
  with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      Caml.exit 2
  | Arg.Help msg ->
      Printf.eprintf "%s" msg;
      Caml.exit 0
;;

let run_as_ppx_rewriter_main ~standalone_args ~usage input =
  let valid_args = get_args ~standalone_args () in
  match List.rev @@ Array.to_list @@ input with
  | output_fn :: input_fn :: flags_and_prog_name
    when List.length flags_and_prog_name > 0 ->
      let prog_name_and_flags = List.rev flags_and_prog_name |> Array.of_list in
      parse_input prog_name_and_flags ~valid_args ~incorrect_input_msg:usage;
      interpret_mask ();
      rewrite_binary_ast_file input_fn output_fn;
      Caml.exit 0
  | [ help; _ ] when String.equal help "-help" || String.equal help "--help" ->
      parse_input input ~valid_args ~incorrect_input_msg:usage;
      assert false
  | _ ->
      Printf.eprintf "Usage: %s\n%!" usage;
      Caml.exit 2


let standalone_run_as_ppx_rewriter () =
  let n = Array.length Caml.Sys.argv in
  let usage = Printf.sprintf "%s -as-ppx [extra_args] <infile> <outfile>" exe_name in
  let argv = Array.make (n - 1) "" in
  argv.(0) <- Caml.Sys.argv.(0);
  for i = 1 to (n - 2) do
    argv.(i) <- Caml.Sys.argv.(i + 1)
  done;
  let standalone_args =
    List.map standalone_args ~f:(fun (arg, spec, _doc) ->
      (arg, spec, " Unused with -as-ppx"))
  in
  run_as_ppx_rewriter_main ~standalone_args ~usage argv
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

let run_as_ppx_rewriter () =
  let usage = Printf.sprintf "%s [extra_args] <infile> <outfile>" exe_name in
  let input = Caml.Sys.argv in
  try run_as_ppx_rewriter_main ~standalone_args:[] ~usage input
  with exn ->
    Location.report_exception Caml.Format.err_formatter exn;
    Caml.exit 1
;;

let pretty () = !pretty

let enable_checks () =
  (* We do not enable the locations check here, we currently require that one
     to be specifically enabled. *)
  perform_checks := true;
  perform_checks_on_extensions := true

let enable_location_check () =
  perform_locations_check := true

let disable_location_check () =
  perform_locations_check := false

let map_structure st =
  map_structure st
