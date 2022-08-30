open Import
open Ast_builder.Default

(* [do_insert_unused_warning_attribute] -- If true, generated code
   contains compiler attribute to disable unused warnings, instead of
   inserting [let _ = ... ]. *)
let do_insert_unused_warning_attribute = ref false
let keep_w32_impl = ref false
let keep_w32_intf = ref false

let () =
  let keep_w32_spec =
    Caml.Arg.Symbol
      ( [ "impl"; "intf"; "both" ],
        function
        | "impl" -> keep_w32_impl := true
        | "intf" -> keep_w32_intf := true
        | "both" ->
            keep_w32_impl := true;
            keep_w32_intf := true
        | _ -> assert false )
  in
  let conv_w32_spec =
    Caml.Arg.Symbol
      ( [ "code"; "attribute" ],
        function
        | "code" -> do_insert_unused_warning_attribute := false
        | "attribute" -> do_insert_unused_warning_attribute := true
        | _ -> assert false )
  in
  Driver.add_arg "-deriving-keep-w32" keep_w32_spec
    ~doc:" Do not try to disable warning 32 for the generated code";
  Driver.add_arg "-deriving-disable-w32-method" conv_w32_spec
    ~doc:" How to disable warning 32 for the generated code";
  Driver.add_arg "-type-conv-keep-w32" keep_w32_spec
    ~doc:" Deprecated, use -deriving-keep-w32";
  Driver.add_arg "-type-conv-w32" conv_w32_spec
    ~doc:" Deprecated, use -deriving-disable-w32-method"

let keep_w32_impl () = !keep_w32_impl || Driver.pretty ()
let keep_w32_intf () = !keep_w32_intf || Driver.pretty ()
let keep_w60_impl = ref false
let keep_w60_intf = ref false

let () =
  let keep_w60_spec =
    Caml.Arg.Symbol
      ( [ "impl"; "intf"; "both" ],
        function
        | "impl" -> keep_w60_impl := true
        | "intf" -> keep_w60_intf := true
        | "both" ->
            keep_w60_impl := true;
            keep_w60_intf := true
        | _ -> assert false )
  in
  Driver.add_arg "-deriving-keep-w60" keep_w60_spec
    ~doc:" Do not try to disable warning 60 for the generated code"

let keep_w60_impl () = !keep_w60_impl || Driver.pretty ()
let keep_w60_intf () = !keep_w60_intf || Driver.pretty ()

module Args = struct
  include (
    Ast_pattern :
      module type of struct
        include Ast_pattern
      end
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) Ast_pattern.t)

  type 'a param = {
    name : string;
    pattern : (expression, 'a) Ast_pattern.Packed.t;
    default : 'a;
  }

  let arg name pattern =
    {
      name;
      default = None;
      pattern = Ast_pattern.Packed.create pattern (fun x -> Some x);
    }

  let flag name =
    let pattern = pexp_ident (lident (string name)) in
    { name; default = false; pattern = Ast_pattern.Packed.create pattern true }

  type (_, _) t =
    | Nil : ('m, 'm) t
    | Cons : ('m1, 'a -> 'm2) t * 'a param -> ('m1, 'm2) t

  let empty = Nil
  let ( +> ) a b = Cons (a, b)

  let rec names : type a b. (a, b) t -> string list = function
    | Nil -> []
    | Cons (t, p) -> p.name :: names t

  module Instance = struct
    type (_, _) instance =
      | I_nil : ('m, 'm) instance
      | I_cons : ('m1, 'a -> 'm2) instance * 'a -> ('m1, 'm2) instance

    let rec create :
        type a b. (a, b) t -> (string * expression) list -> (a, b) instance =
     fun spec args ->
      match spec with
      | Nil -> I_nil
      | Cons (t, p) ->
          let value =
            match List.assoc_opt p.name args with
            | None -> p.default
            | Some expr -> Ast_pattern.Packed.parse p.pattern expr.pexp_loc expr
          in
          I_cons (create t args, value)

    let rec apply : type a b. (a, b) instance -> a -> b =
     fun t f -> match t with I_nil -> f | I_cons (t, x) -> apply t f x
  end

  let apply t args f = Instance.apply (Instance.create t args) f
end

(* +-----------------------------------------------------------------+
   | Generators                                                      |
   +-----------------------------------------------------------------+ *)

type t = string

let ignore (_ : t) = ()

type parsed_args =
  | Args of (string * expression) list
  | Unknown_syntax of Location.t * string

module Generator = struct
  type deriver = t

  type ('a, 'b) t =
    | T : {
        spec : ('c, 'a) Args.t;
        gen : ctxt:Expansion_context.Deriver.t -> 'b -> 'c;
        arg_names : String.Set.t;
        attributes : Attribute.packed list;
        deps : deriver list;
      }
        -> ('a, 'b) t

  let deps (T t) = t.deps

  module V2 = struct
    let make ?(attributes = []) ?(deps = []) spec gen =
      let arg_names = String.Set.of_list (Args.names spec) in
      T { spec; gen; arg_names; attributes; deps }

    let make_noarg ?attributes ?deps gen = make ?attributes ?deps Args.empty gen
  end

  let make ?attributes ?deps spec gen =
    V2.make ?attributes ?deps spec
      (Expansion_context.Deriver.with_loc_and_path gen)

  let make_noarg ?attributes ?deps gen = make ?attributes ?deps Args.empty gen

  let merge_accepted_args l =
    let rec loop acc = function
      | [] -> acc
      | T t :: rest -> loop (String.Set.union acc t.arg_names) rest
    in
    loop String.Set.empty l

  let check_arguments name generators (args : (string * expression) list) =
    let empty_label_error =
      List.filter_map args ~f:(fun (label, e) ->
          if String.is_empty label then
            Some
              (Location.error_extensionf ~loc:e.pexp_loc
                 "Ppxlib.Deriving: generator arguments must be labelled")
          else None)
    in
    let duplicate_argument_error =
      Option.map
        (List.find_a_dup args ~compare:(fun (a, _) (b, _) -> String.compare a b))
        ~f:(fun (label, e) ->
          Location.error_extensionf ~loc:e.pexp_loc
            "Ppxlib.Deriving: argument labelled '%s' appears more than once"
            label)
      |> Option.to_list
    in
    let accepted_args = merge_accepted_args generators in
    let unaccepted_argument =
      List.filter_map args ~f:(fun (label, e) ->
          if not (String.Set.mem label accepted_args) then
            let spellcheck_msg =
              match
                Spellcheck.spellcheck (String.Set.elements accepted_args) label
              with
              | None -> ""
              | Some s -> ".\n" ^ s
            in
            Some
              (Location.error_extensionf ~loc:e.pexp_loc
                 "Ppxlib.Deriving: generator '%s' doesn't accept argument \
                  '%s'%s"
                 name label spellcheck_msg)
          else None)
    in
    let errors =
      empty_label_error @ duplicate_argument_error @ unaccepted_argument
    in
    if List.length errors = 0 then Ok () else Error errors

  let apply (T t) ~name:_ ~ctxt x args = Args.apply t.spec args (t.gen ~ctxt x)

  let apply_all ~ctxt entry (name, generators, args) =
    let open Result in
    check_arguments name.txt generators args >>| fun () ->
    List.concat_map generators ~f:(fun t ->
        apply t ~name:name.txt ~ctxt entry args)

  let apply_all ~ctxt entry generators ext_to_item =
    let l = List.map generators ~f:(apply_all ~ctxt entry) in
    let l1, lerr =
      List.partition_map (function Ok e -> Left e | Error e -> Right e) l
    in
    let lerr =
      List.concat lerr
      |> List.map ~f:(fun err -> ext_to_item ~loc:Location.none err [])
    in
    List.concat l1 @ lerr
end

module Deriver = struct
  module Actual_deriver = struct
    type t = {
      name : string;
      str_type_decl :
        (structure, rec_flag * type_declaration list) Generator.t option;
      str_type_ext : (structure, type_extension) Generator.t option;
      str_exception : (structure, type_exception) Generator.t option;
      str_module_type_decl :
        (structure, module_type_declaration) Generator.t option;
      sig_type_decl :
        (signature, rec_flag * type_declaration list) Generator.t option;
      sig_type_ext : (signature, type_extension) Generator.t option;
      sig_exception : (signature, type_exception) Generator.t option;
      sig_module_type_decl :
        (signature, module_type_declaration) Generator.t option;
      extension :
        (loc:Location.t -> path:string -> core_type -> expression) option;
    }
  end

  module Alias = struct
    type t = {
      str_type_decl : string list;
      str_type_ext : string list;
      str_exception : string list;
      str_module_type_decl : string list;
      sig_type_decl : string list;
      sig_type_ext : string list;
      sig_exception : string list;
      sig_module_type_decl : string list;
    }
  end

  module Field = struct
    type kind = Str | Sig

    type ('a, 'b) t = {
      name : string;
      kind : kind;
      get : Actual_deriver.t -> ('a, 'b) Generator.t option;
      get_set : Alias.t -> string list;
    }

    let str_type_decl =
      {
        kind = Str;
        name = "type";
        get = (fun t -> t.str_type_decl);
        get_set = (fun t -> t.str_type_decl);
      }

    let str_type_ext =
      {
        kind = Str;
        name = "type extension";
        get = (fun t -> t.str_type_ext);
        get_set = (fun t -> t.str_type_ext);
      }

    let str_exception =
      {
        kind = Str;
        name = "exception";
        get = (fun t -> t.str_exception);
        get_set = (fun t -> t.str_exception);
      }

    let str_module_type_decl =
      {
        kind = Str;
        name = "module type";
        get = (fun t -> t.str_module_type_decl);
        get_set = (fun t -> t.str_module_type_decl);
      }

    let sig_type_decl =
      {
        kind = Sig;
        name = "signature type";
        get = (fun t -> t.sig_type_decl);
        get_set = (fun t -> t.sig_type_decl);
      }

    let sig_type_ext =
      {
        kind = Sig;
        name = "signature type extension";
        get = (fun t -> t.sig_type_ext);
        get_set = (fun t -> t.sig_type_ext);
      }

    let sig_exception =
      {
        kind = Sig;
        name = "signature exception";
        get = (fun t -> t.sig_exception);
        get_set = (fun t -> t.sig_exception);
      }

    let sig_module_type_decl =
      {
        kind = Sig;
        name = "signature module type";
        get = (fun t -> t.sig_module_type_decl);
        get_set = (fun t -> t.sig_module_type_decl);
      }
  end

  type t = Actual_deriver of Actual_deriver.t | Alias of Alias.t
  type Ppx_derivers.deriver += T of t

  let derivers () =
    List.filter_map (Ppx_derivers.derivers ()) ~f:(function
      | name, T t -> Some (name, t)
      | _ -> None)

  exception Not_supported of string

  let resolve_actual_derivers (field : (_, _) Field.t) name =
    let rec loop name collected =
      if
        List.exists collected ~f:(fun (d : Actual_deriver.t) ->
            String.equal d.name name)
      then collected
      else
        match Ppx_derivers.lookup name with
        | Some (T (Actual_deriver drv)) -> drv :: collected
        | Some (T (Alias alias)) ->
            let set = field.get_set alias in
            List.fold_right set ~init:collected ~f:loop
        | _ -> raise (Not_supported name)
    in
    List.rev (loop name [])

  let resolve_internal (field : (_, _) Field.t) name =
    List.map (resolve_actual_derivers field name) ~f:(fun drv ->
        match field.get drv with
        | None -> raise (Not_supported name)
        | Some g -> (drv.name, g))

  let supported_for field =
    List.fold_left (derivers ()) ~init:String.Set.empty ~f:(fun acc (name, _) ->
        match resolve_internal field name with
        | _ -> String.Set.add name acc
        | exception Not_supported _ -> acc)
    |> String.Set.elements

  let not_supported (field : (_, _) Field.t) ?(spellcheck = true) name =
    let spellcheck_msg =
      if spellcheck then
        match Spellcheck.spellcheck (supported_for field) name.txt with
        | None -> ""
        | Some s -> ".\n" ^ s
      else ""
    in
    Location.error_extensionf ~loc:name.loc
      "Ppxlib.Deriving: '%s' is not a supported %s deriving generator%s"
      name.txt field.name spellcheck_msg

  let resolve field name =
    try Ok (resolve_internal field name.txt)
    with Not_supported name' ->
      Error (not_supported field ~spellcheck:(String.equal name.txt name') name)

  let resolve_all field derivers =
    let derivers_and_args, derivers_and_args_errors =
      List.partition_map
        (fun (name, args) ->
          match Ppx_derivers.lookup name.txt with
          | None -> Either.Right (not_supported field name)
          | Some (T _) -> (
              (* It's one of ours, parse the arguments now. We can't do it before since
                 ppx_deriving uses a different syntax for arguments. *)
              match args with
              | Args l -> Either.Left (Some (name, l))
              | Unknown_syntax (loc, msg) ->
                  Either.Right
                    (Location.error_extensionf ~loc "Ppxlib.Deriving: %s" msg))
          | Some _ ->
              (* It's not one of ours, ignore it. *)
              Either.Left None)
        derivers
      |> fun (l1, l2) -> (List.filter_opt l1, l2)
    in
    (* Set of actual deriver names *)
    let seen = Hashtbl.create 16 in
    let result, dep_errors =
      List.fold_left ~init:([], []) derivers_and_args
        ~f:(fun (result, errors) (name, args) ->
          match resolve field name with
          | Error e -> (result, errors @ [ e ])
          | Ok named_generators ->
              let l_err =
                List.concat_map named_generators
                  ~f:(fun (actual_deriver_name, gen) ->
                    let dup_error =
                      if
                        Options.fail_on_duplicate_derivers
                        && Hashtbl.mem seen actual_deriver_name
                      then
                        [
                          Location.error_extensionf ~loc:name.loc
                            "Deriver %s appears twice" actual_deriver_name;
                        ]
                      else []
                    in
                    let l_err =
                      List.concat_map (Generator.deps gen) ~f:(fun dep ->
                          List.filter_map (resolve_actual_derivers field dep)
                            ~f:(fun drv ->
                              let dep_name = drv.name in
                              if not (Hashtbl.mem seen dep_name) then
                                Some
                                  (Location.error_extensionf ~loc:name.loc
                                     "Deriver %s is needed for %s, you need to \
                                      add it before in the list"
                                     dep_name name.txt)
                              else None))
                    in
                    Hashtbl.set seen ~key:actual_deriver_name ~data:();
                    dup_error @ l_err)
              in
              ( result @ [ (name, List.map named_generators ~f:snd, args) ],
                errors @ l_err ))
    in
    (result, derivers_and_args_errors @ dep_errors)

  let add ?str_type_decl ?str_type_ext ?str_exception ?str_module_type_decl
      ?sig_type_decl ?sig_type_ext ?sig_exception ?sig_module_type_decl
      ?extension name =
    let actual_deriver : Actual_deriver.t =
      {
        name;
        str_type_decl;
        str_type_ext;
        str_exception;
        str_module_type_decl;
        sig_type_decl;
        sig_type_ext;
        sig_exception;
        sig_module_type_decl;
        extension;
      }
    in
    Ppx_derivers.register name (T (Actual_deriver actual_deriver));
    (match extension with
    | None -> ()
    | Some f ->
        let extension =
          Extension.declare name Expression Ast_pattern.(ptyp __) f
        in
        Driver.register_transformation
          ("Ppxlib.Deriving." ^ name)
          ~rules:[ Context_free.Rule.extension extension ]);
    name

  let add_alias name ?str_type_decl ?str_type_ext ?str_exception
      ?str_module_type_decl ?sig_type_decl ?sig_type_ext ?sig_exception
      ?sig_module_type_decl set =
    let alias : Alias.t =
      let get = function None -> set | Some set -> set in
      {
        str_type_decl = get str_type_decl;
        str_type_ext = get str_type_ext;
        str_exception = get str_exception;
        str_module_type_decl = get str_module_type_decl;
        sig_type_decl = get sig_type_decl;
        sig_type_ext = get sig_type_ext;
        sig_exception = get sig_exception;
        sig_module_type_decl = get sig_module_type_decl;
      }
    in
    Ppx_derivers.register name (T (Alias alias));
    name
end

let add = Deriver.add
let add_alias = Deriver.add_alias

(* +-----------------------------------------------------------------+
   | [@@deriving ] parsing                                           |
   +-----------------------------------------------------------------+ *)

let invalid_with ~loc =
  Location.raise_errorf ~loc "invalid [@@deriving ] attribute syntax"

let generator_name_of_id loc id =
  match Longident.flatten_exn id with
  | l -> { loc; txt = String.concat ~sep:"." l }
  | exception _ -> invalid_with ~loc

exception Unknown_syntax of Location.t * string

let parse_arguments l =
  try
    Args
      (match l with
      | [ (Nolabel, e) ] -> (
          match e.pexp_desc with
          | Pexp_record (fields, None) ->
              List.map fields ~f:(fun (id, expr) ->
                  let name =
                    match id.txt with
                    | Lident s -> s
                    | _ ->
                        raise_notrace
                          (Unknown_syntax (id.loc, "simple identifier expected"))
                  in
                  (name, expr))
          | _ ->
              raise_notrace
                (Unknown_syntax
                   ( e.pexp_loc,
                     "non-optional labelled argument or record expected" )))
      | l ->
          List.map l ~f:(fun (label, expr) ->
              match label with
              | Labelled s -> (s, expr)
              | _ ->
                  raise_notrace
                    (Unknown_syntax
                       (expr.pexp_loc, "non-optional labelled argument expected"))))
  with Unknown_syntax (loc, msg) -> Unknown_syntax (loc, msg)

let mk_deriving_attr context ~prefix ~suffix =
  Attribute.declare
    (prefix ^ "deriving" ^ suffix)
    context
    Ast_pattern.(
      let generator_name () =
        map' (pexp_ident __) ~f:(fun loc f id ->
            f (generator_name_of_id loc id))
      in
      let generator () =
        map (generator_name ()) ~f:(fun f x -> f (x, Args []))
        ||| pack2
              (pexp_apply (generator_name ())
                 (map1 (many __) ~f:parse_arguments))
      in
      let generators =
        pexp_tuple (many (generator ()))
        ||| map (generator ()) ~f:(fun f x -> f [ x ])
      in
      pstr (pstr_eval generators nil ^:: nil))
    (fun x -> x)

(* +-----------------------------------------------------------------+
   | Unused warning stuff + locations check silencing                |
   +-----------------------------------------------------------------+ *)

let disable_warnings_attribute warnings =
  let loc = Location.none in
  let string =
    List.sort warnings ~cmp:Int.compare
    |> List.map ~f:(fun warning -> "-" ^ Int.to_string warning)
    |> String.concat ~sep:""
  in
  {
    attr_name = { txt = "ocaml.warning"; loc };
    attr_payload = PStr [ pstr_eval ~loc (estring ~loc string) [] ];
    attr_loc = loc;
  }

let inline_doc_attr =
  let loc = Location.none in
  {
    attr_name = { txt = "ocaml.doc"; loc };
    attr_payload = PStr [ pstr_eval ~loc (estring ~loc "@inline") [] ];
    attr_loc = loc;
  }

let wrap_str ~loc ~hide st =
  let include_infos = include_infos ~loc (pmod_structure ~loc st) in
  let pincl_attributes =
    if hide then [ inline_doc_attr; Merlin_helpers.hide_attribute ]
    else [ inline_doc_attr ]
  in
  [ pstr_include ~loc { include_infos with pincl_attributes } ]

let wrap_str ~loc ~hide st =
  let loc = { loc with loc_ghost = true } in
  let warnings, st =
    if keep_w32_impl () then ([], st)
    else if not !do_insert_unused_warning_attribute then
      ([], Ignore_unused_warning.add_dummy_user_for_values#structure st)
    else ([ 32 ], st)
  in
  let warnings, st =
    if
      keep_w60_impl ()
      || not (Ignore_unused_warning.binds_module_names#structure st false)
    then (warnings, st)
    else (60 :: warnings, st)
  in
  let wrap, st =
    if List.is_empty warnings then (hide, st)
    else (true, pstr_attribute ~loc (disable_warnings_attribute warnings) :: st)
  in
  if wrap then wrap_str ~loc ~hide st else st

let wrap_sig ~loc ~hide st =
  let include_infos = include_infos ~loc (pmty_signature ~loc st) in
  let pincl_attributes =
    if hide then [ inline_doc_attr; Merlin_helpers.hide_attribute ]
    else [ inline_doc_attr ]
  in
  [ psig_include ~loc { include_infos with pincl_attributes } ]

let wrap_sig ~loc ~hide sg =
  let loc = { loc with loc_ghost = true } in
  let warnings = if keep_w32_intf () then [] else [ 32 ] in
  let warnings =
    if
      keep_w60_intf ()
      || not (Ignore_unused_warning.binds_module_names#signature sg false)
    then warnings
    else 60 :: warnings
  in
  let wrap, sg =
    if List.is_empty warnings then (hide, sg)
    else (true, psig_attribute ~loc (disable_warnings_attribute warnings) :: sg)
  in
  if wrap then wrap_sig ~loc ~hide sg else sg

(* +-----------------------------------------------------------------+
   | Remove attributes used by syntax extensions                     |
   +-----------------------------------------------------------------+ *)
(*
let remove generators =
  let attributes =
    List.concat_map generators ~f:(fun (_, actual_generators, _) ->
      List.concat_map actual_generators ~f:(fun (Generator.T g) -> g.attributes))
  in
  object
    inherit Ast_traverse.map

    (* Don't recurse through attributes and extensions *)
    method! attribute x = x
    method! extension x = x

    method! label_declaration ld =
      Attribute.remove_seen Attribute.Context.label_declaration attributes ld

    method! constructor_declaration cd =
      Attribute.remove_seen Attribute.Context.constructor_declaration attributes cd
  end
*)
(* +-----------------------------------------------------------------+
   | Main expansion                                                  |
   +-----------------------------------------------------------------+ *)

let types_used_by_deriving (tds : type_declaration list) : structure_item list =
  if keep_w32_impl () then []
  else
    List.map tds ~f:(fun td ->
        let typ = Common.core_type_of_type_declaration td in
        let loc = td.ptype_loc in
        pstr_value ~loc Nonrecursive
          [
            value_binding ~loc ~pat:(ppat_any ~loc)
              ~expr:
                (pexp_fun ~loc Nolabel None
                   (ppat_constraint ~loc (ppat_any ~loc) typ)
                   (eunit ~loc));
          ])

let merge_generators field l =
  List.filter_map l ~f:(fun x -> x) |> List.concat |> Deriver.resolve_all field

let expand_str_type_decls ~ctxt rec_flag tds values =
  let generators, l_err = merge_generators Deriver.Field.str_type_decl values in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.pstr_extension ~loc:Location.none err [])
      l_err
  in
  (* TODO: instead of disabling the unused warning for types themselves, we
     should add a tag [@@unused]. *)
  let generated =
    types_used_by_deriving tds @ l_err
    @ Generator.apply_all ~ctxt (rec_flag, tds) generators
        Ast_builder.Default.pstr_extension
  in
  wrap_str
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_sig_type_decls ~ctxt rec_flag tds values =
  let generators, l_err = merge_generators Deriver.Field.sig_type_decl values in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.psig_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt (rec_flag, tds) generators
        Ast_builder.Default.psig_extension
  in
  wrap_sig
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_str_module_type_decl ~ctxt mtd generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.str_module_type_decl generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.pstr_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt mtd generators
        Ast_builder.Default.pstr_extension
  in

  wrap_str
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_sig_module_type_decl ~ctxt mtd generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.sig_module_type_decl generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.psig_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt mtd generators
        Ast_builder.Default.psig_extension
  in
  wrap_sig
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_str_exception ~ctxt ec generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.str_exception generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.pstr_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt ec generators Ast_builder.Default.pstr_extension
  in
  wrap_str
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_sig_exception ~ctxt ec generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.sig_exception generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.psig_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt ec generators Ast_builder.Default.psig_extension
  in
  wrap_sig
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_str_type_ext ~ctxt te generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.str_type_ext generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.pstr_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt te generators Ast_builder.Default.pstr_extension
  in
  wrap_str
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let expand_sig_type_ext ~ctxt te generators =
  let generators, l_err =
    Deriver.resolve_all Deriver.Field.sig_type_ext generators
  in
  let l_err =
    List.map
      ~f:(fun err ->
        Ast_builder.Default.psig_extension ~loc:Location.none err [])
      l_err
  in
  let generated =
    l_err
    @ Generator.apply_all ~ctxt te generators Ast_builder.Default.psig_extension
  in
  wrap_sig
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~hide:(not @@ Expansion_context.Deriver.inline ctxt)
    generated

let rules ~typ ~expand_sig ~expand_str ~rule_str ~rule_sig ~rule_str_expect
    ~rule_sig_expect =
  let prefix = "ppxlib." in
  let deriving_attr = mk_deriving_attr ~suffix:"" ~prefix typ in
  let deriving_attr_expect = mk_deriving_attr ~suffix:"_inline" ~prefix typ in
  [
    rule_sig deriving_attr expand_sig;
    rule_str deriving_attr expand_str;
    rule_str_expect deriving_attr_expect expand_str;
    rule_sig_expect deriving_attr_expect expand_sig;
  ]

let rules_type_decl =
  rules ~typ:Type_declaration ~expand_str:expand_str_type_decls
    ~expand_sig:expand_sig_type_decls
    ~rule_str:Context_free.Rule.attr_str_type_decl
    ~rule_sig:Context_free.Rule.attr_sig_type_decl
    ~rule_str_expect:Context_free.Rule.attr_str_type_decl_expect
    ~rule_sig_expect:Context_free.Rule.attr_sig_type_decl_expect

let rules_type_ext =
  rules ~typ:Type_extension ~expand_str:expand_str_type_ext
    ~expand_sig:expand_sig_type_ext
    ~rule_str:Context_free.Rule.attr_str_type_ext
    ~rule_sig:Context_free.Rule.attr_sig_type_ext
    ~rule_str_expect:Context_free.Rule.attr_str_type_ext_expect
    ~rule_sig_expect:Context_free.Rule.attr_sig_type_ext_expect

let rules_exception =
  rules ~typ:Type_exception ~expand_str:expand_str_exception
    ~expand_sig:expand_sig_exception
    ~rule_str:Context_free.Rule.attr_str_exception
    ~rule_sig:Context_free.Rule.attr_sig_exception
    ~rule_str_expect:Context_free.Rule.attr_str_exception_expect
    ~rule_sig_expect:Context_free.Rule.attr_sig_exception_expect

let rules_module_type_decl =
  rules ~typ:Module_type_declaration ~expand_str:expand_str_module_type_decl
    ~expand_sig:expand_sig_module_type_decl
    ~rule_str:Context_free.Rule.attr_str_module_type_decl
    ~rule_sig:Context_free.Rule.attr_sig_module_type_decl
    ~rule_str_expect:Context_free.Rule.attr_str_module_type_decl_expect
    ~rule_sig_expect:Context_free.Rule.attr_sig_module_type_decl_expect

let () =
  let rules =
    [ rules_type_decl; rules_type_ext; rules_exception; rules_module_type_decl ]
    |> List.concat
  in
  Driver.register_transformation "deriving" ~aliases:[ "type_conv" ] ~rules
