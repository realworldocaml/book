open! Import

let poly_equal a b =
  let module Poly = struct
    type t = T : _ -> t
  end in
  Stdppx.Poly.equal (Poly.T a) (Poly.T b)

module Context = struct
  type 'a t =
    | Label_declaration : label_declaration t
    | Constructor_declaration : constructor_declaration t
    | Type_declaration : type_declaration t
    | Type_exception : type_exception t
    | Type_extension : type_extension t
    | Extension_constructor : extension_constructor t
    | Pattern : pattern t
    | Core_type : core_type t
    | Expression : expression t
    | Value_description : value_description t
    | Class_type : class_type t
    | Class_type_field : class_type_field t
    | Class_infos : _ class_infos t
    | Class_expr : class_expr t
    | Class_field : class_field t
    | Module_type : module_type t
    | Module_declaration : module_declaration t
    | Module_type_declaration : module_type_declaration t
    | Module_substitution : module_substitution t
    | Open_description : open_description t
    | Open_declaration : open_declaration t
    | Include_infos : _ include_infos t
    | Module_expr : module_expr t
    | Value_binding : value_binding t
    | Module_binding : module_binding t
    | Pstr_eval : structure_item t
    | Pstr_extension : structure_item t
    | Psig_extension : signature_item t
    | Rtag : row_field t
    | Object_type_field : object_field t

  let label_declaration = Label_declaration
  let constructor_declaration = Constructor_declaration
  let type_declaration = Type_declaration
  let type_extension = Type_extension
  let type_exception = Type_exception
  let extension_constructor = Extension_constructor
  let pattern = Pattern
  let core_type = Core_type
  let expression = Expression
  let value_description = Value_description
  let class_type = Class_type
  let class_type_field = Class_type_field
  let class_infos = Class_infos
  let class_expr = Class_expr
  let class_field = Class_field
  let module_type = Module_type
  let module_declaration = Module_declaration
  let module_type_declaration = Module_type_declaration
  let open_description = Open_description
  let include_infos = Include_infos
  let module_expr = Module_expr
  let value_binding = Value_binding
  let module_binding = Module_binding
  let pstr_eval = Pstr_eval
  let pstr_extension = Pstr_extension
  let psig_extension = Psig_extension
  let rtag = Rtag
  let object_type_field = Object_type_field

  let get_pstr_eval st =
    match st.pstr_desc with
    | Pstr_eval (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_pstr_eval"

  let get_pstr_extension st =
    match st.pstr_desc with
    | Pstr_extension (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_pstr_extension"

  let get_psig_extension st =
    match st.psig_desc with
    | Psig_extension (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_psig_extension"

  let get_attributes : type a. a t -> a -> attributes =
   fun t x ->
    match t with
    | Label_declaration -> x.pld_attributes
    | Constructor_declaration -> x.pcd_attributes
    | Type_declaration -> x.ptype_attributes
    | Type_extension -> x.ptyext_attributes
    | Type_exception -> x.ptyexn_attributes
    | Extension_constructor -> x.pext_attributes
    | Pattern -> x.ppat_attributes
    | Core_type -> x.ptyp_attributes
    | Expression -> x.pexp_attributes
    | Value_description -> x.pval_attributes
    | Class_type -> x.pcty_attributes
    | Class_type_field -> x.pctf_attributes
    | Class_infos -> x.pci_attributes
    | Class_expr -> x.pcl_attributes
    | Class_field -> x.pcf_attributes
    | Module_type -> x.pmty_attributes
    | Module_declaration -> x.pmd_attributes
    | Module_type_declaration -> x.pmtd_attributes
    | Module_substitution -> x.pms_attributes
    | Open_description -> x.popen_attributes
    | Open_declaration -> x.popen_attributes
    | Include_infos -> x.pincl_attributes
    | Module_expr -> x.pmod_attributes
    | Value_binding -> x.pvb_attributes
    | Module_binding -> x.pmb_attributes
    | Pstr_eval -> snd (get_pstr_eval x)
    | Pstr_extension -> snd (get_pstr_extension x)
    | Psig_extension -> snd (get_psig_extension x)
    | Rtag -> x.prf_attributes
    | Object_type_field -> x.pof_attributes

  let set_attributes : type a. a t -> a -> attributes -> a =
   fun t x attrs ->
    match t with
    | Label_declaration -> { x with pld_attributes = attrs }
    | Constructor_declaration -> { x with pcd_attributes = attrs }
    | Type_declaration -> { x with ptype_attributes = attrs }
    | Type_extension -> { x with ptyext_attributes = attrs }
    | Type_exception -> { x with ptyexn_attributes = attrs }
    | Extension_constructor -> { x with pext_attributes = attrs }
    | Pattern -> { x with ppat_attributes = attrs }
    | Core_type -> { x with ptyp_attributes = attrs }
    | Expression -> { x with pexp_attributes = attrs }
    | Value_description -> { x with pval_attributes = attrs }
    | Class_type -> { x with pcty_attributes = attrs }
    | Class_type_field -> { x with pctf_attributes = attrs }
    | Class_infos -> { x with pci_attributes = attrs }
    | Class_expr -> { x with pcl_attributes = attrs }
    | Class_field -> { x with pcf_attributes = attrs }
    | Module_type -> { x with pmty_attributes = attrs }
    | Module_declaration -> { x with pmd_attributes = attrs }
    | Module_type_declaration -> { x with pmtd_attributes = attrs }
    | Module_substitution -> { x with pms_attributes = attrs }
    | Open_description -> { x with popen_attributes = attrs }
    | Open_declaration -> { x with popen_attributes = attrs }
    | Include_infos -> { x with pincl_attributes = attrs }
    | Module_expr -> { x with pmod_attributes = attrs }
    | Value_binding -> { x with pvb_attributes = attrs }
    | Module_binding -> { x with pmb_attributes = attrs }
    | Pstr_eval ->
        { x with pstr_desc = Pstr_eval (get_pstr_eval x |> fst, attrs) }
    | Pstr_extension ->
        {
          x with
          pstr_desc = Pstr_extension (get_pstr_extension x |> fst, attrs);
        }
    | Psig_extension ->
        {
          x with
          psig_desc = Psig_extension (get_psig_extension x |> fst, attrs);
        }
    | Rtag -> { x with prf_attributes = attrs }
    | Object_type_field -> { x with pof_attributes = attrs }

  let desc : type a. a t -> string = function
    | Label_declaration -> "label declaration"
    | Constructor_declaration -> "constructor declaration"
    | Type_declaration -> "type declaration"
    | Type_extension -> "type extension"
    | Type_exception -> "type exception"
    | Extension_constructor -> "extension constructor"
    | Pattern -> "pattern"
    | Core_type -> "core type"
    | Expression -> "expression"
    | Value_description -> "value"
    | Class_type -> "class type"
    | Class_type_field -> "class type field"
    | Class_infos -> "class declaration"
    | Class_expr -> "class expression"
    | Class_field -> "class field"
    | Module_type -> "module type"
    | Module_declaration -> "module declaration"
    | Module_type_declaration -> "module type declaration"
    | Module_substitution -> "module substitution"
    | Open_description -> "open"
    | Open_declaration -> "open"
    | Include_infos -> "include"
    | Module_expr -> "module expression"
    | Value_binding -> "value binding"
    | Module_binding -> "module binding"
    | Pstr_eval -> "toplevel expression"
    | Pstr_extension -> "toplevel extension"
    | Psig_extension -> "toplevel signature extension"
    | Rtag -> "polymorphic variant tag"
    | Object_type_field -> "object type field"

  (*
  let pattern : type a b c d. a t
    -> (attributes, b, c) Ast_pattern.t
    -> (a, c, d) Ast_pattern.t
    -> (a, b, d) Ast_pattern.t = function
    | Label_declaration       -> Ast_pattern.pld_attributes
    | Constructor_declaration -> Ast_pattern.pcd_attributes
    | Type_declaration        -> Ast_pattern.ptype_attributes
    | Type_extension          -> Ast_pattern.ptyext_attributes
    | Extension_constructor   -> Ast_pattern.pext_attributes
*)

  let equal : _ t -> _ t -> bool = poly_equal
end

module Floating_context = struct
  type 'a t =
    | Structure_item : structure_item t
    | Signature_item : signature_item t
    | Class_field : class_field t
    | Class_type_field : class_type_field t

  let structure_item = Structure_item
  let signature_item = Signature_item
  let class_field = Class_field
  let class_type_field = Class_type_field

  let get_attribute_if_is_floating_node : type a. a t -> a -> attribute option =
   fun t x ->
    match (t, x) with
    | Structure_item, { pstr_desc = Pstr_attribute a; _ } -> Some a
    | Signature_item, { psig_desc = Psig_attribute a; _ } -> Some a
    | Class_field, { pcf_desc = Pcf_attribute a; _ } -> Some a
    | Class_type_field, { pctf_desc = Pctf_attribute a; _ } -> Some a
    | _ -> None

  let get_attribute t x =
    match get_attribute_if_is_floating_node t x with
    | Some a -> a
    | None -> failwith "Attribute.Floating.Context.get_attribute"

  let replace_by_dummy : type a. a t -> a -> a =
    let dummy_ext = ({ txt = ""; loc = Location.none }, PStr []) in
    fun t x ->
      match t with
      | Structure_item -> { x with pstr_desc = Pstr_extension (dummy_ext, []) }
      | Signature_item -> { x with psig_desc = Psig_extension (dummy_ext, []) }
      | Class_field -> { x with pcf_desc = Pcf_extension dummy_ext }
      | Class_type_field -> { x with pctf_desc = Pctf_extension dummy_ext }

  let desc : type a. a t -> string = function
    | Structure_item -> "structure item"
    | Signature_item -> "signature item"
    | Class_field -> "class field"
    | Class_type_field -> "class type field"

  let equal : _ t -> _ t -> bool = poly_equal
end

type packed_context =
  | On_item : _ Context.t -> packed_context
  | Floating : _ Floating_context.t -> packed_context

type _ payload_parser =
  | Payload_parser :
      (payload, 'a, 'b) Ast_pattern.t * (name_loc:Location.t -> 'a)
      -> 'b payload_parser

type ('a, 'b) t = {
  name : Name.Pattern.t;
  context : 'a Context.t;
  payload : 'b payload_parser;
}

type packed = T : (_, _) t -> packed

let name t = Name.Pattern.name t.name
let context t = t.context

let registrar =
  Name.Registrar.create ~kind:"attribute" ~current_file:__FILE__
    ~string_of_context:(function
    | On_item t -> Some (Context.desc t)
    | Floating t -> Some (Floating_context.desc t ^ " (floating)"))

let declare_with_name_loc name context pattern k =
  Name.Registrar.register ~kind:`Attribute registrar (On_item context) name;
  {
    name = Name.Pattern.make name;
    context;
    payload = Payload_parser (pattern, k);
  }

let declare name context pattern k =
  declare_with_name_loc name context pattern (fun ~name_loc:_ -> k)

module Attribute_table = Caml.Hashtbl.Make (struct
  type t = string loc

  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = Poly.equal
end)

let not_seen = Attribute_table.create 128
let mark_as_seen { attr_name; _ } = Attribute_table.remove not_seen attr_name
let mark_as_handled_manually = mark_as_seen

let explicitly_drop =
  object
    inherit Ast_traverse.iter
    method! attribute = mark_as_seen
  end

let get_internal =
  let rec find_best_match t attributes longest_match =
    match attributes with
    | [] -> Ok longest_match
    | ({ attr_name = name; _ } as attr) :: rest ->
        if Name.Pattern.matches t.name name.txt then
          match longest_match with
          | None -> find_best_match t rest (Some attr)
          | Some { attr_name = name'; _ } ->
              let len = String.length name.txt in
              let len' = String.length name'.txt in
              if len > len' then find_best_match t rest (Some attr)
              else if len < len' then find_best_match t rest longest_match
              else
                Error
                  ( Location.Error.createf ~loc:name.loc "Duplicated attribute",
                    [] )
        else find_best_match t rest longest_match
  in
  fun t attributes -> find_best_match t attributes None

let convert ?(do_mark_as_seen = true) pattern attr =
  if do_mark_as_seen then mark_as_seen attr;
  let (Payload_parser (pattern, k)) = pattern in
  Ast_pattern.parse_res pattern
    (Common.loc_of_payload attr)
    attr.attr_payload
    (k ~name_loc:attr.attr_name.loc)

let get_res t ?mark_as_seen:do_mark_as_seen x =
  let open Result in
  let attrs = Context.get_attributes t.context x in
  get_internal t attrs >>= fun res ->
  match res with
  | None -> Ok None
  | Some attr ->
      convert t.payload attr ?do_mark_as_seen >>| fun value -> Some value

let get t ?mark_as_seen:do_mark_as_seen x =
  get_res t ?mark_as_seen:do_mark_as_seen x
  |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)

let consume_res t x =
  let open Result in
  let attrs = Context.get_attributes t.context x in
  get_internal t attrs >>= fun res ->
  match res with
  | None -> Ok None
  | Some attr ->
      let attrs = List.filter attrs ~f:(fun attr' -> not (attr == attr')) in
      let x = Context.set_attributes t.context x attrs in
      convert t.payload attr >>| fun value -> Some (x, value)

let consume t x =
  consume_res t x
  |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)

let remove_seen_res (type a) (context : a Context.t) packeds (x : a) =
  let open Result in
  let attrs = Context.get_attributes context x in
  let rec loop acc = function
    | [] -> Ok acc
    | T t :: rest ->
        if Context.equal t.context context then
          get_internal t attrs >>= fun res ->
          match res with
          | None -> loop acc rest
          | Some attr ->
              let name = attr.attr_name in
              if Attribute_table.mem not_seen name then loop acc rest
              else loop (attr :: acc) rest
        else loop acc rest
  in
  loop [] packeds >>| fun matched ->
  let attrs =
    List.filter attrs ~f:(fun attr' -> not (List.memq ~set:matched attr'))
  in
  Context.set_attributes context x attrs

let remove_seen (type a) (context : a Context.t) packeds (x : a) =
  remove_seen_res (context : a Context.t) packeds (x : a)
  |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)

let pattern_res t p =
  let open Result in
  let f = Ast_pattern.to_func p in
  Ast_pattern.of_func (fun ctx loc x k ->
      consume_res t x >>| fun res ->
      match res with
      | None -> f ctx loc x (k None)
      | Some (x, v) -> f ctx loc x (k (Some v)))

let pattern t p =
  pattern_res t p |> Ast_pattern.to_func
  |> (fun f a b c d ->
       f a b c d
       |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err))
  |> Ast_pattern.of_func

module Floating = struct
  module Context = Floating_context

  type ('a, 'b) t = {
    name : Name.Pattern.t;
    context : 'a Context.t;
    payload : 'b payload_parser;
  }

  let name t = Name.Pattern.name t.name

  let declare name context pattern k =
    Name.Registrar.register ~kind:`Attribute registrar (Floating context) name;
    {
      name = Name.Pattern.make name;
      context;
      payload = Payload_parser (pattern, fun ~name_loc:_ -> k);
    }

  let convert_res ts x =
    let open Result in
    match ts with
    | [] -> Ok None
    | { context; _ } :: _ -> (
        assert (List.for_all ts ~f:(fun t -> Context.equal t.context context));
        let attr = Context.get_attribute context x in
        let name = attr.attr_name in
        match
          List.filter ts ~f:(fun t -> Name.Pattern.matches t.name name.txt)
        with
        | [] -> Ok None
        | [ t ] -> convert t.payload attr >>| fun value -> Some value
        | l ->
            Error
              ( Location.Error.createf ~loc:name.loc
                  "Multiple match for floating attributes: %s"
                  (String.concat ~sep:", "
                     (List.map l ~f:(fun t -> Name.Pattern.name t.name))),
                [] ))

  let convert ts x =
    convert_res ts x
    |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)
end

let collect_attribute_errors registrar context name =
  if
    (not
       (Name.Allowlisted.is_allowlisted ~kind:`Attribute name.txt
       || Name.ignore_checks name.txt))
    && Attribute_table.mem not_seen name
  then
    let allowlist = Name.Allowlisted.get_attribute_list () in
    [
      Name.Registrar.Error.createf registrar context ~allowlist
        "Attribute `%s' was not used" name;
    ]
  else []

let collect_unused_attributes_errors =
  object (self)
    inherit [Location.Error.t list] Ast_traverse.fold as super

    method! attribute { attr_name = name; _ } _ =
      [
        Location.Error.createf ~loc:name.loc
          "attribute not expected here, Ppxlib.Attribute needs updating!";
      ]

    method private check_node : type a.
        a Context.t -> a -> a * Location.Error.t list =
      fun context node ->
        let attrs = Context.get_attributes context node in
        match attrs with
        | [] -> (node, [])
        | _ ->
            let errors =
              List.map attrs
                ~f:(fun
                     ({ attr_name = name; attr_payload = payload; _ } as attr)
                   ->
                  let collected_errors =
                    self#payload payload []
                    @ collect_attribute_errors registrar (On_item context) name
                  in
                  (* If we allow the attribute to pass through, mark it as seen *)
                  mark_as_seen attr;
                  collected_errors)
              |> List.concat
            in
            (Context.set_attributes context node [], errors)

    method private check_floating : type a.
        a Floating.Context.t -> a -> a * Location.Error.t list =
      fun context node ->
        match
          Floating.Context.get_attribute_if_is_floating_node context node
        with
        | None -> (node, [])
        | Some ({ attr_name = name; attr_payload = payload; _ } as attr) ->
            let collected_errors =
              self#payload payload []
              @ collect_attribute_errors registrar (Floating context) name
            in
            mark_as_seen attr;
            (Floating.Context.replace_by_dummy context node, collected_errors)

    method! label_declaration x acc =
      let res, errors = self#check_node Label_declaration x in
      super#label_declaration res (acc @ errors)

    method! constructor_declaration x acc =
      let res, errors = self#check_node Constructor_declaration x in
      super#constructor_declaration res (acc @ errors)

    method! type_declaration x acc =
      let res, errors = self#check_node Type_declaration x in
      super#type_declaration res (acc @ errors)

    method! type_extension x acc =
      let res, errors = self#check_node Type_extension x in
      super#type_extension res (acc @ errors)

    method! type_exception x acc =
      let res, errors = self#check_node Type_exception x in
      super#type_exception res (acc @ errors)

    method! extension_constructor x acc =
      let res, errors = self#check_node Extension_constructor x in
      super#extension_constructor res (acc @ errors)

    method! pattern x acc =
      let res, errors = self#check_node Pattern x in
      super#pattern res (acc @ errors)

    method! core_type x acc =
      let res, errors = self#check_node Core_type x in
      super#core_type res (acc @ errors)

    method! expression x acc =
      let res, errors = self#check_node Expression x in
      super#expression res (acc @ errors)

    method! value_description x acc =
      let res, errors = self#check_node Value_description x in
      super#value_description res (acc @ errors)

    method! class_type x acc =
      let res, errors = self#check_node Class_type x in
      super#class_type res (acc @ errors)

    method! class_infos f x acc =
      let res, errors = self#check_node Class_infos x in
      super#class_infos f res (acc @ errors)

    method! class_expr x acc =
      let res, errors = self#check_node Class_expr x in
      super#class_expr res (acc @ errors)

    method! module_type x acc =
      let res, errors = self#check_node Module_type x in
      super#module_type res (acc @ errors)

    method! module_declaration x acc =
      let res, errors = self#check_node Module_declaration x in
      super#module_declaration res (acc @ errors)

    method! module_type_declaration x acc =
      let res, errors = self#check_node Module_type_declaration x in
      super#module_type_declaration res (acc @ errors)

    method! open_description x acc =
      let res, errors = self#check_node Open_description x in
      super#open_description res (acc @ errors)

    method! open_declaration x acc =
      let res, errors = self#check_node Open_declaration x in
      super#open_declaration res (acc @ errors)

    method! include_infos f x acc =
      let res, errors = self#check_node Include_infos x in
      super#include_infos f res (acc @ errors)

    method! module_expr x acc =
      let res, errors = self#check_node Module_expr x in
      super#module_expr res (acc @ errors)

    method! value_binding x acc =
      let res, errors = self#check_node Value_binding x in
      super#value_binding res (acc @ errors)

    method! module_binding x acc =
      let res, errors = self#check_node Module_binding x in
      super#module_binding res (acc @ errors)

    method! class_field x acc =
      let x, errors1 = self#check_node Class_field x in
      let x, errors2 = self#check_floating Class_field x in
      super#class_field x (acc @ errors1 @ errors2)

    method! class_type_field x acc =
      let x, errors1 = self#check_node Class_type_field x in
      let x, errors2 = self#check_floating Class_type_field x in
      super#class_type_field x (acc @ errors1 @ errors2)

    method! row_field x acc =
      let x, errors =
        match x.prf_desc with Rtag _ -> self#check_node Rtag x | _ -> (x, [])
      in
      super#row_field x (acc @ errors)

    method! core_type_desc x acc =
      let x, errors =
        match x with
        | Ptyp_object (fields, closed_flag) ->
            let fields, errors =
              List.map fields ~f:(self#check_node Object_type_field)
              |> List.split
            in
            (Ptyp_object (fields, closed_flag), List.concat errors)
        | _ -> (x, [])
      in
      super#core_type_desc x (acc @ errors)

    method! structure_item item acc =
      let item, errors = self#check_floating Structure_item item in
      let item, errors2 =
        match item.pstr_desc with
        | Pstr_eval _ -> self#check_node Pstr_eval item
        | Pstr_extension _ -> self#check_node Pstr_extension item
        | _ -> (item, [])
      in
      super#structure_item item (acc @ errors @ errors2)

    method! signature_item item acc =
      let item, errors = self#check_floating Signature_item item in
      let item, errors2 =
        match item.psig_desc with
        | Psig_extension _ -> self#check_node Psig_extension item
        | _ -> (item, [])
      in
      super#signature_item item (acc @ errors @ errors2)
  end

let check_attribute registrar context name =
  match collect_attribute_errors registrar context name with
  | [] -> ()
  | err :: _ -> Location.Error.raise err

let raise_if_non_empty = function
  | [] -> ()
  | err :: _ -> Location.Error.raise err

let check_unused =
  object (self)
    inherit Ast_traverse.iter as super

    method private check_node : type a. a Context.t -> a -> a =
      fun context node ->
        let attrs = Context.get_attributes context node in
        match attrs with
        | [] -> node
        | _ ->
            List.iter attrs
              ~f:(fun ({ attr_name = name; attr_payload = payload; _ } as attr)
                 ->
                self#payload payload;
                check_attribute registrar (On_item context) name;
                (* If we allow the attribute to pass through, mark it as seen *)
                mark_as_seen attr);
            Context.set_attributes context node []

    method! attribute { attr_name = name; _ } =
      Location.raise_errorf ~loc:name.loc
        "attribute not expected here, Ppxlib.Attribute needs updating!"

    method! label_declaration x =
      collect_unused_attributes_errors#label_declaration x []
      |> raise_if_non_empty

    method! constructor_declaration x =
      collect_unused_attributes_errors#constructor_declaration x []
      |> raise_if_non_empty

    method! type_declaration x =
      collect_unused_attributes_errors#type_declaration x []
      |> raise_if_non_empty

    method! type_extension x =
      collect_unused_attributes_errors#type_extension x [] |> raise_if_non_empty

    method! type_exception x =
      collect_unused_attributes_errors#type_exception x [] |> raise_if_non_empty

    method! extension_constructor x =
      collect_unused_attributes_errors#extension_constructor x []
      |> raise_if_non_empty

    method! pattern x =
      collect_unused_attributes_errors#pattern x [] |> raise_if_non_empty

    method! core_type x =
      collect_unused_attributes_errors#core_type x [] |> raise_if_non_empty

    method! expression x =
      collect_unused_attributes_errors#expression x [] |> raise_if_non_empty

    method! value_description x =
      collect_unused_attributes_errors#value_description x []
      |> raise_if_non_empty

    method! class_type x =
      collect_unused_attributes_errors#class_type x [] |> raise_if_non_empty

    method! class_infos f x =
      super#class_infos f (self#check_node Class_infos x)

    method! class_expr x =
      collect_unused_attributes_errors#class_expr x [] |> raise_if_non_empty

    method! module_type x =
      collect_unused_attributes_errors#module_type x [] |> raise_if_non_empty

    method! module_declaration x =
      collect_unused_attributes_errors#module_declaration x []
      |> raise_if_non_empty

    method! module_type_declaration x =
      collect_unused_attributes_errors#module_type_declaration x []
      |> raise_if_non_empty

    method! open_description x =
      collect_unused_attributes_errors#open_description x []
      |> raise_if_non_empty

    method! open_declaration x =
      collect_unused_attributes_errors#open_declaration x []
      |> raise_if_non_empty

    method! include_infos f x =
      super#include_infos f (self#check_node Include_infos x)

    method! module_expr x =
      collect_unused_attributes_errors#module_expr x [] |> raise_if_non_empty

    method! value_binding x =
      collect_unused_attributes_errors#value_binding x [] |> raise_if_non_empty

    method! module_binding x =
      collect_unused_attributes_errors#module_binding x [] |> raise_if_non_empty

    method! class_field x =
      collect_unused_attributes_errors#class_field x [] |> raise_if_non_empty

    method! class_type_field x =
      collect_unused_attributes_errors#class_type_field x []
      |> raise_if_non_empty

    method! row_field x =
      collect_unused_attributes_errors#row_field x [] |> raise_if_non_empty

    method! core_type_desc x =
      collect_unused_attributes_errors#core_type_desc x [] |> raise_if_non_empty

    method! structure_item item =
      collect_unused_attributes_errors#structure_item item []
      |> raise_if_non_empty

    method! signature_item item =
      collect_unused_attributes_errors#signature_item item []
      |> raise_if_non_empty
  end

let reset_checks () = Attribute_table.clear not_seen

let collect =
  object
    inherit Ast_traverse.iter as super

    method! attribute ({ attr_name = name; attr_payload = payload; _ } as attr)
        =
      let loc = Common.loc_of_attribute attr in
      super#payload payload;
      Attribute_table.add not_seen name loc
  end

let collect_unseen_errors () =
  let fail name loc acc =
    let txt = name.txt in
    if not (Name.ignore_checks txt) then
      Location.Error.createf ~loc "Attribute `%s' was silently dropped" txt
      :: acc
    else acc
  in
  Attribute_table.fold fail not_seen []

let check_all_seen () =
  match collect_unseen_errors () with
  | [] -> ()
  | err :: _ -> Location.Error.raise err

let remove_attributes_present_in table =
  object
    inherit Ast_traverse.iter as super

    method! attribute { attr_name = name; attr_payload = payload; _ } =
      super#payload payload;
      Attribute_table.remove table name
  end

let copy_of_not_seen () =
  let copy = Attribute_table.create (Attribute_table.length not_seen) in
  Attribute_table.iter (Attribute_table.add copy) not_seen;
  copy

let dropped_so_far_structure st =
  let table = copy_of_not_seen () in
  (remove_attributes_present_in table)#structure st;
  Attribute_table.fold
    (fun name loc acc -> { txt = name.txt; loc } :: acc)
    table []

let dropped_so_far_signature sg =
  let table = copy_of_not_seen () in
  (remove_attributes_present_in table)#signature sg;
  Attribute_table.fold
    (fun name loc acc -> { txt = name.txt; loc } :: acc)
    table []
