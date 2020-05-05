(*
  Tools shared between OCaml code generators.
  (ox means OCaml-X)
*)

open Atd.Import
open Mapping

type 'a expr = (Ocaml.Repr.t, 'a) Mapping.mapping
type 'a def = (Ocaml.Repr.t, 'a) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type name = (loc * loc * string)
(* location of the containing record or variant,
   location of the field definition,
   field/constructor name *)

type names = {
  field_names : name list list;
  poly_variant_names : name list list;
  classic_variant_names : name list list;
}

type target =
  | Files of string
  | Stdout

let rec extract_names_from_expr ?(is_root = false) root_loc acc (x : 'a expr) =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float  _
  | String _ -> acc
  | Sum (loc, va, o, _) ->
      let l, (fn, pvn, cvn) =
        Array.fold_left (extract_names_from_variant root_loc) ([], acc) va
      in
      (match o with
         Sum x ->
           (match x with
              Poly -> (fn, l :: pvn, cvn)
            | Classic ->
                if is_root then (fn, pvn, l :: cvn)
                else
                  Error.error loc
                    "Anonymous classic variant types are not allowed \
                     by OCaml."
           )
       | _ -> assert false
      )

  | Record (loc, fa, _, _) ->
      if is_root then
        let l, (fn, pvn, cvn) =
          Array.fold_left (extract_names_from_field root_loc) ([], acc) fa
        in
        (l :: fn, pvn, cvn)
      else
        Error.error loc "Anonymous record types are not allowed by OCaml."

  | Tuple (_, ca, _, _) ->
      Array.fold_left (extract_names_from_cell root_loc) acc ca

  | List (_, x, _, _)
  | Option (_, x, _, _)
  | Nullable (_, x, _, _)
  | Wrap (_, x, _, _) ->
      extract_names_from_expr root_loc acc x

  | Name (_, _, l, _, _) ->
      List.fold_left (extract_names_from_expr root_loc) acc l

  | External (_, _, l, _, _) ->
      List.fold_left (extract_names_from_expr root_loc) acc l

  | Tvar _ -> acc

and extract_names_from_variant root_loc (l, acc) x =
  let l =
    match x.var_arepr with
      Variant v -> (root_loc, x.var_loc, v.Ocaml.ocaml_cons) :: l
    | _ -> assert false
  in
  match x.var_arg with
    None -> (l, acc)
  | Some x ->
      (l, extract_names_from_expr root_loc acc x)

and extract_names_from_field root_loc (l, acc) x =
  let l =
    match x.f_arepr with
      Field f -> (root_loc, x.f_loc, f.Ocaml.ocaml_fname) :: l
    | _ -> assert false
  in
  (l, extract_names_from_expr root_loc acc x.f_value)

and extract_names_from_cell root_loc acc x =
  extract_names_from_expr root_loc acc x.cel_value


let extract_ocaml_names_from_defs l =
  let fn, pvn, cvn =
    List.fold_left (
      fun acc def ->
        match def.def_value with
          None -> acc
        | Some x ->
            let root_loc = loc_of_mapping x in
            extract_names_from_expr ~is_root:true root_loc acc x
    ) ([], [], []) l
  in
  {
    field_names = List.rev fn;
    poly_variant_names = List.rev pvn;
    classic_variant_names = List.rev cvn;
  }

let check_duplicate_names container_kind field_kind l =
  let tbl = Hashtbl.create 200 in
  List.iter (
    fun (root_loc, loc, s) ->
      try
        let orig_loc = Hashtbl.find tbl s in
        let msg1 =
          sprintf "\
%s contains a %s that is already defined elsewhere
and cannot be reused."
            (String.capitalize_ascii container_kind) field_kind
        in
        let msg2 = sprintf "First definition of %s %s." field_kind s in
        let msg3 = sprintf "\
Impossible second definition of %s %s.

Use a different name, possibly by placing <ocaml name=\"NAME\">
after the field name or variant name in the ATD type definition.
<ocaml field_prefix=\"PREFIX\"> can also be used after a whole record."
            field_kind s
        in
        if loc <> orig_loc then
          Error.error3
            root_loc msg1
            orig_loc msg2
            loc msg3
        else
          Error.error2
            root_loc msg1
            orig_loc msg2

      with Not_found ->
        Hashtbl.add tbl s loc
  ) l

let check_names x =
  check_duplicate_names "record type" "field name"
    (List.flatten x.field_names);
  check_duplicate_names "variant type" "constructor name"
    (List.flatten x.classic_variant_names)


let check grouped_defs =
  let x = extract_ocaml_names_from_defs (List.concat_map snd grouped_defs) in
  check_names x


let get_full_type_name x =
  let s = x.def_name in
  match x.def_param with
    [] -> s
  | [x] -> sprintf "'%s %s" x s
  | l ->
      let l = List.map (fun s -> "'" ^ s) l in
      sprintf "(%s) %s" (String.concat ", " l) s

let anon_param_type_name s n_param =
  match n_param with
  | 0 -> s
  | 1 -> "_ " ^ s
  | n ->
      let underscores = Array.make n "_" in
      let params = String.concat ", " (Array.to_list underscores) in
      "(" ^ params ^ ") " ^ s

(* Get a type expression that uses the original user-given name (e.g. not _1) *)
let get_type_constraint ~original_types def =
  try
    let (poly_name, n_params) = Hashtbl.find original_types def.def_name in
    anon_param_type_name poly_name n_params
  with Not_found ->
    get_full_type_name def


(* Classic variants and records need type annotations in order to allow
   constructor/field name disambiguation *)
let needs_type_annot (x : _ expr) =
  match x with
  | Record (_, _, Record Record, _)
  | Sum (_, _, Sum Classic, _) -> true
  | _ -> false

let insert_annot type_annot =
  match type_annot with
  | None -> ""
  | Some t -> sprintf " : %s" t

(* Add an optional type annotation on an OCaml expression or pattern *)
let opt_annot type_annot expr =
  match type_annot with
  | None -> expr
  | Some t -> sprintf "(%s : %s)" expr t

(* Add an optional type annotation after all function parameters
   in a let binding (last thing before the equal sign) *)
let opt_annot_def type_annot fun_param =
  match type_annot with
  | None -> fun_param
  | Some t -> sprintf "%s : %s" fun_param t


let write_file file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let write_ocaml out mli ml =
  match out with
    Stdout ->
      printf "\
struct
%s
end :
sig
%s
end
"
        ml mli;
      flush stdout

  | Files prefix ->
      write_file (prefix ^ ".mli") mli;
      write_file (prefix ^ ".ml") ml

let is_exportable def =
  let s = def.def_name in
  s <> "" && s.[0] <> '_'
  && def.def_value <> None

let make_record_creator deref x =
  match x.def_value with
    Some (Record (_, a, Ocaml.Repr.Record Ocaml.Record, _)) ->
      let s = x.def_name in
      let full_name = get_full_type_name x in
      let (intf_params, impl_params, impl_fields) =
        Array.map (Ocaml.map_record_creator_field deref) a
        |> Array.to_list
        |> List.map (fun { Ocaml. intf_params; impl_params; impl_fields } ->
          (intf_params, impl_params, impl_fields))
        |> List.split3
      in
      let intf =
        sprintf "\
val create_%s :%s
  unit -> %s
  (** Create a record of type {!%s}. *)

"
          s (String.concat "" intf_params)
          full_name
          s
      in
      let impl =
        sprintf "\
let create_%s %s
  () : %s =
  {%s
  }
"
          s (String.concat "" impl_params) full_name
          (String.concat "" impl_fields)
      in
      intf, impl

  | _ -> "", ""

let rec is_lambda (l : Indent.t list) =
  match l with
    [] -> false
  | x :: _ ->
      match x with
        Line _ -> false
      | Block l -> is_lambda l
      | Inline l -> is_lambda l
      | Annot ("fun", _) -> true
      | Annot (_, x) -> is_lambda [x]

let is_function = is_lambda

let name_of_var s = "_" ^ s

let nth name i len =
  Array.init len (fun j -> if i = j then name else "_")
  |> Array.to_list
  |> String.concat ", "

let get_let ~is_rec ~is_first =
  if is_first then
    if is_rec then "let rec", "and"
    else "let", "let"
  else "and", "and"

let write_opens buf l =
  List.iter (fun s -> bprintf buf "open %s\n" s) l;
  bprintf buf "\n"

let def_of_atd (loc, (name, param, an), x) ~target ~def ~external_
    ~mapping_of_expr =
  let ocaml_predef = Ocaml.get_ocaml_predef target an in
  let doc = Atd.Doc.get_doc loc an in
  let o =
    match as_abstract x with
      Some (_, _) ->
        Ocaml.get_ocaml_module_and_t target name an
        |> Option.map (fun (types_module, main_module, ext_name) ->
          let args = List.map (fun s -> Tvar (loc, s)) param in
          External
            (loc, name, args,
             Ocaml.Repr.External (types_module, main_module, ext_name),
             external_))
    | None -> Some (mapping_of_expr x)
  in
  {
    def_loc = loc;
    def_name = name;
    def_param = param;
    def_value = o;
    def_arepr =
      Ocaml.Repr.Def { Ocaml.ocaml_predef = ocaml_predef;
                       ocaml_ddoc = doc };
    def_brepr = def;
  }

let maybe_write_creator_impl ~with_create deref buf defs =
  if with_create then
    List.iter (
      fun (_, l) ->
        let l = List.filter is_exportable l in
        List.iter (
          fun x ->
            let _, impl = make_record_creator deref x in
            Buffer.add_string buf impl
        ) l
    ) defs

let maybe_write_creator_intf ~with_create deref buf x =
  if with_create && is_exportable x then (
    let create_record_intf, _ = make_record_creator deref x in
    bprintf buf "%s" create_record_intf;
    bprintf buf "\n"
  )

let default_value x deref =
  let ocamlf =
    match x.f_arepr with
    | Ocaml.Repr.Field o -> o
    | _ -> failwith "Ox_emit.default_value" in
  match x.f_kind, ocamlf.Ocaml.ocaml_default with
  | With_default, None ->
      begin match Ocaml.get_implicit_ocaml_default (deref x.f_value) with
        | None -> Error.error x.f_loc "Missing default field value"
        | Some d -> Some d
      end
  | With_default, Some d -> Some d
  | Optional, _ -> Some "None"
  | Required, _ -> None

let include_intf (x : (Ocaml.Repr.t, _) Mapping.def) =
  x.def_name <> "" && x.def_name.[0] <> '_' && x.def_value <> None

type field =
  { mapping : (Ocaml.Repr.t, Json.json_repr) Mapping.field_mapping
  ; ocaml_fname : string
  ; json_fname : string
  ; ocaml_default : string option
  ; optional : bool
  ; unwrapped : bool
  }

let get_fields deref a =
  List.map (fun x ->
    let ocamlf, jsonf =
      match x.f_arepr, x.f_brepr with
      | Ocaml.Repr.Field o, Json.Field j -> o, j
      | _ -> assert false
    in
    let ocaml_fname = ocamlf.Ocaml.ocaml_fname in
    let ocaml_default =
      match x.f_kind, ocamlf.Ocaml.ocaml_default with
      | With_default, None ->
          (match Ocaml.get_implicit_ocaml_default (deref x.f_value) with
           | None -> Error.error x.f_loc "Missing default field value"
           | Some d -> Some d
          )
      | With_default, Some d -> Some d
      | Optional, _ -> Some "None"
      | Required, _ -> None
    in
    let json_fname = jsonf.Json.json_fname in
    let optional = not (Atd.Ast.is_required x.f_kind) in
    let unwrapped = jsonf.Json.json_unwrapped in
    { mapping = x
    ; ocaml_fname
    ; ocaml_default
    ; json_fname
    ; optional
    ; unwrapped
    }
  ) (Array.to_list a)

let is_string deref x =
  (*
    Calling 'unwrap' allows us to ignore 'wrap' constructors
    and determine that the JSON representation is a string.
    This assumes that no '<json>' annotation imposes
    another representation for the JSON string.
  *)
  match Mapping.unwrap deref x with
  | String _ -> true
  | _ -> false (* or maybe we just don't know *)

let get_assoc_type deref loc x =
  match deref x with
  | Tuple (_, [| k; v |], Ocaml.Repr.Tuple, Json.Tuple) ->
      if not (is_string deref k.cel_value) then
        Error.error loc "Due to <json repr=\"object\"> keys must be strings";
      (k.cel_value, v.cel_value)
  | _ ->
      Error.error loc "Expected due to <json repr=\"object\">: (string * _) list"
