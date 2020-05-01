
(*
  Translation from ATD types into OCaml types and pretty-printing.

  This is derived from the ATD pretty-printer (atd_print.ml).
*)

open Atd.Import

open Easy_format
open Atd.Ast
open Mapping

type pp_convs =
  | Camlp4 of string list
  | Ppx of string list

(* Type mapping from ATD to OCaml *)

type atd_ocaml_sum = Classic | Poly
type atd_ocaml_record = Record | Object

type atd_ocaml_int = Int | Char | Int32 | Int64 | Float
type atd_ocaml_list = List | Array

type atd_ocaml_wrap = {
  ocaml_wrap_t : string;
  ocaml_wrap : string;
  ocaml_unwrap : string;
}

type atd_ocaml_field = {
  ocaml_default : string option;
  ocaml_fname : string;
  ocaml_mutable : bool;
  ocaml_fdoc : Atd.Doc.doc option;
}

type atd_ocaml_variant = {
  ocaml_cons : string;
  ocaml_vdoc : Atd.Doc.doc option;
}

type atd_ocaml_def = {
  ocaml_predef : bool;
  ocaml_ddoc : Atd.Doc.doc option;
}

let tick = function
  | Poly -> "`"
  | Classic -> ""

let dot = function
  | Record -> "."
  | Object -> "#"

module Repr = struct
  type t =
    | Unit
    | Bool
    | Int of atd_ocaml_int
    | Float
    | String
    | Sum of atd_ocaml_sum
    | Record of atd_ocaml_record
    | Tuple
    | List of atd_ocaml_list
    | Option
    | Nullable
    | Wrap of atd_ocaml_wrap option
    | Name of string
    | External of (string * string * string)
        (*
          (module providing the type,
           module providing everything else,
           type name)
        *)

    | Cell of atd_ocaml_field
    | Field of atd_ocaml_field
    | Variant of atd_ocaml_variant
    | Def of atd_ocaml_def
end

type target = Default | Biniou | Json | Validate | Bucklescript


let ocaml_int_of_string s : atd_ocaml_int option =
  match s with
      "int" -> Some Int
    | "char" -> Some Char
    | "int32" -> Some Int32
    | "int64" -> Some Int64
    | "float" -> Some Float
    | _ -> None

let string_of_ocaml_int (x : atd_ocaml_int) =
  match x with
      Int -> "int"
    | Char -> "Char.t"
    | Int32 -> "Int32.t"
    | Int64 -> "Int64.t"
    | Float -> "float"

let ocaml_sum_of_string s : atd_ocaml_sum option =
  match s with
      "classic" -> Some Classic
    | "poly" -> Some Poly
    | _ -> None

let ocaml_record_of_string s : atd_ocaml_record option =
  match s with
      "record" -> Some Record
    | "object" -> Some Object
    | _ -> None

let ocaml_list_of_string s : atd_ocaml_list option =
  match s with
      "list" -> Some List
    | "array" -> Some Array
    | _ -> None

let string_of_ocaml_list (x : atd_ocaml_list) =
  match x with
      List -> "list"
    | Array -> "Atdgen_runtime.Util.ocaml_array"

let path_of_target (target : target) =
  match target with
    | Default -> [ "ocaml" ]
    | Biniou -> [ "ocaml_biniou"; "ocaml" ]
    | Json -> [ "ocaml_json"; "ocaml" ]
    | Bucklescript -> ["ocaml_bs"; "ocaml"]
    | Validate -> [ "ocaml_validate"; "ocaml" ]

let get_ocaml_int target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:ocaml_int_of_string
    ~default:Int
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_type_path target atd_name an =
  let x =
    match atd_name with
        "unit" -> `Unit
      | "bool" -> `Bool
      | "int" -> `Int (get_ocaml_int target an)
      | "float" -> `Float
      | "string" -> `String
      | s -> `Name s
  in
  match x with
      `Unit -> "unit"
    | `Bool -> "bool"
    | `Int x -> string_of_ocaml_int x
    | `Float -> "float"
    | `String -> "string"
    | `Name s -> s

let get_ocaml_sum target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:ocaml_sum_of_string
    ~default:Poly
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_field_prefix target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:""
    ~sections:path
    ~field:"field_prefix"
    an

let get_ocaml_record target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:ocaml_record_of_string
    ~default:Record
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_list target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:ocaml_list_of_string
    ~default:List
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_wrap ~type_param target loc an =
  let path = path_of_target target in
  let module_ =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:path
      ~field:"module"
      an
  in
  let default field =
    Option.map (fun s ->
      sprintf "%s.%s" s field) module_
  in
  let default_t field =
    Option.map (fun s ->
      let type_param =
        match List.map (sprintf "'%s") type_param with
        | [] -> ""
        | x::[] -> sprintf "%s " x
        | param -> sprintf "(%s) " (String.concat ", " type_param) in
      sprintf "%s%s.%s" type_param s field) module_
  in
  let t =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default_t "t")
      ~sections:path
      ~field:"t"
      an
  in
  let wrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "wrap")
      ~sections:path
      ~field:"wrap"
      an
  in
  let unwrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "unwrap")
      ~sections:path
      ~field:"unwrap"
      an
  in
  match t, wrap, unwrap with
      None, None, None -> None
    | Some t, Some wrap, Some unwrap ->
        Some { ocaml_wrap_t = t; ocaml_wrap = wrap; ocaml_unwrap = unwrap }
    | _ ->
        Error.error loc "Incomplete annotation. Missing t, wrap or unwrap"

let get_ocaml_cons target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:path
    ~field:"name"
    an

let get_ocaml_fname target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default
    ~sections:path
    ~field:"name"
    an

let get_ocaml_default target an =
  let path = path_of_target target in
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:path
    ~field:"default"
    an

let get_ocaml_mutable target an =
  let path = path_of_target target in
  Atd.Annot.get_flag
    ~sections:path
    ~field:"mutable"
    an

let get_ocaml_predef target an =
  let path = path_of_target target in
  Atd.Annot.get_flag
    ~sections:path
    ~field:"predef"
    an

let get_ocaml_module target an =
  let path = path_of_target target in
  let o =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:path
      ~field:"module"
      an
  in
  match o with
    Some s -> Some (s, s)
  | None ->
      Atd.Annot.get_opt_field
        ~parse:(fun s -> Some s)
        ~sections:path
        ~field:"from" an
      |> Option.map (fun s ->
        let type_module = s ^ "_t" in
        let main_module =
          match target with
          | Default -> type_module
          | Biniou -> s ^ "_b"
          | Json -> s ^ "_j"
          | Bucklescript -> s ^ "_bs"
          | Validate -> s ^ "_v"
        in
        (type_module, main_module))

let get_ocaml_t target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default
    ~sections:path
    ~field:"t"
    an

let get_ocaml_module_and_t target default_name an =
  get_ocaml_module target an
  |> Option.map (fun (type_module, main_module) ->
  (type_module, main_module, get_ocaml_t target default_name an))


(*
  OCaml syntax tree
*)
type ocaml_type_param = string list

type ocaml_expr =
    [ `Sum of (atd_ocaml_sum * ocaml_variant list)
    | `Record of (atd_ocaml_record * ocaml_field list)
    | `Tuple of ocaml_expr list
    | `Name of (string * ocaml_expr list)
    | `Tvar of string
    ]

and ocaml_variant =
    string * ocaml_expr option * Atd.Doc.doc option

and ocaml_field =
    (string * bool (* is mutable? *)) * ocaml_expr * Atd.Doc.doc option

type ocaml_def = {
  o_def_name : (string * ocaml_type_param);
  o_def_alias : (string * ocaml_type_param) option;
  o_def_expr : ocaml_expr option;
  o_def_doc : Atd.Doc.doc option
}

type ocaml_module_item =
    [ `Type of ocaml_def ]

type ocaml_module_body = ocaml_module_item list



(*
  Mapping from ATD to OCaml
*)

let rec map_expr target
    (type_param: type_param) (x : type_expr) : ocaml_expr =
  match x with
    Atd.Ast.Sum (_, l, an) ->
      let kind = get_ocaml_sum target an in
      `Sum (kind, List.map (map_variant target) l)
  | Record (loc, l, an) ->
      let kind = get_ocaml_record target an in
      let field_prefix = get_ocaml_field_prefix target an in
      if l = [] then
        Error.error loc "Empty record (not valid in OCaml)"
      else
        `Record (kind, List.map (map_field target field_prefix) l)
  | Tuple (_, l, _) ->
      `Tuple (List.map (fun (_, x, _) -> (map_expr target []) x) l)
  | List (_, x, an) ->
      let s = string_of_ocaml_list (get_ocaml_list target an) in
      `Name (s, [map_expr target [] x])
  | Option (_, x, _) ->
      `Name ("option", [map_expr target [] x])
  | Nullable (_, x, _) ->
      `Name ("option", [map_expr target [] x])
  | Shared (_, _, _) ->
      failwith "Sharing is not supported"
  | Wrap (loc, x, a) ->
      (match get_ocaml_wrap ~type_param target loc a with
         None -> map_expr target [] x
       | Some { ocaml_wrap_t ; _ } -> `Name (ocaml_wrap_t, [])
      )
  | Name (_, (_2, s, l), an) ->
      let s = get_ocaml_type_path target s an in
      `Name (s, List.map (map_expr target []) l)
  | Tvar (_, s) ->
      `Tvar s

and map_variant target (x : variant) : ocaml_variant =
  match x with
    Inherit _ -> assert false
  | Variant (loc, (s, an), o) ->
      let s = get_ocaml_cons target s an in
      (s, Option.map (map_expr target []) o, Atd.Doc.get_doc loc an)

and map_field target ocaml_field_prefix (x : field) : ocaml_field =
  match x with
    `Inherit _ -> assert false
  | `Field (loc, (atd_fname, _, an), x) ->
      let ocaml_fname =
        get_ocaml_fname target (ocaml_field_prefix ^ atd_fname) an in
      let fname =
        if ocaml_fname = atd_fname then ocaml_fname
        else sprintf "%s (*atd %s *)" ocaml_fname atd_fname
      in
      let is_mutable = get_ocaml_mutable target an in
      ((fname, is_mutable), map_expr target [] x, Atd.Doc.get_doc loc an)

let map_def
    ~(target : target)
    ~(type_aliases : string option)
    ((loc, (s, param, an1), x) : type_def) : ocaml_def option =
  let is_predef = get_ocaml_predef target an1 in
  let is_abstract = Mapping.is_abstract x in
  let define_alias =
    if is_predef || is_abstract || type_aliases <> None then
      match get_ocaml_module_and_t target s an1, type_aliases with
          Some (types_module, _, s), _ -> Some (types_module, s)
        | None, Some types_module -> Some (types_module, s)

        | None, None -> None
    else
      None
  in
  if is_predef && define_alias = None then
    None
  else
    let an2 = Atd.Ast.annot_of_type_expr x in
    let an = an1 @ an2 in
    let doc = Atd.Doc.get_doc loc an in
    let alias, x =
      match define_alias with
          None ->
            if is_abstract then (None, None)
            else (None, Some (map_expr target param x))
        | Some (module_path, ext_name) ->
            let alias = Some (module_path ^ "." ^ ext_name, param) in
            let x =
              match map_expr target param x with
                  `Sum (Classic, _)
                | `Record (Record, _) as x -> Some x
                | _ -> None
            in
            (alias, x)
    in
    if x = None && alias = None then
      None
    else
      Some {
        o_def_name = (s, param);
        o_def_alias = alias;
        o_def_expr = x;
        o_def_doc = doc
      }


let map_module ~target ~type_aliases (l : module_body) : ocaml_module_body =
  List.filter_map (
    fun (Atd.Ast.Type td) ->
      Option.map (fun x -> `Type x) (map_def ~target ~type_aliases td)
  ) l


(*
  Mapping from Mapping to OCaml
*)


let rec ocaml_of_expr_mapping (x : (Repr.t, _) mapping) : ocaml_expr =
  match x with
    Unit (_, Unit, _) -> `Name ("unit", [])
  | Bool (_, Bool, _) -> `Name ("bool", [])
  | Int (_, Int x, _) -> `Name (string_of_ocaml_int x, [])
  | Float (_, Float, _) -> `Name ("float", [])
  | String (_, String, _) -> `Name ("string", [])
  | Sum (_, a, Sum kind, _) ->
      let l = Array.to_list a in
      `Sum (kind, List.map ocaml_of_variant_mapping l)
  | Record (_, a, Record _, _) ->
      let l = Array.to_list a in
      `Record (Record, List.map ocaml_of_field_mapping l)
  | Tuple (_, a, _, _) ->
      let l = Array.to_list a in
      `Tuple (List.map (fun x -> ocaml_of_expr_mapping x.cel_value) l)
  | List (_, x, List kind, _) ->
      `Name (string_of_ocaml_list kind, [ocaml_of_expr_mapping x])
  | Option (_, x, Option, _) ->
      `Name ("option", [ocaml_of_expr_mapping x])
  | Nullable (_, x, Nullable, _) ->
      `Name ("option", [ocaml_of_expr_mapping x])
  | Wrap _ ->
      assert false
  | Name (_, s, l, _, _) ->
      `Name (s, List.map ocaml_of_expr_mapping l)
  | Tvar (_, s) ->
      `Tvar s
  | _ -> assert false

and ocaml_of_variant_mapping x =
  let o =
    match x.var_arepr with
        Variant o -> o
      | _ -> assert false
  in
  (o.ocaml_cons, Option.map ocaml_of_expr_mapping x.var_arg, o.ocaml_vdoc)

and ocaml_of_field_mapping x =
  let o =
    match x.f_arepr with
        Field o -> o
      | _ -> assert false
  in
  let v = ocaml_of_expr_mapping x.f_value in
  ((o.ocaml_fname, o.ocaml_mutable), v, o.ocaml_fdoc)


(*
  Pretty-printing
*)



let rlist = { list with
                wrap_body = `Force_breaks;
                indent_body = 0;
                align_closing = false;
                space_after_opening = false;
                space_before_closing = false
            }

let plist = { list with
                align_closing = false;
                space_after_opening = false;
                space_before_closing = false }

let hlist = { list with wrap_body = `No_breaks }
let shlist = { hlist with
                 stick_to_label = false;
                 space_after_opening = false;
                 space_before_closing = false }

let llist = {
  list with
    separators_stick_left = false;
    space_before_separator = true;
    space_after_separator = true
}

let lplist = {
  llist with
    space_after_opening = false;
    space_before_closing = false
}

let vlist1 = { list with stick_to_label = false }

let vlist = {
  vlist1 with
    wrap_body = `Force_breaks;
}


let make_atom s = Atom (s, atom)

let horizontal_sequence l = Easy_format.List (("", "", "", shlist), l)

let vertical_sequence ?(skip_lines = 0) l =
  let l =
    if skip_lines = 0 then l
    else
      let sep = List.init skip_lines (fun _ -> (Atom ("", atom))) in
      List.insert_sep l ~sep
  in
  Easy_format.List (("", "", "", rlist), l)

let escape f s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match f c with
        None -> Buffer.add_char buf c
      | Some s -> Buffer.add_string buf s
  done;
  Buffer.contents buf

let ocamldoc_escape s =
  let esc = function
      '{' | '}' | '[' | ']' | '@' | '\\' as c -> Some (sprintf "\\%c" c)
    | _ -> None
  in
  escape esc s

let ocamldoc_verbatim_escape s =
  let esc = function
      '{' | '}' | '\\' as c -> Some (sprintf "\\%c" c)
    | _ -> None
  in
  escape esc s

let split = Re.Str.split (Re.Str.regexp " ")


let make_ocamldoc_block = function
  | Atd.Doc.Pre s -> Atom ("\n{v\n" ^ ocamldoc_verbatim_escape s ^ "\nv}", atom)
  | Paragraph l ->
      let l = List.map (function
        | Atd.Doc.Text s -> ocamldoc_escape s
        | Code s -> "[" ^ ocamldoc_escape s ^ "]"
      ) l
      in
      let words = split (String.concat "" l) in
      let atoms = List.map (fun s -> Atom (s, atom)) words in
      List (("", "", "", plist), atoms)

let rec make_ocamldoc_blocks = function
  | []
  | [_] as l -> List.map make_ocamldoc_block l
  | x :: (y :: _ as xs) ->
      let rest = make_ocamldoc_blocks xs in
      let rest =
        match y with
        | Atd.Doc.Paragraph _ -> Atom ("", atom) :: rest
        | Pre _ -> rest in
      make_ocamldoc_block x :: rest

let make_ocamldoc_comment l =
  let blocks = make_ocamldoc_blocks l in
  let xlist =
    match l with
      [] | [_] -> vlist1
    | _ -> vlist
  in
  Easy_format.List (("(**", "", "*)", xlist), blocks)

let prepend_ocamldoc_comment doc x =
  match doc with
      None -> x
    | Some y ->
        let comment = make_ocamldoc_comment y in
        Easy_format.List (("", "", "", rlist), [comment;x])

let append_ocamldoc_comment x doc =
  match doc with
      None -> x
    | Some y ->
        let comment = make_ocamldoc_comment y in
        Label ((x, label), comment)

let format_pp_conv_node node = function
  | Camlp4 []
  | Ppx [] -> node
  | converters ->
    let converters =
      match converters with
      | Ppx cs -> "[@@deriving " ^ (String.concat ", " cs) ^ "]"
      | Camlp4 cs -> "with " ^ (String.concat ", " cs) in
    Label ((node, label), make_atom converters)

let rec format_module_item pp_convs
    is_first (`Type def : ocaml_module_item) =
  let type_ = if is_first then "type" else "and" in
  let s, param = def.o_def_name in
  let alias = def.o_def_alias in
  let expr = def.o_def_expr in
  let doc = def.o_def_doc in
  let append_if b s1 s2 =
    if b then s1 ^ s2
    else s1
  in
  let part1 =
    horizontal_sequence (
      make_atom type_ ::
        prepend_type_param param
        [ make_atom (append_if (alias <> None || expr <> None) s " =") ]
    )
  in
  let part12 =
    match alias with
        None -> part1
      | Some (name, param) ->
          let right =
            horizontal_sequence (
              prepend_type_param param
                [ make_atom (append_if (expr <> None) name " =") ]
            )
          in
          Label (
            (part1, label),
            right
          )
  in
  let part123 =
    match expr with
        None -> part12

      | Some t ->
          Label (
            (part12, label),
            format_type_expr t
          )
  in
  format_pp_conv_node (prepend_ocamldoc_comment doc part123) pp_convs


and prepend_type_param l tl =
  match l with
      [] -> tl
    | _ ->
        let make_var s = make_atom ("'" ^ s) in
        let x =
          match l with
              [s] -> make_var s
            | l -> List (("(", ",", ")", plist), List.map make_var l)
        in
        x :: tl

and prepend_type_args l tl =
  match l with
      [] -> tl
    | _ ->
        let x =
          match l with
              [t] -> format_type_expr t
            | l -> List (("(", ",", ")", plist), List.map format_type_expr l)
        in
        x :: tl

and format_type_expr x =
  match x with
      `Sum (kind, l) ->
        let op, cl =
          match kind with
              Classic -> "", ""
            | Poly -> "[", "]"
        in
        List (
            (op, "|", cl, llist),
            List.map (format_variant kind) l
          )
    | `Record (kind, l) ->
        let op, cl =
          match kind with
              Record -> "{", "}"
            | Object -> "<", ">"
        in
        List (
          (op, ";", cl, list),
          List.map format_field l
        )
    | `Tuple l ->
        List (
          ("(", "*", ")", lplist),
          List.map format_type_expr l
        )
    | `Name (name, args) ->
        format_type_name name args

    | `Tvar name ->
        make_atom ("'" ^ name)

and format_type_name name args =
  horizontal_sequence (prepend_type_args args [ make_atom name ])

and format_field ((s, is_mutable), t, doc) =
  let l =
    let l = [make_atom (s ^ ":")] in
    if is_mutable then
      make_atom "mutable" :: l
    else l
  in
  let field =
    Label (
      (horizontal_sequence l, label),
      format_type_expr t
    )
  in
  append_ocamldoc_comment field doc

and format_variant kind (s, o, doc) =
  let s = tick kind ^ s in
  let cons = make_atom s in
  let variant =
    match o with
        None -> cons
      | Some t ->
          Label (
            (cons, label),
            Label (
              (make_atom "of", label),
              format_type_expr t
            )
          )
  in
  append_ocamldoc_comment variant doc

let format_module_items pp_convs (l : ocaml_module_body) =
  match l with
      x :: l ->
        format_module_item pp_convs true x ::
          List.map (fun x -> format_module_item pp_convs false x) l
    | [] -> []

let format_module_bodies pp_conv (l : (bool * ocaml_module_body) list) =
  List.concat_map (fun (_, x) -> format_module_items pp_conv x) l

let format_head (loc, an) =
  match Atd.Doc.get_doc loc an with
      None -> []
    | Some doc -> [make_ocamldoc_comment doc]

let format_all l =
  vertical_sequence ~skip_lines:1 l


let ocaml_of_expr x : string =
  Easy_format.Pretty.to_string (format_type_expr x)

let ocaml_of_atd ?(pp_convs=Ppx []) ~target ~type_aliases
    (head, (l : (bool * module_body) list)) : string =
  let head = format_head head in
  let bodies =
    List.map (fun (is_rec, m) ->
                (is_rec, map_module ~target ~type_aliases m)) l
  in
  let body = format_module_bodies pp_convs bodies in
  let x = format_all (head @ body) in
  Easy_format.Pretty.to_string x

let unwrap_option = function
  | Option (_, x, _, _)
  | Nullable (_, x, _, _) -> x
  | Name (loc, s, _, _, _) ->
      Error.error loc ("Not an option type: " ^ s)
  | x ->
      Error.error (loc_of_mapping x) "Not an option type"


let get_implicit_ocaml_default = function
  | Unit (_, Repr.Unit, _) -> Some "()"
  | Bool (_, Bool, _) -> Some "false"
  | Int (_, Int o, _) ->
      Some (match o with
          Int -> "0"
        | Char -> "'\000'"
        | Int32 -> "0l"
        | Int64 -> "0L"
        | Float -> "0.")
  | Float (_, Float, _) -> Some "0.0"
  | String (_, String, _) -> Some "\"\""
  | List (_, _, List List, _) -> Some "[]"
  | List (_, _, List Array, _) -> Some "[||]"
  | Option (_, _, Option, _) -> Some "None"
  | Nullable (_, _, Nullable, _) -> Some "None"
  | _ -> None

type create_fields =
  { intf_params: string
  ; impl_params: string
  ; impl_fields: string
  }

let map_record_creator_field deref x =
  let o =
    match x.f_arepr with
        Repr.Field o -> o
      | _ -> assert false
  in
  let fname = o.ocaml_fname in
  let impl2 = sprintf "\n    %s = %s;" fname fname in
  match x.f_kind with
      Required ->
        let t = ocaml_of_expr (ocaml_of_expr_mapping x.f_value) in
        let intf = sprintf "\n  %s: %s ->" fname t in
        let impl1 = sprintf "\n  ~%s" fname in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

    | Optional ->
        let x = unwrap_option (deref x.f_value) in
        let t = ocaml_of_expr (ocaml_of_expr_mapping x) in
        let intf = sprintf "\n  ?%s: %s ->" fname t in
        let impl1 = sprintf "\n  ?%s" fname in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

    | With_default ->
        let t = ocaml_of_expr (ocaml_of_expr_mapping x.f_value) in
        let intf = sprintf "\n  ?%s: %s ->" fname t in
        let impl1 =
          let default =
            match o.ocaml_default with
                None ->
                  (match get_implicit_ocaml_default (deref x.f_value) with
                       None ->
                         Error.error x.f_loc "Missing default field value"
                     | Some s -> s
                  )
              | Some s -> s
          in
          sprintf "\n  ?(%s = %s)" fname default
        in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

let obj_unimplemented loc = function
   | Record -> ()
   | Object -> Error.error loc "Sorry, OCaml objects are not supported"
