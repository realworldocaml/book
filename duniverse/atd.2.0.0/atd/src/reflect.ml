(*
  Conversion of an ATD tree to OCaml source code for that value.
*)

open Import

let print_loc buf (_, _) =
  bprintf buf "loc"

let print_list f buf l =
  bprintf buf "[";
  List.iter (fun x -> bprintf buf "%a;\n" f x) l;
  bprintf buf "]"

let print_opt f buf o =
  match o with
      None -> bprintf buf "None"
    | Some x -> bprintf buf "Some (%a)" f x

let print_qstring buf s = bprintf buf "%S" s

let print_prop_list buf l =
  print_list (
    fun buf (s, (loc, o)) ->
      bprintf buf "(%S, (%a, %a))"
        s print_loc loc (print_opt print_qstring) o
  )
    buf l

let print_annot_list buf l =
  print_list (
    fun buf (s, (loc, l)) ->
      bprintf buf "(%S, (%a, %a))" s print_loc loc print_prop_list l
  )
    buf l

let rec print_type_expr buf (x : Ast.type_expr) =
  match x with
  | Sum (loc, variant_list, annot_list) ->
      bprintf buf "`Sum (%a, %a, %a)"
        print_loc loc
        (print_list print_variant) variant_list
        print_annot_list annot_list
  | Record (loc, field_list, annot_list) ->
      bprintf buf "`Record (%a, %a, %a)"
        print_loc loc
        (print_list print_field) field_list
        print_annot_list annot_list
  | Tuple (loc, cell_list, annot_list) ->
      bprintf buf "`Tuple (%a, %a, %a)"
        print_loc loc
        (print_list print_cell) cell_list
        print_annot_list annot_list
  | List (loc, type_expr, annot_list) ->
      bprintf buf "`List (%a, %a, %a)"
        print_loc loc
        print_type_expr type_expr
        print_annot_list annot_list
  | Option (loc, type_expr, annot_list) ->
      bprintf buf "`Option (%a, %a, %a)"
        print_loc loc
        print_type_expr type_expr
        print_annot_list annot_list
  | Nullable (loc, type_expr, annot_list) ->
      bprintf buf "`Nullable (%a, %a, %a)"
        print_loc loc
        print_type_expr type_expr
        print_annot_list annot_list
  | Shared (loc, type_expr, annot_list) ->
      bprintf buf "`Shared (%a, %a, %a)"
        print_loc loc
        print_type_expr type_expr
        print_annot_list annot_list
  | Wrap (loc, type_expr, annot_list) ->
      bprintf buf "`Wrap (%a, %a, %a)"
        print_loc loc
        print_type_expr type_expr
        print_annot_list annot_list
  | Name (loc, type_inst, annot_list) ->
      bprintf buf "`Name (%a, %a, %a)"
        print_loc loc
        print_type_inst type_inst
        print_annot_list annot_list
  | Tvar (loc, string) ->
      bprintf buf "`Tvar (%a, %S)"
        print_loc loc
        string

and print_cell buf (loc, x, a) =
  bprintf buf "(%a, %a, %a)"
    print_loc loc
    print_type_expr x
    print_annot_list a

and print_variant buf x =
  match x with
    Variant (loc, (s, a), o) ->
      bprintf buf "`Variant (%a, (%S, %a), %a)"
        print_loc loc
        s print_annot_list a
        (print_opt print_type_expr) o
  | Inherit (loc, x) ->
      bprintf buf "`Inherit (%a, %a)"
        print_loc loc
        print_type_expr x

and print_field buf x =
  match x with
      `Field (loc, (s, kind, a), x) ->
        bprintf buf "`Field (%a, (%S, %a, %a), %a)"
          print_loc loc
          s print_field_kind kind print_annot_list a
          print_type_expr x
    | `Inherit (loc, x) ->
        bprintf buf "`Inherit (%a, %a)"
          print_loc loc
          print_type_expr x

and print_field_kind buf fk =
  Buffer.add_string buf
    (match fk with
       Required -> "`Required"
     | Optional -> "`Optional"
     | With_default -> "`With_default")

and print_type_inst buf (loc, s, l) =
  bprintf buf "(%a, %S, %a)"
    print_loc loc
    s
    (print_list print_type_expr) l


let print_module_item buf (Ast.Type (loc, (name, param, a), x)) =
  bprintf buf "`Type (%a, (%S, %a, %a), %a)"
    print_loc loc
    name (print_list print_qstring) param print_annot_list a
    print_type_expr x

let print_module_body buf l =
  bprintf buf "[\n";
  List.iter (fun x ->
               print_module_item buf x;
               bprintf buf ";\n"
            ) l;
  bprintf buf "]\n"

let print_module_body_def buf name l =
  bprintf buf "\
let %s_body : Ast.module_body =
  let loc = Ast.dummy_loc in
%a

let %s = %s_body (* for backward compatibility with atd <= 1.0.1 *)
"
    name print_module_body l
    name name

let print_module_head_def buf name an =
  bprintf buf "\
let %s_head : Ast.module_head =
  let loc = Ast.dummy_loc in
  (loc, %a)
"
    name print_annot_list an

let print_full_module_def buf name ((_, an), l) =
  print_module_head_def buf name an;
  print_module_body_def buf name l;
  bprintf buf "\
let %s_full : Ast.full_module =
  (%s_head, %s_body)
"
    name name name
