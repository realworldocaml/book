open Import
open Lexing


type loc = Lexing.position * Lexing.position

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Atd_error of string

type full_module = module_head * module_body

and module_head = loc * annot

and module_body = module_item list

and annot = annot_section list

and annot_section = string * (loc * annot_field list)

and annot_field = string * (loc * string option)

and type_def = loc * (string * type_param * annot) * type_expr

and module_item =
  | Type of type_def

and type_param = string list

and type_expr =
  | Sum of loc * variant list * annot
  | Record of loc * field list * annot
  | Tuple of loc * cell list * annot
  | List of loc * type_expr * annot
  | Option of loc * type_expr * annot
  | Nullable of loc * type_expr * annot
  | Shared of loc * type_expr * annot
  | Wrap of loc * type_expr * annot
  | Name of loc * type_inst * annot
  | Tvar of loc * string
  (* List, Option, Nullable, Shared and Wrap are
     the only predefined types with a type
     parameter (and no special syntax). *)

and type_inst = loc * string * type_expr list

and variant =
  | Variant of loc * (string * annot) * type_expr option
  | Inherit of loc * type_expr

and cell = loc * type_expr * annot

and field_kind =
  | Required
  | Optional
  | With_default

and simple_field = (loc * (string * field_kind * annot) * type_expr)

and field =
  [ `Field of simple_field
  | `Inherit of (loc * type_expr) ]

type any =
  | Full_module of full_module
  | Module_head of module_head
  | Module_body of module_body
  | Module_item of module_item
  | Type_def of type_def
  | Type_expr of type_expr
  | Variant of variant
  | Cell of cell
  | Field of field

let loc_of_type_expr = function
  | Sum (loc, _, _)
  | Record (loc, _, _)
  | Tuple (loc, _, _)
  | List (loc, _, _)
  | Option (loc, _, _)
  | Nullable (loc, _, _)
  | Shared (loc, _, _)
  | Wrap (loc, _, _)
  | Name (loc, _, _)
  | Tvar (loc, _) -> loc

let rec amap_type_expr f = function
  | Sum (loc, vl, a) ->  Sum (loc, List.map (amap_variant f) vl, f a)
  | Record (loc, fl, a) -> Record (loc, List.map (amap_field f) fl, f a)
  | Tuple (loc, tl, a) -> Tuple (loc, List.map (amap_cell f) tl, f a)
  | List (loc, t, a) -> List (loc, amap_type_expr f t, f a)
  | Option (loc, t, a) -> Option (loc, amap_type_expr f t, f a)
  | Nullable (loc, t, a) -> Nullable (loc, amap_type_expr f t, f a)
  | Shared (loc, t, a) -> Shared (loc, amap_type_expr f t, f a)
  | Wrap (loc, t, a) -> Wrap (loc, amap_type_expr f t, f a)
  | Tvar _ as x -> x
  | Name (loc, (loc2, name, args), a) ->
      Name (loc, (loc2, name, List.map (amap_type_expr f) args), f a)

and amap_variant f = function
    Variant (loc, (name, a), o) ->
      let o = Option.map (amap_type_expr f) o in
      Variant (loc, (name, f a), o)
  | Inherit (loc, x) ->
      Inherit (loc, amap_type_expr f x)

and amap_field f = function
    `Field (loc, (name, kind, a), x) ->
      `Field (loc, (name, kind, f a), amap_type_expr f x)
  | `Inherit (loc, x) ->
      `Inherit (loc, amap_type_expr f x)
and amap_cell f (loc, x, a) =
  (loc, amap_type_expr f x, f a)

let amap_module_item f (Type (loc, (name, param, a), x)) =
  Type (loc, (name, param, f a), amap_type_expr f x)

let amap_head f (loc, a) = (loc, f a)

let amap_body f l =
  List.map (amap_module_item f) l

let map_all_annot f ((head, body) : full_module) =
  (amap_head f head, amap_body f body)

let set_type_expr_loc loc = function
  | Sum (_, a, b) -> Sum (loc, a, b)
  | Record (_, a, b) -> Record (loc, a, b)
  | Tuple (_, a, b) -> Tuple (loc, a, b)
  | List (_, a, b) -> List (loc, a, b)
  | Option (_, a, b) -> Option (loc, a, b)
  | Nullable (_, a, b) -> Nullable (loc, a, b)
  | Shared (_, a, b) -> Shared (loc, a, b)
  | Wrap (_, a, b) -> Wrap (loc, a, b)
  | Name (_, a, b) -> Name (loc, a, b)
  | Tvar (_, a) -> Tvar (loc, a)

let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)

let error s = raise (Atd_error s)

let error_at loc s = error (string_of_loc loc ^ ":\n" ^ s)

let annot_of_type_expr = function
  | Sum (_, _, an)
  | Record (_, _, an)
  | Tuple (_, _, an)
  | List (_, _, an)
  | Option (_, _, an)
  | Nullable (_, _, an)
  | Shared (_, _, an)
  | Wrap (_, _, an)
  | Name (_, _, an) -> an
  | Tvar (_, _) -> []

let annot_of_variant (x : variant) =
  match x with
  | Variant (_, (_, an), _) -> an
  | Inherit _ -> []

let annot_of_field (x : field) =
  match x with
  | `Field (_, (_, _, an), _) -> an
  | `Inherit _ -> []

let map_annot f = function
  | Sum (loc, vl, a) ->  Sum (loc, vl, f a)
  | Record (loc, fl, a) -> Record (loc, fl, f a)
  | Tuple (loc, tl, a) -> Tuple (loc, tl, f a)
  | List (loc, t, a) -> List (loc, t, f a)
  | Option (loc, t, a) -> Option (loc, t, f a)
  | Nullable (loc, t, a) -> Nullable (loc, t, f a)
  | Shared (loc, t, a) -> Shared (loc, t, f a)
  | Wrap (loc, t, a) -> Wrap (loc, t, f a)
  | Tvar _ as x -> x
  | Name (loc, (loc2, name, args), a) ->
      Name (loc, (loc2, name, args), f a)

type visitor_hooks = {
  full_module: (full_module -> unit) -> full_module -> unit;
  module_head: (module_head -> unit) -> module_head -> unit;
  module_body: (module_body -> unit) -> module_body -> unit;
  module_item: (module_item -> unit) -> module_item -> unit;
  type_def: (type_def -> unit) -> type_def -> unit;
  type_expr: (type_expr -> unit) -> type_expr -> unit;
  variant: (variant -> unit) -> variant -> unit;
  cell: (cell -> unit) -> cell -> unit;
  field: (field -> unit) -> field -> unit;
}

let rec visit_type_expr hooks x =
  let cont x =
    match x with
    | Sum (loc, vl, a) -> List.iter (visit_variant hooks) vl
    | Record (loc, fl, a) -> List.iter (visit_field hooks) fl
    | Tuple (loc, tl, a) -> List.iter (visit_cell hooks) tl
    | List (loc, t, a) -> visit_type_expr hooks t
    | Option (loc, t, a) -> visit_type_expr hooks t
    | Nullable (loc, t, a) -> visit_type_expr hooks t
    | Shared (loc, t, a) -> visit_type_expr hooks t
    | Wrap (loc, t, a) -> visit_type_expr hooks t
    | Tvar _ -> ()
    | Name (loc, (loc2, name, args), a) ->
        List.iter (visit_type_expr hooks) args
  in
  hooks.type_expr cont x

and visit_variant hooks x =
  let cont x =
    match (x : variant) with
    | Variant (loc, (name, a), o) ->
        (match o with
         | None -> ()
         | Some x -> visit_type_expr hooks x)
    | Inherit (loc, x) -> visit_type_expr hooks x
  in
  hooks.variant cont x

and visit_field hooks x =
  let cont x =
    match x with
    | `Field (loc, (name, kind, a), x) -> visit_type_expr hooks x
    | `Inherit (loc, x) -> visit_type_expr hooks x
  in
  hooks.field cont x

and visit_cell hooks x =
  let cont (loc, x, a) = visit_type_expr hooks x in
  hooks.cell cont x

let visit_type_def hooks x =
  let cont (loc, (name, param, a), x) = visit_type_expr hooks x in
  hooks.type_def cont x

let visit_module_item hooks x =
  let cont (Type x) = visit_type_def hooks x in
  hooks.module_item cont x

let visit_module_head hooks x =
  let cont x = () in
  hooks.module_head cont x

let visit_module_body hooks x =
  let cont x = List.iter (visit_module_item hooks) x in
  hooks.module_body cont x

let visit_full_module hooks x =
  let cont (head, body) =
    visit_module_head hooks head;
    visit_module_body hooks body
  in
  hooks.full_module cont x

let visit
  ?(full_module = fun cont x -> cont x)
  ?(module_head = fun cont x -> cont x)
  ?(module_body = fun cont x -> cont x)
  ?(module_item = fun cont x -> cont x)
  ?(type_def = fun cont x -> cont x)
  ?(type_expr = fun cont x -> cont x)
  ?(variant = fun cont x -> cont x)
  ?(cell = fun cont x -> cont x)
  ?(field = fun cont x -> cont x)
  () =
  let hooks : visitor_hooks = {
    full_module;
    module_head;
    module_body;
    module_item;
    type_def;
    type_expr;
    variant;
    cell;
    field;
  } in
  let visit (any : any) =
    match any with
    | Full_module x -> visit_full_module hooks x
    | Module_head x -> visit_module_head hooks x
    | Module_body x -> visit_module_body hooks x
    | Module_item x -> visit_module_item hooks x
    | Type_def x -> visit_type_def hooks x
    | Type_expr x -> visit_type_expr hooks x
    | Variant x -> visit_variant hooks x
    | Cell x -> visit_cell hooks x
    | Field x -> visit_field hooks x
  in
  visit

let fold_annot
  ?module_head
  ?type_def
  ?type_expr
  ?variant
  ?cell
  ?field
  any init =
  let acc = ref init in
  let fold opt_folder get_annot =
    match opt_folder with
    | None -> (fun cont x -> cont x)
    | Some f ->
        (fun cont x ->
           acc := f x (get_annot x) !acc;
           cont x)
  in
  let visitor =
    visit
      ~module_head:(fold module_head (fun (_, an) -> an))
      ~type_def:(fold type_def (fun (_, (_, _, an), _) -> an))
      ~type_expr:(fold type_expr annot_of_type_expr)
      ~variant:(fold variant annot_of_variant)
      ~cell:(fold cell (fun (_, _, an) -> an))
      ~field:(fold field annot_of_field)
      ()
  in
  visitor any;
  !acc

(* TODO: rewrite this more compactly using the visitor machinery above *)
let rec fold (f : type_expr -> 'a -> 'a) (x : type_expr) acc =
  let acc = f x acc in
  match x with
    Sum (_, variant_list, _annot) ->
      List.fold_right (fold_variant f) variant_list acc

  | Record (_, field_list, _annot) ->
      List.fold_right (fold_field f) field_list acc

  | Tuple (_, l, _annot) ->
      List.fold_right (fun (_, x, _) acc -> fold f x acc) l acc

  | List (_, type_expr, _annot) ->
      fold f type_expr acc

  | Option (_, type_expr, _annot) ->
      fold f type_expr acc

  | Nullable (_, type_expr, _annot) ->
      fold f type_expr acc

  | Shared (_, type_expr, _annot) ->
      fold f type_expr acc

  | Wrap (_, type_expr, _annot) ->
      fold f type_expr acc

  | Name (_, (_2, _name, type_expr_list), _annot) ->
      List.fold_right (fold f) type_expr_list acc

  | Tvar (_, _string) ->
      acc

and fold_variant f x acc =
  match x with
    Variant (_, _, Some type_expr) -> fold f type_expr acc
  | Variant _ -> acc
  | Inherit (_, type_expr) -> fold f type_expr acc

and fold_field f x acc =
  match x with
    `Field (_, _, type_expr) -> fold f type_expr acc
  | `Inherit (_, type_expr) -> fold f type_expr acc


module Type_names = Set.Make (String)

let extract_type_names ?(ignorable = []) x =
  let ign s = List.mem s ignorable in

  let add s set =
    if ign s then set
    else Type_names.add s set
  in

  let acc =
    fold (
      fun x acc ->
        match x with
          Name (_, (_, name, _), _) -> add name acc
        | _ -> acc
    )
      x Type_names.empty
  in
  Type_names.elements acc

let is_parametrized x =
  fold (fun x b -> b || match x with Tvar _ -> true | _ -> false) x false

let is_required = function
  | Optional
  | With_default -> false
  | Required -> true
