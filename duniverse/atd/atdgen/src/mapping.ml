open Atd.Import

type loc = Atd.Ast.loc

(*
  Generic mapping, based on the core ATD types
*)
type ('a, 'b) mapping =
  | Unit of loc * 'a * 'b
  | Bool of loc * 'a * 'b
  | Int of loc * 'a * 'b
  | Float of loc * 'a * 'b
  | String of loc * 'a * 'b
  | Sum of loc * ('a, 'b) variant_mapping array * 'a * 'b
  | Record of loc * ('a, 'b) field_mapping array * 'a * 'b
  | Tuple of loc * ('a, 'b) cell_mapping array * 'a * 'b
  | List of loc * ('a, 'b) mapping * 'a * 'b
  | Option of loc * ('a, 'b) mapping * 'a * 'b
  | Nullable of loc * ('a, 'b) mapping * 'a * 'b
  | Wrap of loc * ('a, 'b) mapping * 'a * 'b
  | Name of loc * string * ('a, 'b) mapping list * 'a option * 'b option
  | External of loc * string * ('a, 'b) mapping list * 'a * 'b
  | Tvar of loc * string

and ('a, 'b) cell_mapping = {
  cel_loc : loc;
  cel_value : ('a, 'b) mapping;
  cel_arepr : 'a;
  cel_brepr : 'b
}

and ('a, 'b) field_mapping = {
  f_loc : loc;
  f_name : string;
  f_kind : Atd.Ast.field_kind;
  f_value : ('a, 'b) mapping;
  f_arepr : 'a;
  f_brepr : 'b
}

and ('a, 'b) variant_mapping = {
  var_loc : loc;
  var_cons : string;
  var_arg : ('a, 'b) mapping option;
  var_arepr : 'a;
  var_brepr : 'b
}

type ('a, 'b) def = {
  def_loc : loc;
  def_name : string;
  def_param : string list;
  def_value : ('a, 'b) mapping option;
  def_arepr : 'a;
  def_brepr : 'b;
}


let as_abstract = function
    Atd.Ast.Name (_, (loc, "abstract", l), a) ->
      if l <> [] then
        Error.error loc "\"abstract\" takes no type parameters";
      Some (loc, a)
  | _ ->
      None

let is_abstract x = as_abstract x <> None


let loc_of_mapping x =
  match (x : (_, _) mapping) with
    | Unit (loc, _, _)
    | Bool (loc, _, _)
    | Int (loc, _, _)
    | Float (loc, _, _)
    | String (loc, _, _)
    | Sum (loc, _, _, _)
    | Record (loc, _, _, _)
    | Tuple (loc, _, _, _)
    | List (loc, _, _, _)
    | Option (loc, _, _, _)
    | Nullable (loc, _, _, _)
    | Wrap (loc, _, _, _)
    | Name (loc, _, _, _, _)
    | External (loc, _, _, _, _)
    | Tvar (loc, _) -> loc


module Env = Map.Make (String)

let rec subst env (x : (_, _) mapping) =
  match x with
    Unit (_, _, _)
  | Bool (_, _, _)
  | Int (_, _, _)
  | Float (_, _, _)
  | String (_, _, _) -> x
  | Sum (loc, ar, a, b) ->
      Sum (loc, Array.map (subst_variant env) ar, a, b)
  | Record (loc, ar, a, b) ->
      Record (loc, Array.map (subst_field env) ar, a, b)
  | Tuple (loc, ar, a, b) ->
      Tuple (loc, Array.map (subst_cell env) ar, a, b)
  | List (loc, x, a, b) ->
      List (loc, subst env x, a, b)
  | Option (loc, x, a, b) ->
      Option (loc, subst env x, a, b)
  | Nullable (loc, x, a, b) ->
      Nullable (loc, subst env x, a, b)
  | Wrap (loc, x, a, b) ->
      Wrap (loc, subst env x, a, b)
  | Name (loc, name, args, a, b) ->
      Name (loc, name, List.map (subst env) args, a, b)
  | External (loc, name, args, a, b) ->
      External (loc, name, List.map (subst env) args, a, b)
  | Tvar (_, s) ->
      try Env.find s env
      with Not_found ->
        invalid_arg (sprintf "Mapping.subst_var: '%s" s)

and subst_variant env x =
  match x.var_arg with
      None -> x
    | Some v -> { x with var_arg = Some (subst env v) }

and subst_field env x =
  { x with f_value = subst env x.f_value }

and subst_cell env x =
  { x with cel_value = subst env x.cel_value }

(*
  Substitute type variables param in x by args
*)
let apply param x args =
  if List.length param <> List.length args then
    invalid_arg "Mapping.apply";
  let env =
    List.fold_left2
      (fun env var value -> Env.add var value env)
      Env.empty param args
  in
  subst env x


let rec find_name loc env visited name =
  if List.mem name visited then
    Error.error loc "Cyclic type definition"
  else
    let param, x = Env.find name env in
    (param, deref_expr env (name :: visited) x)

and deref_expr env visited x =
  match x with
    Name (loc, name, args, _, _) ->
      (try
         let param, x = find_name loc env visited name in
         apply param x args
       with Not_found -> x)
  | _ -> x

let make_deref
    (l : (bool * ('a, 'b) def list) list) :
    (('a, 'b) mapping -> ('a, 'b) mapping) =

  let defs =
    List.fold_left
      (fun env d ->
         match d.def_value with
             None -> env
           | Some v -> Env.add d.def_name (d.def_param, v) env)
      Env.empty (List.concat_map snd l) in

  fun x -> deref_expr defs [] x

(*
   Resolve names and unwrap `wrap` constructs
   (discarding annotations along the way)
*)
let rec unwrap (deref: ('a, 'b) mapping -> ('a, 'b) mapping) x =
  match deref x with
  | Wrap (_, x, _, _) -> unwrap deref x
  | x -> x
