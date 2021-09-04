open Base
open Ppxlib
open Ast_builder.Default

module Filename = Caml.Filename
module Parsing  = Caml.Parsing

module Type = struct
  type t =
    | Var of string
    | Bool
    | Int
    | Char
    | String
    | Tuple of t list

  let rec to_string = function
    | Var v   -> "'" ^ v
    | Bool    -> "bool"
    | Int     -> "int"
    | Char    -> "char"
    | String  -> "string"
    | Tuple l -> "(" ^ String.concat ~sep:" * " (List.map l ~f:to_string) ^ ")"
end

module Value = struct

  type t =
    | Bool   of bool
    | Int    of int
    | Char   of char
    | String of string
    | Tuple  of t list

  let ocaml_version =
    Caml.Scanf.sscanf Caml.Sys.ocaml_version "%d.%d.%d"
      (fun major minor patchlevel -> Tuple [Int major; Int minor; Int patchlevel])
  ;;

  let config_bool name =
    Bool
      (Ocaml_common.Config.config_var name
       |> Option.map ~f:Bool.of_string
       |> Option.value ~default:false)
  ;;


  let flambda_backend = config_bool "flambda_backend";;

  let flambda2 = config_bool "flambda2";;

  let rec to_expression loc t =
    match t with
    | Bool   x   -> ebool   ~loc x
    | Int    x   -> eint    ~loc x
    | Char   x   -> echar   ~loc x
    | String x   -> estring ~loc x
    | Tuple  []  -> eunit   ~loc
    | Tuple  [x] -> to_expression loc x
    | Tuple  l   -> pexp_tuple ~loc (List.map l ~f:(to_expression loc))
  ;;

  let rec to_pattern loc t =
    match t with
    | Bool   x   -> pbool   ~loc x
    | Int    x   -> pint    ~loc x
    | Char   x   -> pchar   ~loc x
    | String x   -> pstring ~loc x
    | Tuple  []  -> punit   ~loc
    | Tuple  [x] -> to_pattern loc x
    | Tuple  l   -> ppat_tuple ~loc (List.map l ~f:(to_pattern loc))
  ;;

  let to_string_pretty v =
    let e = to_expression Location.none v in
    Pprintast.string_of_expression e

  let to_string v =
    let buf = Buffer.create 128 in
    let rec aux = function
      | Bool b ->
        Buffer.add_string buf (Bool.to_string b)
      | Int n ->
        Buffer.add_string buf (Int.to_string n)
      | Char ch ->
        Buffer.add_char buf ch
      | String s ->
        Buffer.add_string buf s;
      | Tuple [] ->
        Buffer.add_string buf "()"
      | Tuple (x :: l) ->
        Buffer.add_char buf '(';
        aux x;
        List.iter l ~f:(fun x ->
          Buffer.add_string buf ", ";
          aux x);
        Buffer.add_char buf ')'
    in
    aux v;
    Buffer.contents buf
  ;;

  let rec type_ : t -> Type.t = function
    | Bool   _ -> Bool
    | Int    _ -> Int
    | Char   _ -> Char
    | String _ -> String
    | Tuple  l -> Tuple (List.map l ~f:type_)
  ;;
end

module Env : sig
  type t

  val init  : t
  val empty : t

  val add : t -> var:string Location.loc -> value:Value.t -> t
  val undefine : t -> string Location.loc -> t

  val of_list : (string Location.loc * Value.t) list -> t

  val eval : t -> string Location.loc -> Value.t
  val is_defined : ?permissive:bool -> t -> string Location.loc -> bool

  val seen : t -> string Location.loc -> bool

  val to_expression : t -> expression
end = struct
  type var_state =
    | Defined of Value.t
    | Undefined

  type entry =
    { loc   : Location.t (** Location at which it was defined/undefined *)
    ; state : var_state
    }

  type t = entry Map.M(String).t

  let empty = Map.empty (module String)

  let to_expression t =
    pexp_apply ~loc:Location.none (evar ~loc:Location.none "env")
      (List.map (Map.to_alist t) ~f:(fun (var, { loc; state }) ->
         (Labelled var,
          match state with
          | Defined v -> pexp_construct ~loc { txt = Lident "Defined"; loc }
                           (Some (Value.to_expression loc v))
          | Undefined -> pexp_construct ~loc { txt = Lident "Undefined"; loc }
                           None)))

  let seen t (var : _ Loc.t) = Map.mem t var.txt

  let add t ~(var:_ Loc.t) ~value =
    Map.set t ~key:var.txt ~data:{ loc = var.loc; state = Defined value }
  ;;

  let undefine t (var : _ Loc.t) =
    Map.set t ~key:var.txt ~data:{ loc = var.loc; state = Undefined }
  ;;

  let of_list l = List.fold_left l ~init:empty ~f:(fun acc (var, value) ->
    add acc ~var ~value)
  ;;

  let init =
    of_list
      [ { loc = Location.none
        ; txt = "ocaml_version"
        },
        Value.ocaml_version
      ; { loc = Location.none
        ; txt = "flambda_backend"
        },
        Value.flambda_backend
      ; { loc = Location.none
        ; txt = "flambda2"
        },
        Value.flambda2
      ]

  let short_loc_string (loc : Location.t) =
    Printf.sprintf "%s:%d" loc.loc_start.pos_fname loc.loc_start.pos_lnum
  ;;

  let eval (t : t) (var:string Loc.t) =
    match Map.find t var.txt with
    | Some { state = Defined v; loc = _  } -> v
    | Some { state = Undefined; loc      } ->
      Location.raise_errorf ~loc:var.loc "optcomp: %s is undefined (undefined at %s)"
        var.txt (short_loc_string loc)
    | None ->
      Location.raise_errorf ~loc:var.loc "optcomp: unbound value %s" var.txt
  ;;

  let is_defined ?(permissive=false) (t : t) (var:string Loc.t) =
    match Map.find t var.txt with
    | Some { state = Defined _; _ } -> true
    | Some { state = Undefined; _ } -> false
    | None -> if permissive then false else
      Location.raise_errorf ~loc:var.loc
        "optcomp: doesn't know about %s.\n\
         You need to either define it or undefine it with #undef.\n\
         Optcomp doesn't accept variables it doesn't know about to avoid typos."
        var.txt
  ;;
end

(* +-----------------------------------------------------------------+
   | Expression evaluation                                           |
   +-----------------------------------------------------------------+ *)

let invalid_type loc expected real =
  Location.raise_errorf ~loc
    "optcomp: this expression has type %s but is used with type %s"
    (Type.to_string real) (Type.to_string expected)
;;

let var_of_lid (id : _ Located.t) =
  match Longident.flatten_exn id.txt with
  | l -> { id with txt = String.concat ~sep:"." l }
  | exception _ ->
    Location.raise_errorf ~loc:id.loc "optcomp: invalid variable name"
;;

let cannot_convert loc dst x =
  Location.raise_errorf ~loc "cannot convert %s to %s" (Value.to_string_pretty x) dst
;;

let convert_from_string loc dst f x =
  try
    f x
  with _ ->
    Location.raise_errorf ~loc "optcomp: cannot convert %S to %s" x dst
;;

exception Pattern_match_failure of pattern * Value.t

let lid_of_expr e =
  match e.pexp_desc with
  | Pexp_ident id | Pexp_construct (id, None) -> id
  | _ -> Location.raise_errorf ~loc:e.pexp_loc "optcomp: identifier expected"
;;

let var_of_expr e = var_of_lid (lid_of_expr e)

let not_supported e =
  Location.raise_errorf ~loc:e.pexp_loc "optcomp: expression not supported"
;;

let parse_int loc x =
  match Int.of_string x with
  | v -> v
  | exception _ ->
    Location.raise_errorf ~loc "optcomp: invalid integer"
;;

let rec eval env e : Value.t =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant (Pconst_integer    (x, None)) -> Int (parse_int loc x)
  | Pexp_constant (Pconst_char    x       ) -> Char x
  | Pexp_constant (Pconst_string (x, _, _   )) -> String x

  | Pexp_construct ({ txt = Lident "true" ; _ }, None) -> Bool true
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> Bool false
  | Pexp_construct ({ txt = Lident "()"   ; _ }, None) -> Tuple []

  | Pexp_tuple l -> Tuple (List.map l ~f:(eval env))

  | Pexp_ident id | Pexp_construct (id, None) ->
      Env.eval env (var_of_lid id)

  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ }, args) -> begin
      let args =
        List.map args ~f:(fun (l, x) -> match l with Nolabel -> x | _ -> not_supported e)
      in
      match s, args with
      | "="  , [x; y] -> eval_cmp     env Poly.( = )   x y
      | "<"  , [x; y] -> eval_cmp     env Poly.( < )   x y
      | ">"  , [x; y] -> eval_cmp     env Poly.( > )   x y
      | "<=" , [x; y] -> eval_cmp     env Poly.( <= )  x y
      | ">=" , [x; y] -> eval_cmp     env Poly.( >= )  x y
      | "<>" , [x; y] -> eval_cmp     env Poly.( <> )  x y
      | "min", [x; y] -> eval_poly2   env Poly.min     x y
      | "max", [x; y] -> eval_poly2   env Poly.max     x y
      | "+"  , [x; y] -> eval_int2    env ( + )   x y
      | "-"  , [x; y] -> eval_int2    env ( - )   x y
      | "*"  , [x; y] -> eval_int2    env ( * )   x y
      | "/"  , [x; y] -> eval_int2    env ( / )   x y
      | "mod", [x; y] -> eval_int2    env Caml.( mod ) x y
      | "not", [x]    -> Bool (not (eval_bool env x))
      | "||" , [x; y] -> eval_bool2   env ( || ) x y
      | "&&" , [x; y] -> eval_bool2   env ( && ) x y
      | "^"  , [x; y] -> eval_string2 env ( ^ )  x y
      | "fst", [x]    -> fst (eval_pair env x)
      | "snd", [x]    -> snd (eval_pair env x)
      | "to_string", [x] ->
        String (Value.to_string (eval env x))
      | "to_int", [x] ->
        Int
          (match eval env x with
           | String x -> convert_from_string loc "int" Int.of_string x
           | Int    x -> x
           | Char   x -> Char.to_int x
           | Bool _ | Tuple _ as x -> cannot_convert loc "int" x)
      | "to_bool", [x] ->
        Bool
          (match eval env x with
           | String x -> convert_from_string loc "bool" Bool.of_string x
           | Bool   x -> x
           | Int _ | Char _ | Tuple _ as x -> cannot_convert loc "bool" x)
      | "to_char", [x] ->
        Char
          (match eval env x with
           | String x ->
             convert_from_string loc "char"
               (fun s -> assert (String.length s = 1); s.[0]) x
           | Char x -> x
           | Int x ->
             begin
               match
                 Char.of_int x
               with
               | Some x -> x
               | None ->
                 Location.raise_errorf ~loc "optcomp: cannot convert %d to char" x
             end
           | Bool _ | Tuple _ as x -> cannot_convert loc "char" x)
      | "show", [x] -> let v = eval env x in
        let ppf = Caml.Format.err_formatter in
        let pprinted = Value.to_string_pretty v in
        Caml.Format.fprintf ppf "%a:@.SHOW %s@." Location.print loc pprinted;
        v
      | "defined", [x] -> Bool (Env.is_defined env (var_of_expr x))
      | "not_defined", [x] -> Bool (not (Env.is_defined env (var_of_expr x)))
      | "not_defined_permissive", [x] -> Bool (not (
        Env.is_defined ~permissive:true env (var_of_expr x)))
      | _ -> not_supported e
    end

  (* Let-binding *)
  | Pexp_let (Nonrecursive, vbs, e) ->
    let env =
      List.fold_left vbs ~init:env ~f:(fun new_env vb ->
        let v = eval env vb.pvb_expr in
        do_bind new_env vb.pvb_pat v)
    in
    eval env e

  (* Pattern matching *)
  | Pexp_match (e, cases) ->
    let v = eval env e in
    let rec loop = function
      | [] ->
        Location.raise_errorf ~loc "optcomp: cannot match %s against any of the cases"
          (Value.to_string v)
      | case :: rest ->
        match bind env case.pc_lhs v with
        | exception Pattern_match_failure _ -> loop rest
        | env ->
          let guard_ok =
            match case.pc_guard with
            | None   -> true
            | Some e -> eval_bool env e
          in
          if guard_ok then
            eval env case.pc_rhs
          else
            loop rest
    in
    loop cases

  | _ -> not_supported e

and bind env patt value =
  let loc = patt.ppat_loc in
  match patt.ppat_desc, value with
  | Ppat_any, _ -> env

  | Ppat_constant (Pconst_integer    (x, None)), Int    y when parse_int loc x = y -> env
  | Ppat_constant (Pconst_char    x       ), Char   y when Char.equal   x y -> env
  | Ppat_constant (Pconst_string (x, _, _   )), String y when String.equal x y -> env

  | Ppat_construct ({ txt = Lident "true" ; _ }, None), Bool true  -> env
  | Ppat_construct ({ txt = Lident "false"; _ }, None), Bool false -> env
  | Ppat_construct ({ txt = Lident "()"   ; _ }, None), Tuple []   -> env

  | Ppat_var var, _              -> Env.add env ~var ~value
  | Ppat_construct (id, None), _ -> Env.add env ~var:(var_of_lid id) ~value

  | Ppat_alias (patt, var), _ ->
    Env.add (bind env patt value) ~var ~value

  | Ppat_tuple x, Tuple y when List.length x = List.length y ->
    Caml.ListLabels.fold_left2 x y ~init:env ~f:bind

  | _ ->
    raise (Pattern_match_failure (patt, value))

and do_bind env patt value =
  try
    bind env patt value
  with Pattern_match_failure (pat, v) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Cannot match %s with this pattern" (Value.to_string_pretty v)

and eval_same env ex ey =
  let vx = eval env ex and vy = eval env ey in
  let tx = Value.type_ vx and ty = Value.type_ vy in
  if Poly.equal tx ty then
    (vx, vy)
  else
    invalid_type ey.pexp_loc tx ty

and eval_int env e =
  match eval env e with
  | Int x -> x
  | v -> invalid_type e.pexp_loc Int (Value.type_ v)

and eval_bool env e =
  match eval env e with
  | Bool x -> x
  | v -> invalid_type e.pexp_loc Bool (Value.type_ v)

and eval_string env e =
  match eval env e with
  | String x -> x
  | v -> invalid_type e.pexp_loc String (Value.type_ v)

and eval_pair env e =
  match eval env e with
  | Tuple [x; y] -> (x, y)
  | v -> invalid_type e.pexp_loc (Tuple [Var "a"; Var "b"]) (Value.type_ v)

and eval_int2 env f a b =
  let a = eval_int env a in
  let b = eval_int env b in
  Int (f a b)

and eval_bool2 env f a b =
  let a = eval_bool env a in
  let b = eval_bool env b in
  Bool (f a b)

and eval_string2 env f a b =
  let a = eval_string env a in
  let b = eval_string env b in
  String (f a b)

and eval_cmp env f a b =
  let a, b = eval_same env a b in
  Bool (f a b)

and eval_poly2 env f a b =
  let a, b = eval_same env a b in
  f a b


(* +-----------------------------------------------------------------+
   | Environment serialization                                       |
   +-----------------------------------------------------------------+ *)

module EnvIO = struct
  let to_expression = Env.to_expression

  let of_expression expr =
    Ast_pattern.parse
      Ast_pattern.(pexp_apply (pexp_ident (lident (string "env"))) __)
      expr.pexp_loc
      expr
      (fun args ->
         List.fold args ~init:Env.empty ~f:(fun env arg ->
           match arg with
           | Labelled var, { pexp_desc = Pexp_construct ({txt=Lident "Defined"; _},
                                                         Some e)
                           ; pexp_loc = loc
                           ; _
                           } ->
             Env.add env ~var:{ txt = var; loc } ~value:(eval Env.empty e)
           | Labelled var, { pexp_desc = Pexp_construct ({txt=Lident "Undefined"; _},
                                                         None)
                           ; pexp_loc = loc
                           ; _
                           } ->
             Env.undefine env { txt = var; loc }
           | _, e ->
             Location.raise_errorf ~loc:e.pexp_loc "ppx_optcomp: invalid cookie"))
end
