open HopixAST

let type_error = Error.error "typechecking"

(** Abstract syntax for types.

    The following internal syntax for types is the same as the one for
    the types [ty] defined in {!HopixAST} except that all positions
    have been erased.

 *)
type aty =
  | ATyVar of type_variable
  | ATyCon of type_constructor * aty list
  | ATyTuple of aty list
  | ATyArrow of aty * aty

let make_fresh_name_generator () =
  let r = ref (-1) in
  let mangle () =
    if !r > 26 then "a" ^ string_of_int !r
    else String.make 1 Char.(chr (code 'a' + !r))
  in
  fun () ->
    incr r;
    TId ("`" ^ mangle ())

let fresh = make_fresh_name_generator ()

let rec hprod_destruct = function
  | ATyTuple tys -> List.(flatten (map hprod_destruct tys))
  | ty -> [ ty ]

let hprod tys = ATyTuple List.(flatten (map hprod_destruct tys))

let rec aty_of_ty = function
  | TyVar x -> ATyVar x
  | TyCon (t, ts) -> ATyCon (t, List.map aty_of_ty' ts)
  | TyArrow (ins, out) -> ATyArrow (aty_of_ty' ins, aty_of_ty' out)
  | TyTuple ins -> hprod (List.map aty_of_ty' ins)

and aty_of_ty' x = aty_of_ty (Position.value x)

let pretty_print_aty bound_vars aty =
  let fresh = make_fresh_name_generator () in
  let r = ref [] in
  let print_var x =
    ( if not (List.mem x bound_vars) then x
    else
      try List.assoc x !r
      with Not_found ->
        let y = fresh () in
        r := (x, y) :: !r;
        y )
    |> function
    | TId x -> x
  in
  let rec print_aty = function
    | ATyVar x -> print_var x
    | ATyArrow (ins, out) ->
        let ins = print_aty' ins in
        let out = print_aty' out in
        ins ^ " -> " ^ out
    | ATyTuple tys -> String.concat " * " (List.map print_aty' tys)
    | ATyCon (TCon x, []) -> x
    | ATyCon (TCon x, ts) ->
        x ^ "<" ^ String.concat ", " (List.map print_aty' ts) ^ ">"
  and print_aty' = function
    | ATyArrow (_, _) as t -> "(" ^ print_aty t ^ ")"
    | x -> print_aty x
  in
  let s = print_aty aty in
  (s, !r)

let print_aty aty = fst (pretty_print_aty [] aty)

let tvar x = ATyVar (TId x)

let ( --> ) tys ty =
  List.fold_left (fun ty aty -> ATyArrow (aty, ty)) ty (List.rev tys)

let rec destruct_arrows = function
  | ATyArrow (ins, out) ->
      let ins', out = destruct_arrows out in
      (ins :: ins', out)
  | ty -> ([], ty)

exception NotAFunction

let output_type_of_function = function
  | ATyArrow (_, ty) -> ty
  | _ -> raise NotAFunction

let constant x = (TCon x, ATyCon (TCon x, []))

let tcunit, hunit = constant "unit"

let tcbool, hbool = constant "bool"

let tcint, hint = constant "int"

let tcstring, hstring = constant "string"

let tcchar, hchar = constant "char"

let tcref = TCon "mut"

let href ty = ATyCon (tcref, [ ty ])

exception NotAReference

let type_of_reference_type = function
  | ATyCon (t, [ ty ]) when t = tcref -> ty
  | _ -> raise NotAReference

module TypeVariableSet = Set.Make (struct
  type t = type_variable

  let compare = compare
end)

let rec occurs x = function
  | ATyVar tv -> x = tv
  | ATyCon (_, tys) -> List.exists (occurs x) tys
  | ATyArrow (ins, out) -> List.exists (occurs x) [ out; ins ]
  | ATyTuple tys -> List.exists (occurs x) tys

let free_type_variables ty =
  let rec aux accu = function
    | ATyVar tv -> TypeVariableSet.add tv accu
    | ATyCon (_, tys) -> aux' accu tys
    | ATyArrow (ins, out) -> aux (aux accu out) ins
    | ATyTuple tys -> aux' accu tys
  and aux' accu = function [] -> accu | ty :: tys -> aux' (aux accu ty) tys in
  TypeVariableSet.elements (aux TypeVariableSet.empty ty)

type aty_scheme = Scheme of type_variable list * aty

let mk_type_scheme ty = Scheme (free_type_variables ty, ty)

let monotype ty = Scheme ([], ty)

exception NotAMonotype

let type_of_monotype = function
  | Scheme ([], ty) -> ty
  | _ -> raise NotAMonotype

exception InvalidInstantiation of int * int

let rec substitute phi = function
  | ATyVar tv -> ( try List.assoc tv phi with Not_found -> ATyVar tv )
  | ATyArrow (ins, out) -> ATyArrow (substitute phi ins, substitute phi out)
  | ATyCon (t, tys) -> ATyCon (t, List.map (substitute phi) tys)
  | ATyTuple tys -> hprod (List.map (substitute phi) tys)

let instantiate_type_scheme (Scheme (ts, ty)) types =
  if List.(length ts <> length types) then
    raise (InvalidInstantiation (List.length ts, List.length types));
  let substitution = List.combine ts types in
  substitute substitution ty

let refresh_type_scheme (Scheme (ts, ty)) =
  let ts' = List.map (fun _ -> fresh ()) ts in
  let phi = List.(map (fun (x, y) -> (x, ATyVar y)) (combine ts ts')) in
  Scheme (ts', substitute phi ty)

type typing_environment = {
  values : (identifier * aty_scheme) list;
  constructors : (constructor * aty_scheme) list;
  destructors : (label * aty_scheme) list;
  type_constructors : (type_constructor * (int * type_information)) list;
  type_variables : type_variable list;
}

and type_information =
  | Abstract
  | Sum of constructor list
  | Record of label list

let diffdomain tenv tenv' =
  let d =
    List.fold_left
      (fun s (x, _) -> if not (List.mem_assoc x tenv.values) then x :: s else s)
      [] tenv'.values
  in
  List.sort compare d

exception UnboundTypeConstructor of Position.position * type_constructor

let check_well_formed_type pos env ty =
  let rec aux = function
    | ATyVar (TId a as x) ->
        if not (List.mem x env.type_variables) then
          type_error pos
            (Printf.sprintf "Ill-formed type: unbound type variable %s." a)
    | ATyCon (tcon, atys) -> (
        try
          let arity, _ = List.assoc tcon env.type_constructors in
          List.iter aux atys;
          if List.length atys <> arity then
            type_error pos "Ill-formed type: invalid arity."
        with Not_found ->
          type_error pos "Ill-formed type: unbound type constructor." )
    | ATyTuple tys -> List.iter aux tys
    | ATyArrow (ins, out) ->
        aux ins;
        aux out
  in
  aux ty

let internalize_ty env ty =
  let pos = Position.position ty in
  let ty = Position.value ty in
  let aty = aty_of_ty ty in
  check_well_formed_type pos env aty;
  aty

let empty_typing_environment =
  {
    values = [];
    constructors = [];
    type_constructors = [];
    destructors = [];
    type_variables = [];
  }

let print_tenv env =
  Printf.sprintf "tvs: %s\n"
    (String.concat ", " (List.map (fun (TId x) -> x) env.type_variables))

exception AlreadyBoundTypeVariable of Position.position * type_variable

let bind_type_variable pos env tv =
  if List.mem tv env.type_variables then
    raise (AlreadyBoundTypeVariable (pos, tv));
  { env with type_variables = tv :: env.type_variables }

let bind_type_variables pos env ts =
  List.fold_left (fun env t -> bind_type_variable pos env t) env ts

let is_type_variable_defined _ env tv = List.mem tv env.type_variables

let bind_value x scheme env = { env with values = (x, scheme) :: env.values }

exception UnboundIdentifier of Position.position * identifier

let lookup_type_scheme_of_value pos x env =
  try List.assoc x env.values
  with Not_found -> raise (UnboundIdentifier (pos, x))

let make_pre_type_environment env ts x arity tdef =
  let env = bind_type_variables Position.dummy env ts in
  let type_constructors = (x, (arity, tdef)) :: env.type_constructors in
  { env with type_constructors; constructors = [] }

let bind_abstract_type x ts env =
  let arity = List.length ts in
  let type_constructors = (x, (arity, Abstract)) :: env.type_constructors in
  { env with type_constructors }

let bind_sum_type_definition x ts ds env =
  let arity = List.length ts in
  let constructors = List.map (fun (k, _) -> Position.value k) ds in
  let pre_env = make_pre_type_environment env ts x arity (Sum constructors) in
  let constructor_definition (k, tys) =
    let atys = List.map (internalize_ty pre_env) tys in
    let scheme =
      Scheme (ts, atys --> ATyCon (x, List.map (fun v -> ATyVar v) ts))
    in
    (Position.value k, scheme)
  in
  let constructors = List.map constructor_definition ds @ env.constructors in
  let type_constructors =
    (x, (arity, Sum (List.map fst constructors))) :: env.type_constructors
  in
  { env with type_constructors; constructors }

let bind_record_type_definition x ts fs env =
  let arity = List.length ts in
  let labels = List.map (fun (l, _) -> Position.value l) fs in
  let pre_env = make_pre_type_environment env ts x arity (Record labels) in
  let destructor_definition (l, ty) =
    let aty = internalize_ty pre_env ty in
    let scheme =
      Scheme (ts, [ ATyCon (x, List.map (fun v -> ATyVar v) ts) ] --> aty)
    in
    (Position.value l, scheme)
  in
  let destructors = List.map destructor_definition fs @ env.destructors in
  let type_constructors =
    (x, (arity, Record labels)) :: env.type_constructors
  in
  { env with type_constructors; destructors }

let bind_type_definition x ts tenv = function
  | DefineSumType ks -> bind_sum_type_definition x ts ks tenv
  | DefineRecordType fs -> bind_record_type_definition x ts fs tenv
  | Abstract -> bind_abstract_type x ts tenv

exception UnboundConstructor

let lookup_type_scheme_of_constructor x env =
  try List.assoc x env.constructors with Not_found -> raise UnboundConstructor

exception UnboundLabel

let lookup_type_scheme_of_label l env =
  try List.assoc l env.destructors with Not_found -> raise UnboundLabel

let lookup_type_constructor_of_label l env =
  try
    let label_type_definition (_, (_, d)) =
      match d with Record labels -> List.mem l labels | _ -> false
    in
    let tycon, (arity, labels) =
      List.find label_type_definition env.type_constructors
    in
    let labels = match labels with Record ls -> ls | _ -> assert false in
    (tycon, arity, labels)
  with Not_found -> raise UnboundLabel

let initial_typing_environment () =
  empty_typing_environment
  |> List.fold_right
       (fun ti env -> bind_abstract_type ti [] env)
       [ tcunit; tcstring; tcchar; tcint; tcbool ]
  |> bind_abstract_type (TCon "mut") [ TId "'a" ]
  |> List.fold_right
       (fun (x, s) env -> bind_value (Id x) (mk_type_scheme s) env)
       [
         ("true", hbool);
         ("false", hbool);
         ("nothing", hunit);
         ("print_int", [ hint ] --> hunit);
         ("print_string", [ hstring ] --> hunit);
         ("print", [ tvar "'a" ] --> hunit);
         ("`||`", [ hbool; hbool ] --> hbool);
         ("`&&`", [ hbool; hbool ] --> hbool);
         ("`=?`", [ hint; hint ] --> hbool);
         ("`<=?`", [ hint; hint ] --> hbool);
         ("`>=?`", [ hint; hint ] --> hbool);
         ("`<?`", [ hint; hint ] --> hbool);
         ("`>?`", [ hint; hint ] --> hbool);
         ("`+`", [ hint; hint ] --> hint);
         ("`*`", [ hint; hint ] --> hint);
         ("`-`", [ hint; hint ] --> hint);
         ("`/`", [ hint; hint ] --> hint);
       ]

let print_type_scheme (Scheme (ts, aty)) =
  let sty, subst = pretty_print_aty ts aty in
  let ts = List.(map (fun x -> assoc x subst) ts) in
  let forall =
    let type_variable (TId s) = s in
    match ts with
    | [] -> ""
    | ts -> "[ " ^ String.concat " " (List.map type_variable ts) ^ "] "
  in
  forall ^ sty

let print_binding (Id x, s) = x ^ " : " ^ print_type_scheme s

let print_typing_environment tenv =
  let excluded = initial_typing_environment () in
  let values =
    List.filter
      (fun (x, _) -> not (List.mem_assoc x excluded.values))
      (List.rev tenv.values)
  in
  String.concat "\n" (List.map print_binding values)
