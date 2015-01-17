open Core.Std

type expr =
  | Const of float
  | Var   of string
  | Prod  of expr * expr
  | Sum   of expr * expr
with sexp

let precedence = function
  | Const _ -> 0
  | Var   _ -> 1
  | Prod  _ -> 2
  | Sum   _ -> 3

let rec to_string expr =
  let maybe_paren sub =
    if precedence sub > precedence expr then
      "(" ^ to_string sub ^ ")"
    else
      to_string sub
  in
  match expr with
  | Const x -> sprintf "%G" x
  | Var s -> s
  | Prod (x,y) -> maybe_paren x ^ " " ^ maybe_paren y
  | Sum (x,y) -> maybe_paren x ^ " + " ^ maybe_paren y
;;

let rec apply_rule_somewhere rule expr =
  match rule expr with
  | Some _ as expr -> expr
  | None ->
    let apply_to_one_of e1 e2 =
      match apply_rule_somewhere rule e1 with
      | Some e1' -> Some (e1', e2)
      | None ->
        match apply_rule_somewhere rule e2 with
        | Some e2' -> Some (e1, e2')
        | None -> None
    in
    match expr with
    | Const _ | Var _ -> None
    | Prod (e1,e2) ->
      Option.map (apply_to_one_of e1 e2) ~f:(fun (e1,e2) -> Prod (e1,e2))
    | Sum (e1,e2) ->
      Option.map (apply_to_one_of e1 e2) ~f:(fun (e1,e2) -> Sum  (e1,e2))

let rec apply_some_rule named_rules expr =
  match named_rules with
  | [] -> None
  | (name,rule) :: tl ->
    match apply_rule_somewhere rule expr with
    | None -> apply_some_rule tl expr
    | Some expr' -> Some (name,expr')

let rec build_trace named_rules trace =
  match trace with
  | [] -> []
  | (_,expr) :: _ ->
    match apply_some_rule named_rules expr with
    | None -> List.rev trace
    | Some step -> build_trace named_rules (step :: trace)

let simplify named_rules expr =
  build_trace named_rules ["initial expression", expr]

let distribute = function
  | Prod (Sum (a,b), c) -> Some (Sum (Prod (a,c), Prod (b,c)))
  | Prod (a, Sum (b,c)) -> Some (Sum (Prod (a,b), Prod (a,c)))
  | _ -> None

let simplify_constants = function
  | Sum  (Const 0., x)      -> Some x
  | Sum  (Const x, Const y) -> Some (Const (x +. y))
  | Prod (Const 0., _)      -> Some (Const 0.)
  | Prod (Const 1., x)      -> Some x
  | Prod (Const x, Const y) -> Some (Const (x *. y))
  | _ -> None

let rec prod_to_list = function
  | Prod (x,y) -> prod_to_list x @ prod_to_list y
  | e -> [e]

let rec prod_of_list = function
  | []  -> Const 1.
  | [x] -> x
  | hd :: tl -> Prod (hd,prod_of_list tl)

let collapse_products expr =
  let (constants,other) = List.partition_map (prod_to_list expr)
    ~f:(function Const x -> `Fst x | x -> `Snd x)
  in
  match constants with
  | [] | [_] -> None
  | hd :: tl ->
    let constant = Const (List.fold ~init:hd ~f:( *. ) tl) in
    match other with
    | [] -> Some constant
    | _ -> Some (Prod (constant, prod_of_list other))

let some_rules =
  [ "simplify constants", simplify_constants;
    "distribute", distribute;
    "collapse constants in prod", collapse_products;
  ]

let rec print_trace = function
  | [] -> ()
  | (rule_name,expr) :: tl ->
    printf "%-30s: %s\n" rule_name (to_string expr);
    print_trace tl

let () =
  let ( * ) x y = Prod (x,y) in
  let ( + ) x y = Sum (x,y) in
  let ( ! ) x = Const x in
  let x = Var "x" in
  let y = Var "y" in
  !2. * (!3. * (x + !4. * y) + !(-1.) * x * x + !(-12.) * y)
  |> simplify some_rules
  |> print_trace
