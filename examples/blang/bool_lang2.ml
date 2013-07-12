open Core.Std

type 'a expr =
  | Base  of 'a
  | Const of bool
  | And   of 'a expr list
  | Or    of 'a expr list
  | Not   of 'a expr
with sexp

let rec eval t base_eval =
  let eval' t = eval t base_eval in
  match t with
  | Base  base -> base_eval base
  | Const bool -> bool
  | And   ts   -> List.for_all ts ~f:eval'
  | Or    ts   -> List.exists  ts ~f:eval'
  | Not   t    -> not (eval' t)

let and_ l =
  if List.mem l (Const false) then Const false
  else
    match List.filter l ~f:((<>) (Const true)) with
    | [] -> Const true
    | [ x ] -> x
    | l -> And l

let or_ l =
  if List.mem l (Const true) then Const true
  else
    match List.filter l ~f:((<>) (Const false)) with
    | [] -> Const false
    | [x] -> x
    | l -> Or l

let not_ = function
  | Const b -> Const (not b)
  (* | e -> Not e *)
  | Not e -> e
  | (Base _ | And _ | Or _) as e -> Not e

let rec simplify = function
  | Base _ | Const _ as x -> x
  | Not e -> not_ (simplify e)
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_  (List.map ~f:simplify l)


let ex e =
  let print e =
    printf "%s\n" (e |> sexp_of_expr sexp_of_string |> Sexp.to_string_hum)
  in
  printf "------------\n";
  print e;
  print (simplify e)

let () =
  ex (Not (And [ Or [Base "it's snowing"; Const true]
               ; Base "it's raining"]));
  ex (Not (And [ Or [Base "it's snowing"; Const true]
               ; Not (Not (Base "it's raining"))]))
