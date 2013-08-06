open Core.Std

type 'a expr =
| Base  of 'a
| Const of bool
| And   of 'a expr list
| Or    of 'a expr list
| Not   of 'a expr

let rec eval t base_eval =
  let eval' t = eval t base_eval in
  match t with
  | Base  base -> base_eval base
  | Const bool -> bool
  | And   ts   -> List.for_all ts ~f:eval'
  | Or    ts   -> List.exists  ts ~f:eval'
  | Not   t    -> not (eval' t)

let and_ l =
  if List.mem l (Const false) then Const false else And l

let or_ l =
  if List.mem l (Const true) then Const true else Or l

let not_ = function
  | Const b -> Const (not b)
  | expr -> expr

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_  (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)
