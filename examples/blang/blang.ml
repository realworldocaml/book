open Core.Std

type 'a t =
| Base  of 'a
| Const of bool
| And   of 'a t list
| Or    of 'a t list
| Not   of 'a t
with sexp

let rec eval t base_eval =
  match t with
  | Base base -> base_eval base
  | Const bool -> bool
  | And ts ->
    List.for_all ts ~f:(fun t -> eval t base_eval)
  | Or ts ->
    List.exists ts ~f:(fun t -> eval t base_eval)
  | Not t -> not (eval t base_eval)

let base x = Base x
let and_ l = And l
let or_ l  = Or l
let not l  = Not l
let true_  = Const true
let false_ = Const false

let rec simplify =
  | Base _ | Const _ as x -> x
  | And ts ->
    if List.exists ts ~f:(function Const false -> true | _ -> false)
    then Const false
    else And ts
  | Or ts ->
    if List.exists ts ~f:(function Const true -> true | _ -> false)

