open Core.Std

type 'a t =
| Base  of 'a
| Const of bool
| And   of 'a t list
| Or    of 'a t list
| Not   of 'a t

let rec eval t base_eval =
  let eval' t = eval t base_eval in
  match t with
  | Base  base -> base_eval base
  | Const bool -> bool
  | And   ts   -> List.for_all ts ~f:eval'
  | Or    ts   -> List.exists  ts ~f:eval'
  | Not   t    -> not (eval' t)

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And ts ->
    let ts = List.map ~f:simplify ts in
    if List.exists ts ~f:(function Const false -> true | _ -> false)
    then Const false
    else And ts
  | Or ts ->
    let ts = List.map ~f:simplify ts in
    if List.exists ts ~f:(function Const true -> true | _ -> false)
    then Const true else Or ts
  | Not t ->
    match simplify t with
    | Const b -> Const (not b)
    | Not t -> t
    | (And _ | Or _ | Base _) as t -> Not t

let rec specialize t ~f =
  let specialize' t = specialize t ~f in
  let specialized =
    match t with
    | Base base ->
      begin match f base with
      | Some bool -> Const bool
      | None      -> Base  base
      end
    | Not t  -> Not (specialize' t)
    | And ts -> And (List.map ~f:specialize' ts)
    | Or  ts -> Or  (List.map ~f:specialize' ts)
    | Const x -> Const x
  in
  simplify specialized
