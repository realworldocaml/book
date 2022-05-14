open! Core
module Array = Base.Array
module List = Base.List
module Option = Base.Option
module Sequence = Base.Sequence

module Node = struct
  type 'a t =
    { value : 'a
    ; children : 'a t list
    }
end

open Node

type 'a t =
  { compare : 'a -> 'a -> int
  ; length : int
  ; heap : 'a Node.t option
  }

let create ~compare = { compare; length = 0; heap = None }

let merge
      ~compare
      ({ value = e1; children = nl1 } as n1)
      ({ value = e2; children = nl2 } as n2)
  =
  if compare e1 e2 < 0
  then { value = e1; children = n2 :: nl1 }
  else { value = e2; children = n1 :: nl2 }
;;

let merge_pairs ~compare t =
  let rec loop acc t =
    match t with
    | [] -> acc
    | [ head ] -> head :: acc
    | head :: next1 :: next2 -> loop (merge ~compare head next1 :: acc) next2
  in
  match loop [] t with
  | [] -> None
  | [ h ] -> Some h
  | x :: xs -> Some (List.fold xs ~init:x ~f:(merge ~compare))
;;

let add { compare; length; heap } e =
  let new_node = { value = e; children = [] } in
  let heap =
    match heap with
    | None -> new_node
    | Some heap -> merge ~compare new_node heap
  in
  { compare; length = length + 1; heap = Some heap }
;;

let top_exn t =
  match t.heap with
  | None -> failwith "Fheap.top_exn called on an empty heap"
  | Some { value; _ } -> value
;;

let top t =
  try Some (top_exn t) with
  | _ -> None
;;

let pop_exn { compare; length; heap } =
  match heap with
  | None -> failwith "Heap.pop_exn called on an empty heap"
  | Some { value; children } ->
    let new_heap = merge_pairs ~compare children in
    let t' = { compare; length = length - 1; heap = new_heap } in
    value, t'
;;

let pop t =
  try Some (pop_exn t) with
  | _ -> None
;;

let pop_min = pop
let pop_min_exn = pop_exn

let remove_top t =
  try
    let _, t' = pop_exn t in
    Some t'
  with
  | _ -> None
;;

let pop_if t f =
  match top t with
  | None -> None
  | Some v -> if f v then pop t else None
;;

let fold t ~init ~f =
  let rec loop acc to_visit =
    match to_visit with
    | [] -> acc
    | { value; children } :: rest ->
      let acc = f acc value in
      let to_visit = List.unordered_append children rest in
      loop acc to_visit
  in
  match t.heap with
  | None -> init
  | Some node -> loop init [ node ]
;;

let length t = t.length

module C = Container.Make (struct
    type nonrec 'a t = 'a t

    let fold = fold
    let iter = `Define_using_fold
    let length = `Custom length
  end)

let is_empty t = Option.is_none t.heap
let iter = C.iter
let mem = C.mem
let min_elt = C.min_elt
let max_elt = C.max_elt
let find = C.find
let find_map = C.find_map
let for_all = C.for_all
let exists = C.exists
let sum = C.sum
let count = C.count
let to_list = C.to_list
let fold_result = C.fold_result
let fold_until = C.fold_until

(* We could avoid the intermediate list here, but it doesn't seem like a big deal. *)
let to_array = C.to_array

let of_fold c ~compare fold =
  let h = create ~compare in
  fold c ~init:h ~f:add
;;

let of_list l ~compare = of_fold l ~compare List.fold
let of_array arr ~compare = of_fold arr ~compare Array.fold
let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let to_sequence t = Sequence.unfold ~init:t ~f:pop
