open Core.Std

type t = int String.Map.t

let empty = String.Map.empty

let touch t s =
  let count =
    match String.Map.find t s with
    | None -> 0
    | Some x -> x
  in
  String.Map.add t s (count + 1)

let to_list t = String.Map.to_alist t
