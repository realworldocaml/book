open Core.Std

type t = (string,int) Map.t

let empty = Map.empty

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.add t s (count + 1)

let to_list t = Map.to_alist t
