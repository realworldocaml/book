open Core_kernel

type t = (string * int) list

let empty = []

let to_list x = x

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.add t ~key:s ~data:(count + 1)
