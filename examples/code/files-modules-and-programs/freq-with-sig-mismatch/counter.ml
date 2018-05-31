open Core_kernel

type t = int String.Map.t

let empty = String.Map.empty

let to_list t = String.Map.to_alist t

let touch (t:t) (s:string) : t =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.set t ~key:s ~data:(count + 1)
