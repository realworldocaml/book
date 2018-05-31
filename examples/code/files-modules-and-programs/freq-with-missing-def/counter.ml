open Core_kernel
type t = (string*int) list
let empty = []
let touch t s = 
  let equal = String.equal in
  let count = match List.Assoc.find ~equal t s  with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count+1) ~equal
let to_list x:t = x
