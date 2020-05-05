open Base

type t = (string * int) list

let empty = []

let to_list x = x

let touch counts line =
  let count = 
    match List.Assoc.find ~equal:String.equal counts line with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add ~equal:String.equal counts line (count + 1)
