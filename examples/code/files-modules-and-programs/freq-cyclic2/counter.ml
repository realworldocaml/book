open Base

type t = int Map.M(String).t

let empty = Map.empty (module String)

let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.set t ~key:s ~data:(count + 1)

[@@@part "1"]
let _build_counts = Freq.build_counts
