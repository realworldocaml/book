
open Astring

(* Total *)
let find_all p s =
  let rec loop acc i = match String.find ~start:i p s with
  | None -> List.rev acc
  | Some i -> loop (i :: acc) (i + 1)
  in
  loop [] 0

(* Not total *)
let find_all p s =
  let rec loop acc i =
    if i > String.length s then List.rev acc else
    match String.find ~start:i p s with
    | None -> List.rev acc
    | Some i -> loop (i :: acc) (i + 1)
  in
  loop [] 0
