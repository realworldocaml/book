open! Import
include List0

let is_empty = function
  | [] -> true
  | _ -> false
;;

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> rev fst, rev snd
    | x :: t ->
      (match (f x : _ Either0.t) with
       | First y -> loop t (y :: fst) snd
       | Second y -> loop t fst (y :: snd))
  in
  loop t [] []
;;
