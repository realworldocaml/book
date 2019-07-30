open! Import

include List0

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
      match f x with
      | `Fst y -> loop t (y :: fst) snd
      | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
;;
