open! Import
open! Ordering

let%test _ = equal (of_int (-10)) Less
let%test _ = equal (of_int (-1) ) Less
let%test _ = equal (of_int 0    ) Equal
let%test _ = equal (of_int 1    ) Greater
let%test _ = equal (of_int 10   ) Greater

let%test _ = equal (of_int (Int.compare 0 1)) Less
let%test _ = equal (of_int (Int.compare 1 1)) Equal
let%test _ = equal (of_int (Int.compare 1 0)) Greater

let%test _ = List.for_all all ~f:(fun t -> equal t (t |> to_int |> of_int))
let%test _ = List.for_all [ -1; 0; 1 ] ~f:(fun i -> i = (i |> of_int |> to_int))
