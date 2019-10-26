open! Core_kernel
open! Rope

let%bench "small on the right" =
  let rec go acc = function
    | 0 -> acc
    | n -> go (acc ^ of_string "bla") (n - 1)
  in
  to_string (go (of_string "") 2048)
;;

let%bench "balanced" =
  let rec go = function
    | 0 -> of_string "bla"
    | n -> go (n - 1) ^ go (n - 1)
  in
  to_string (go 11)
;;
