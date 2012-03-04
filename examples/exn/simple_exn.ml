(* simple_exn.ml *)

open Core.Std

exception Empty_list

let list_max l =
  match l with
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:Int.max


let () =
  let print_max l =
    try printf "%d\n" (list_max l)
    with Empty_list -> printf "Empty list\n"
  in
  print_max [1;2;3];
  print_max [];
  print_max [-1;0;1]

(*
let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max []);
  printf "%d\n" (list_max [-1;0;1])
*)
