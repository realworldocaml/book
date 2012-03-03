(* simple_exn.ml *)

open Core.Std

exception Empty_list

let list_max l =
  match l with
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(+)


let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
