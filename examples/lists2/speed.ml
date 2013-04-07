open Core.Std
module Bench = Core_extended.Std.Bench
module Test = Bench.Test

let num_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 6
  | 6 -> 7
  | 7 -> 8
  | 8 -> 9
  | 9 -> 10
  | _ -> 11

let num_if x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else if x = 4 then 5
  else if x = 5 then 6
  else if x = 6 then 7
  else if x = 7 then 8
  else if x = 8 then 9
  else if x = 9 then 10
  else 11

let pos_match l =
  match l with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: x :: _ -> x
  | _ :: x :: _ -> x
  | x :: _ -> x
  | [] -> 0

let pos_if l =
  if      List.length l > 7 then List.nth_exn l 7
  else if List.length l > 6 then List.nth_exn l 6
  else if List.length l > 5 then List.nth_exn l 5
  else if List.length l > 4 then List.nth_exn l 4
  else if List.length l > 3 then List.nth_exn l 3
  else if List.length l > 2 then List.nth_exn l 2
  else if List.length l > 1 then List.nth_exn l 1
  else if List.length l > 6 then List.nth_exn l 0
  else 0

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Bench.bench
      [ Test.create ~name:"num match" (fun () -> ignore (num_match 9))
      ; Test.create ~name:"num if"    (fun () -> ignore (num_if 9))
      ; Test.create ~name:"pos match" (fun () -> assert (1 = pos_match [0;0;0;0;0;0;1]))
      ; Test.create ~name:"pos if"    (fun () -> assert (1 = pos_if [0;0;0;0;0;0;1]))
      ]
  )
