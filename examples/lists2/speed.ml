open Core.Std
module Bench = Core_extended.Std.Bench
module Test = Bench.Test

let f x =
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

let g x =
  if x = 0 then 1
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

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Bench.bench
      [ Test.create ~name:"match" (fun () -> ignore (f 9))
      ; Test.create ~name:"if"    (fun () -> ignore (g 9))
      ]
  )
