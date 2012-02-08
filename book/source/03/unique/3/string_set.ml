open Core.Std

let empty = []
let add l x =
   if List.mem l x then l else x :: l
let mem = List.mem
