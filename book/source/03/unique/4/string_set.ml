open Core.Std

type t = string list

let empty = []
let add l x =
   if List.mem l x then l else x :: l
let mem t l = List.mem t l
