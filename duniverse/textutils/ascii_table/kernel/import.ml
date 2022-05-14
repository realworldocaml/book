open! Core
module Attr = Ansi_kernel.Attr

let list_sum l ~f = List.sum (module Int) l ~f
let list_max ~f lst = List.fold lst ~init:0 ~f:(fun a b -> max a (f b))
