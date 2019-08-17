open! Core_kernel

exception E of int [@@deriving sexp]

let () = raise (E 42)
