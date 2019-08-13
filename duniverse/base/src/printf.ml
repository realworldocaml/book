open! Import0

include Caml.Printf

(** failwith, invalid_arg, and exit accepting printf's format. *)

let failwithf    fmt = ksprintf (fun s () -> failwith    s) fmt
let invalid_argf fmt = ksprintf (fun s () -> invalid_arg s) fmt
