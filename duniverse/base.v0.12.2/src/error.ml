(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Error] and [Or_error] to be used in various places.  Please avoid adding new
   dependencies. *)

open! Import

include Info

let raise t = raise (to_exn t)

let raise_s sexp = raise (create_s sexp)

let to_info t = t
let of_info t = t

include Pretty_printer.Register_pp(struct
    type nonrec t = t
    let module_name = "Base.Error"
    let pp = pp
  end)
