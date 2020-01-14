open Sexplib.Sexp
open Sexplib.Conv

let exn_to_string e = to_string_hum (sexp_of_exn e)

let sexp_of_big_int n = Atom (Big_int.string_of_big_int n)
let sexp_of_nat n = Atom (Nat.string_of_nat n)
let sexp_of_ratio n = Atom (Ratio.string_of_ratio n)
let sexp_of_num n = Atom (Num.string_of_num n)

let big_int_of_sexp sexp = match sexp with
  | Atom str ->
    (try Big_int.big_int_of_string str
     with exc ->
       of_sexp_error ("big_int_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "big_int_of_sexp: atom needed" sexp

let nat_of_sexp sexp = match sexp with
  | Atom str ->
    (try Nat.nat_of_string str
     with exc ->
       of_sexp_error ("nat_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "nat_of_sexp: atom needed" sexp

let ratio_of_sexp sexp = match sexp with
  | Atom str ->
    (try Ratio.ratio_of_string str
     with exc ->
       of_sexp_error ("ratio_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "ratio_of_sexp: atom needed" sexp

let num_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Num.num_of_string str
     with exc ->
       of_sexp_error ("num_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "num_of_sexp: atom needed" sexp
