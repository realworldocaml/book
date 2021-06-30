(* This is broken off to avoid circular dependency between Sign and Comparable. *)

open! Import

type t =
  | Neg
  | Zero
  | Pos
[@@deriving_inline sexp, compare, hash, enumerate]

let t_of_sexp =
  (let _tp_loc = "sign0.ml.t" in
   function
   | Ppx_sexp_conv_lib.Sexp.Atom ("neg" | "Neg") -> Neg
   | Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") -> Zero
   | Ppx_sexp_conv_lib.Sexp.Atom ("pos" | "Pos") -> Pos
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("neg" | "Neg") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("pos" | "Pos") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
     Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
   | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
             : Ppx_sexp_conv_lib.Sexp.t -> t)
;;

let sexp_of_t =
  (function
    | Neg -> Ppx_sexp_conv_lib.Sexp.Atom "Neg"
    | Zero -> Ppx_sexp_conv_lib.Sexp.Atom "Zero"
    | Pos -> Ppx_sexp_conv_lib.Sexp.Atom "Pos"
             : t -> Ppx_sexp_conv_lib.Sexp.t)
;;

let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)

let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv arg ->
     match arg with
     | Neg -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
     | Zero -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
     | Pos -> Ppx_hash_lib.Std.Hash.fold_int hsv 2
              : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)
;;

let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create () in
       hash_fold_t hsv arg)
  in
  fun x -> func x
;;

let all = ([ Neg; Zero; Pos ] : t list)

[@@@end]

module Replace_polymorphic_compare = struct
  let ( < ) (x : t) y = Poly.( < ) x y
  let ( <= ) (x : t) y = Poly.( <= ) x y
  let ( <> ) (x : t) y = Poly.( <> ) x y
  let ( = ) (x : t) y = Poly.( = ) x y
  let ( > ) (x : t) y = Poly.( > ) x y
  let ( >= ) (x : t) y = Poly.( >= ) x y
  let ascending (x : t) y = Poly.ascending x y
  let descending (x : t) y = Poly.descending x y
  let compare (x : t) y = Poly.compare x y
  let equal (x : t) y = Poly.equal x y
  let max (x : t) y = if x >= y then x else y
  let min (x : t) y = if x <= y then x else y
end

let of_string s = t_of_sexp (sexp_of_string s)
let to_string t = string_of_sexp (sexp_of_t t)

let to_int = function
  | Neg -> -1
  | Zero -> 0
  | Pos -> 1
;;

let _ = hash

(* Ignore the hash function produced by [@@deriving_inline hash] *)
let hash = to_int
let module_name = "Base.Sign"
let of_int n = if n < 0 then Neg else if n = 0 then Zero else Pos
