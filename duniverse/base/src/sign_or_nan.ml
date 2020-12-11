open! Import

module T = struct
  type t =
    | Neg
    | Zero
    | Pos
    | Nan
  [@@deriving_inline sexp, compare, hash, enumerate]

  let t_of_sexp =
    (let _tp_loc = "sign_or_nan.ml.T.t" in
     function
     | Ppx_sexp_conv_lib.Sexp.Atom ("neg" | "Neg") -> Neg
     | Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") -> Zero
     | Ppx_sexp_conv_lib.Sexp.Atom ("pos" | "Pos") -> Pos
     | Ppx_sexp_conv_lib.Sexp.Atom ("nan" | "Nan") -> Nan
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("neg" | "Neg") :: _) as
       sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") :: _)
       as sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("pos" | "Pos") :: _) as
       sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("nan" | "Nan") :: _) as
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
      | Nan -> Ppx_sexp_conv_lib.Sexp.Atom "Nan"
               : t -> Ppx_sexp_conv_lib.Sexp.t)
  ;;

  let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    (fun hsv arg ->
       match arg with
       | Neg -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
       | Zero -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
       | Pos -> Ppx_hash_lib.Std.Hash.fold_int hsv 2
       | Nan -> Ppx_hash_lib.Std.Hash.fold_int hsv 3
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

  let all = ([ Neg; Zero; Pos; Nan ] : t list)

  [@@@end]

  let of_string s = t_of_sexp (sexp_of_string s)
  let to_string t = string_of_sexp (sexp_of_t t)
  let module_name = "Base.Sign_or_nan"
end

module Replace_polymorphic_compare = struct
  let ( < ) (x : T.t) y = Poly.( < ) x y
  let ( <= ) (x : T.t) y = Poly.( <= ) x y
  let ( <> ) (x : T.t) y = Poly.( <> ) x y
  let ( = ) (x : T.t) y = Poly.( = ) x y
  let ( > ) (x : T.t) y = Poly.( > ) x y
  let ( >= ) (x : T.t) y = Poly.( >= ) x y
  let ascending (x : T.t) y = Poly.ascending x y
  let descending (x : T.t) y = Poly.descending x y
  let compare (x : T.t) y = Poly.compare x y
  let equal (x : T.t) y = Poly.equal x y
  let max (x : T.t) y = if x >= y then x else y
  let min (x : T.t) y = if x <= y then x else y
end

include T
include Identifiable.Make (T)

(* Open [Replace_polymorphic_compare] after including functor applications so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Replace_polymorphic_compare

let of_sign = function
  | Sign.Neg -> Neg
  | Sign.Zero -> Zero
  | Sign.Pos -> Pos
;;

let to_sign_exn = function
  | Neg -> Sign.Neg
  | Zero -> Sign.Zero
  | Pos -> Sign.Pos
  | Nan -> invalid_arg "Base.Sign_or_nan.to_sign_exn: Nan"
;;

let of_int n = of_sign (Sign.of_int n)
let to_int_exn t = Sign.to_int (to_sign_exn t)

let flip = function
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg
  | Nan -> Nan
;;

let ( * ) t t' =
  match t, t' with
  | Nan, _ | _, Nan -> Nan
  | _ -> of_sign (Sign.( * ) (to_sign_exn t) (to_sign_exn t'))
;;

(* Include [Replace_polymorphic_compare] at the end, after any functor applications that
   could shadow its definitions. This is here so that efficient versions of the comparison
   functions are exported by this module. *)
include Replace_polymorphic_compare
