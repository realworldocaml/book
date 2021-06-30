open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving_inline compare, hash, enumerate, sexp]

let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)

let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv arg ->
     match arg with
     | Less -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
     | Equal -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
     | Greater -> Ppx_hash_lib.Std.Hash.fold_int hsv 2
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

let all = ([ Less; Equal; Greater ] : t list)

let t_of_sexp =
  (let _tp_loc = "ordering.ml.t" in
   function
   | Ppx_sexp_conv_lib.Sexp.Atom ("less" | "Less") -> Less
   | Ppx_sexp_conv_lib.Sexp.Atom ("equal" | "Equal") -> Equal
   | Ppx_sexp_conv_lib.Sexp.Atom ("greater" | "Greater") -> Greater
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("less" | "Less") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("equal" | "Equal") :: _)
     as sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom ("greater" | "Greater") :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
     Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
   | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
             : Ppx_sexp_conv_lib.Sexp.t -> t)
;;

let sexp_of_t =
  (function
    | Less -> Ppx_sexp_conv_lib.Sexp.Atom "Less"
    | Equal -> Ppx_sexp_conv_lib.Sexp.Atom "Equal"
    | Greater -> Ppx_sexp_conv_lib.Sexp.Atom "Greater"
                 : t -> Ppx_sexp_conv_lib.Sexp.t)
;;

[@@@end]

let equal a b = compare a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n = if n < 0 then Less else if n = 0 then Equal else Greater

let to_int = function
  | Less -> -1
  | Equal -> 0
  | Greater -> 1
;;
