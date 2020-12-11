open! Import

let failwithf = Printf.failwithf

module T = struct
  include Uchar0

  let module_name = "Base.Uchar"
  let hash_fold_t state t = Hash.fold_int state (to_int t)
  let hash t = Hash.run hash_fold_t t
  let to_string t = Printf.sprintf "U+%04X" (to_int t)

  (* Do not actually export this. See discussion in the .mli *)

  let sexp_of_t t = Sexp.Atom (to_string t)

  let t_of_sexp sexp =
    match sexp with
    | Sexp.List _ -> of_sexp_error "Uchar.t_of_sexp: atom needed" sexp
    | Sexp.Atom s ->
      (try Caml.Scanf.sscanf s "U+%X" (fun i -> Uchar0.of_int i) with
       | _ -> of_sexp_error "Uchar.t_of_sexp: atom of the form U+XXXX needed" sexp)
  ;;
end

include T
include Pretty_printer.Register (T)
include Comparable.Make (T)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Uchar_replace_polymorphic_compare

let invariant (_ : t) = ()
let int_is_scalar = is_valid

let succ_exn c =
  try Uchar0.succ c with
  | Invalid_argument msg -> failwithf "Uchar.succ_exn: %s" msg ()
;;

let succ c =
  try Some (Uchar0.succ c) with
  | Invalid_argument _ -> None
;;

let pred_exn c =
  try Uchar0.pred c with
  | Invalid_argument msg -> failwithf "Uchar.pred_exn: %s" msg ()
;;

let pred c =
  try Some (Uchar0.pred c) with
  | Invalid_argument _ -> None
;;

let of_scalar i = if int_is_scalar i then Some (unsafe_of_int i) else None

let of_scalar_exn i =
  if int_is_scalar i
  then unsafe_of_int i
  else failwithf "Uchar.of_int_exn got a invalid Unicode scalar value: %04X" i ()
;;

let to_scalar t = Uchar0.to_int t
let to_char c = if is_char c then Some (unsafe_to_char c) else None

let to_char_exn c =
  if is_char c
  then unsafe_to_char c
  else failwithf "Uchar.to_char_exn got a non latin-1 character: U+%04X" (to_int c) ()
;;

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Uchar_replace_polymorphic_compare
