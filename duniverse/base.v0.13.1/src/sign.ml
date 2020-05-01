open! Import
include Sign0
include Identifiable.Make (Sign0)

(* Open [Replace_polymorphic_compare] after including functor applications so
   they do not shadow its definitions. This is here so that efficient versions
   of the comparison functions are available within this module. *)
open! Replace_polymorphic_compare

let to_float = function
  | Neg -> -1.
  | Zero -> 0.
  | Pos -> 1.
;;

let flip = function
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg
;;

let ( * ) t t' = of_int (to_int t * to_int t')

(* Include type-specific [Replace_polymorphic_compare at the end, after any
   functor applications that could shadow its definitions. This is here so
   that efficient versions of the comparison functions are exported by this
   module. *)
include Replace_polymorphic_compare
