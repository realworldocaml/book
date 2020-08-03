(** Runtime support for auto-generated comparators.  Users are not intended to use this
    module directly. *)

val phys_equal : 'a -> 'a -> bool

(*_ /!\ WARNING /!\ all these functions need to declared "external" in order to get the
  lazy behavior for ( && ) (relied upon by [@@deriving equal]) and the type-based
  specialization for equal/compare. *)

external polymorphic_compare : 'a -> 'a -> int = "%compare"
external polymorphic_equal : 'a -> 'a -> bool = "%equal"
external ( && ) : bool -> bool -> bool = "%sequand"

type 'a compare = 'a -> 'a -> int
type 'a equal = 'a -> 'a -> bool

(** Raise when fully applied *)
val compare_abstract : type_name:string -> _ compare

val equal_abstract : type_name:string -> _ equal

module Builtin : sig
  val compare_bool : bool compare
  val compare_char : char compare
  val compare_float : float compare
  val compare_int : int compare
  val compare_int32 : int32 compare
  val compare_int64 : int64 compare
  val compare_nativeint : nativeint compare
  val compare_string : string compare
  val compare_unit : unit compare
  val compare_array : 'a compare -> 'a array compare
  val compare_list : 'a compare -> 'a list compare
  val compare_option : 'a compare -> 'a option compare
  val compare_ref : 'a compare -> 'a ref compare
  val equal_bool : bool equal
  val equal_char : char equal
  val equal_float : float equal
  val equal_int : int equal
  val equal_int32 : int32 equal
  val equal_int64 : int64 equal
  val equal_nativeint : nativeint equal
  val equal_string : string equal
  val equal_unit : unit equal
  val equal_array : 'a equal -> 'a array equal
  val equal_list : 'a equal -> 'a list equal
  val equal_option : 'a equal -> 'a option equal
  val equal_ref : 'a equal -> 'a ref equal
end
