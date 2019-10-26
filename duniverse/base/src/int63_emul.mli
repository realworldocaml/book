open! Import

include Int_intf.S

val of_int : int -> t
val to_int : t -> int option
val to_int_trunc : t -> int

val of_int32 : int32 -> t
val to_int32 : t -> Int32.t option
val to_int32_trunc : t -> Int32.t

val of_int64 : Int64.t -> t option
val of_int64_trunc : Int64.t -> t

val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint option
val of_nativeint_trunc : nativeint -> t
val to_nativeint_trunc : t -> nativeint

(*_ exported for Core_kernel *)
module W : sig
  val wrap_exn : int64 -> t
  val unwrap : t -> int64
end

module Repr : sig
  type emulated = t
  type ('underlying_type, 'intermediate_type) t =
    | Int   : (int   , int     ) t
    | Int64 : (int64 , emulated) t
end with type emulated := t

val repr : (t, t) Repr.t
