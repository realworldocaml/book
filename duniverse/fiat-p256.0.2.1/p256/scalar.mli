type t
(** A scalar value strictly between 1 and n-1 where n is the group order. *)

val of_cstruct : Cstruct.t -> (t, Error.scalar_error) result
(** [of_cstruct cs] is a scalar {!t} when successful.
    It should be in big endian format.
    Returns an [Error _] when [cs] is not 32 bytes long;
    or when the number is zero;
    or if it is larger than or equal to the group order.
*)

val bit_at : t -> int -> bool
(** [bit_at d n] returns the [n]th bit from [d], where bit 0 is the least
    significant bit. *)
