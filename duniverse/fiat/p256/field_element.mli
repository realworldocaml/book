(** Field elements *)

type t
(** A field element. This corresponds to a mutable chunk of memory.
    Most operations are done in Montgomery form. This means that the internal
    value is multiplied by a constant [R = 2**256].

    @see <https://en.wikipedia.org/wiki/Montgomery_modular_multiplication>
*)

val create : unit -> t
(** Allocate a new element, corresponding to zero. *)

val one : unit -> t
(** Allocate a new element, corresponding to one. *)

val from_bytes : t -> Cstruct.t -> unit
(** [from_bytes dst src] deserializes a field element in the Montgomery domain
    from bytes in little-endian order. *)

val to_bytes : Cstruct.t -> t -> unit
(** [to_bytes dst src] serializes a field element in the Montgomery domain to
    bytes in little-endian order. *)

val from_montgomery : t -> unit
(** Translate a field element out of the Montgomery domain.
    That is to say, it divides by [R] in place. *)

val copy : t -> t -> unit
(** [copy dst src] sets [dst] to [src]. *)

val add : t -> t -> t -> unit
(** [add dst a b] adds [a] and [b] and stores the result in [dst].
    Due to how Montgomery form works, it works both in and out of the Montgomery
    domain. *)

val sub : t -> t -> t -> unit
(** [sub dst a b] subtracts [a] and [b] and stores the result in [dst].
    Due to how Montgomery form works, it works both in and out of the Montgomery
    domain. *)

val mul : t -> t -> t -> unit
(** [mul dst a b] multiplies [a] and [b] (in the Montgomery domain) and stores
    the result in [dst]. *)

val nz : t -> bool
(** [nz n] returns [true] if [n] is non-zero. *)

val sqr : t -> t -> unit
(** [sqr dst src] squares [src] in the Montgomery domain and stores the result
    in [dst]. *)

val inv : t -> t -> unit
(** [inv dst src] computes the modular inverse of [src] and stores the result in
    [dst]. *)

val from_be_cstruct : Cstruct.t -> t
(** Converts from a big-endian cstruct to Montgomery form. *)

val select : bool -> then_:t -> else_:t -> t
(** Constant-time selection.
    [select true ~then_ ~false_] returns a copy of [then_], and
    [select false ~then_ ~false_] returns a copy of [else_]. *)
