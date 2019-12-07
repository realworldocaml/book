(** type-safe runtime type introspection *)

(** runtime type representations *)
module rec Typerep : sig

  type _ t =
  | Int        : int t
  | Int32      : int32 t
  | Int64      : int64 t
  | Nativeint  : nativeint t
  | Char       : char t
  | Float      : float t
  | String     : string t
  | Bytes      : bytes t
  | Bool       : bool t
  | Unit       : unit t
  | Option     : 'a t -> 'a option t
  | List       : 'a t -> 'a list t
  | Array      : 'a t -> 'a array t
  | Lazy       : 'a t -> 'a lazy_t t
  | Ref        : 'a t -> 'a ref t
  | Function   : ('dom t * 'rng t) -> ('dom -> 'rng) t
  | Tuple      : 'a Typerep.Tuple.t -> 'a t
  | Record     : 'a Typerep.Record.t -> 'a t
  | Variant    : 'a Typerep.Variant.t -> 'a t
    (** The [Named] constructor both allows for custom implementations of generics
        based on name and provides a way to represent recursive types, the lazy
        part dealing with cycles *)
  | Named      : ('a Typerep.Named.t * 'a t lazy_t option) -> 'a t

  type packed = T : 'a t -> packed

  module Named : sig
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Typerep.t
      type t
      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type e val e : e Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
    val name : _ t -> string
  end

  module Tuple : sig
    type _ t =
    | T2 : ('a Typerep.t * 'b Typerep.t)
      -> ('a * 'b) t
    | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
  end

  include Variant_and_record_intf.S with type 'a t := 'a t

  (** [same t t'] will return a proof a equality if [t] and [t'] are the same type.
      One can think of two types being the [same] as two types whose values could be for
      example put in a list together.
      It is worth noting that this function *does not* operate compatiblity diffs between
      two different types with the same structure. Example:
      {[
        module M1 = struct
          type t = {
            a : int;
            b : float;
          } with typerep
        end
        module M2 = struct
          type t = {
            a : int;
            b : float;
          } with typerep
        end
        TEST = not (same M1.typerep_of_t M2.typerep_of_t)

        type a = int with typerep
        type b = int with typerep
        TEST = same typerep_of_a typerep_of_b
      ]}
      This is meant to recover type equality hidden by existential constructors.

      Basically this function does structural equality for everything except variant
      types, record types, and named types with no lazy definition exposed. This last case
      is about types that are defined [with typerep(abstract)]
  *)
  val same : _ t -> _ t -> bool
  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val typename_of_t : 'a t -> 'a Typename.t

  (** [head ty] is used to traverse the [Named] constructor. It might be used when one
      care to pattern match directly on the representation in a low level way rather than
      going through a full generic. [head t] is [t] if [t] is not of the form [Named _] *)
  val head : 'a t -> 'a t
end

(* basic *)
val typerep_of_int        : int       Typerep.t
val typerep_of_int32      : int32     Typerep.t
val typerep_of_int64      : int64     Typerep.t
val typerep_of_nativeint  : nativeint Typerep.t
val typerep_of_char       : char      Typerep.t
val typerep_of_float      : float     Typerep.t
val typerep_of_string     : string    Typerep.t
val typerep_of_bytes      : bytes     Typerep.t
val typerep_of_bool       : bool      Typerep.t
val typerep_of_unit       : unit      Typerep.t

(* variant with no argument *)
type tuple0
val value_tuple0 : tuple0

(* nested *)
val typerep_of_option    : 'a Typerep.t -> 'a option Typerep.t
val typerep_of_list      : 'a Typerep.t -> 'a list Typerep.t
val typerep_of_array     : 'a Typerep.t -> 'a array Typerep.t
val typerep_of_lazy_t    : 'a Typerep.t -> 'a lazy_t Typerep.t
val typerep_of_ref       : 'a Typerep.t -> 'a ref Typerep.t
val typerep_of_function  : 'a Typerep.t -> 'b Typerep.t -> ('a -> 'b) Typerep.t
val typerep_of_tuple0    : tuple0 Typerep.t
val typerep_of_tuple2 :
  'a Typerep.t -> 'b Typerep.t
  -> ('a * 'b) Typerep.t
val typerep_of_tuple3 :
  'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t
  -> ('a * 'b * 'c) Typerep.t
val typerep_of_tuple4 :
  'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t
  -> ('a * 'b * 'c * 'd) Typerep.t
val typerep_of_tuple5 :
  'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t -> 'e Typerep.t
  -> ('a * 'b * 'c * 'd * 'e) Typerep.t

val typename_of_int        : int       Typename.t
val typename_of_int32      : int32     Typename.t
val typename_of_int64      : int64     Typename.t
val typename_of_nativeint  : nativeint Typename.t
val typename_of_char       : char      Typename.t
val typename_of_float      : float     Typename.t
val typename_of_string     : string    Typename.t
val typename_of_bytes      : bytes     Typename.t
val typename_of_bool       : bool      Typename.t
val typename_of_unit       : unit      Typename.t

val typename_of_option   : 'a Typename.t -> 'a option Typename.t
val typename_of_list     : 'a Typename.t -> 'a list Typename.t
val typename_of_array    : 'a Typename.t -> 'a array Typename.t
val typename_of_lazy_t   : 'a Typename.t -> 'a lazy_t Typename.t
val typename_of_ref      : 'a Typename.t -> 'a ref Typename.t
val typename_of_function : 'a Typename.t -> 'b Typename.t -> ('a -> 'b) Typename.t
val typename_of_tuple0   : tuple0 Typename.t
val typename_of_tuple2 :
  'a Typename.t -> 'b Typename.t
  -> ('a * 'b) Typename.t
val typename_of_tuple3 :
  'a Typename.t -> 'b Typename.t -> 'c Typename.t
  -> ('a * 'b * 'c) Typename.t
val typename_of_tuple4 :
  'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t
  -> ('a * 'b * 'c * 'd) Typename.t
val typename_of_tuple5 :
  'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t -> 'e Typename.t
  -> ('a * 'b * 'c * 'd * 'e) Typename.t
