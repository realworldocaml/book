open! Import

(** Maps type variables to patterns and expressions for gensym'd variables. Used to handle
    type parameters in polymorphic type definitions. *)
type t

val empty : t
val lookup : t -> loc:location -> tyvar:string -> expression

val create
  :  loc:location
  -> prefix:string
  -> (core_type * (variance * injectivity)) list
  -> pattern list * t

(** For generators, we want contravariant type parameters to map to observer names. For
    observers, vice versa. So we create both environment simultaneously. *)
val create_with_variance
  :  loc:location
  -> covariant:string
  -> contravariant:string
  -> (core_type * (variance * injectivity)) list
  -> pattern list * [ `Covariant of t ] * [ `Contravariant of t ]
