val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
