open Core

(** An object for bounding the rate of consumption of some resource by a bound
    over a rolling window. *)

type t

val create : now:Time_ns.t -> period:Time_ns.Span.t -> rate:int -> t
val maybe_consume : t -> now:Time_ns.t -> [ `Consumed | `No_capacity ]
