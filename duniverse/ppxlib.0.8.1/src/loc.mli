(** Located items *)

open! Import

type 'a t = 'a loc =
  { txt : 'a
  ; loc : Location.t
  }

val txt : 'a t -> 'a
val loc : _ t -> Location.t

val make : loc:Location.t -> 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t
