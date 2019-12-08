open! Base

type -'a t

val opaque : _ t
val create : ('a -> size:int -> hash:Hash.state -> Hash.state) -> 'a t
val observe : 'a t -> 'a -> size:int -> hash:Hash.state -> Hash.state
