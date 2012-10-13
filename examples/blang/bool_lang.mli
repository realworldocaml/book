type 'a t =
| Base  of 'a
| Const of bool
| And   of 'a t list
| Or    of 'a t list
| Not   of 'a t

val eval : 'a t -> ('a -> bool) -> bool
val simplify : 'a t -> 'a t
val specialize : 'a t -> f:('a -> bool option) -> 'a t
