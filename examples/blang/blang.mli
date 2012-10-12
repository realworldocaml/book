type 'a t with sexp

val base   : 'a -> 'a t
val true_  : _ t
val false_ : _ t
val and_   : 'a t list -> 'a t
val or_    : 'a t list -> 'a t
val not    : 'a t -> 'a t

val eval : 'a t -> ('a -> bool) -> bool
