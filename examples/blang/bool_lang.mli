type 'a expr =
| Base  of 'a
| Const of bool
| And   of 'a expr list
| Or    of 'a expr list
| Not   of 'a expr

val eval : 'a expr -> ('a -> bool) -> bool
val simplify : 'a expr -> 'a expr


