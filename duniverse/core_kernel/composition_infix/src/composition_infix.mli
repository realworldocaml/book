(** Infix composition operators.

    - [ a |> (f >> g) = a |> f |> g ]
    - [ (f << g) a = f (g a) ] *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
