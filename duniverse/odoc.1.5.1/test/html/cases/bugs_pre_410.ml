type 'a opt' = int option
let foo' (type a) ?(bar : a opt') () = ()
(** Similar to [Bugs], but the printed type of [~bar] should be [int], not
    ['a]. This probably requires fixing in the compiler. See
    {:https://github.com/ocaml/odoc/pull/230#issuecomment-433226807}. *)

