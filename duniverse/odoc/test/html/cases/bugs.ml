type 'a opt = 'a option
let foo (type a) ?(bar : a opt) () = ()
(** Triggers an assertion failure when
    {:https://github.com/ocaml/odoc/issues/101} is not fixed. *)

