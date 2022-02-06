(** Generate expressions in a hygienic way.

    The idea is that whenever we want to refer to an expression in generated
    code we first quote it. The result will be an identifier that is guaranteed
    to refer to the expression it was created from. This way it is impossible
    for quoted fragments to refer to newly introduced expressions. *)

open Import

type t

val create : unit -> t
(** Creates a quoter. A quoter guarantees to give names that do not clash with
    any other names used before *)

val quote : t -> expression -> expression
(** [quote t e] returns the expression that is safe to use in place of [e] in
    generated code*)

val sanitize : t -> expression -> expression
(** [sanitize t e] Returns [e] wrapped with bindings for all quoted expressions
    in the quoter [t] *)
