open! Base
open! Ppxlib

(** Sexp conversion function, expressed as either a single expression or as a collection
    of [match] cases. Expressing as cases rather than wrapping directly in [pexp_function]
    allows us to simplify some expressions built on this. *)
type t

(** Construct [t] from a list of pattern/expression cases. *)
val of_lambda : cases -> t

(** Construct [t] from an identifier, possibly applied to arguments. Raise on any other
    form of expression. *)
val of_reference_exn : expression -> t

(** Convert [t] to an expression. *)
val to_expression : t -> loc:location -> expression

(** Convert [t] to an expression that is a syntactic value, i.e. a constant, identifier,
    or lambda expression that does no "work", can can be preallocated, and works in the
    context of a [let rec]. *)
val to_value_expression : t -> loc:location -> expression

(** Apply [t] to an argument. *)
val apply
  :  t
  -> loc:location
  -> expression (** argument [t] is applied to *)
  -> expression

(** Wrap [t] in [let]-bindings. *)
val bind : t -> value_binding list -> t

module Apply_all : sig
  type t =
    { bindings : value_binding list
    ; arguments : pattern list
    ; converted : expression list
    }
end

(** Applies each [t] to a fresh variable, and binds the results to fresh variables.
    Returns the corresponding [value_binding]s, patterns for the argument variables, and
    expressions for the result variables. *)
val apply_all : t list -> loc:location -> Apply_all.t
