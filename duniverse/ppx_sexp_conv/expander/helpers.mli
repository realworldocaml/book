open! Base
open! Ppxlib

(** Constructs a branch of a [match] or [function] expression with no guard. *)
val ( --> ) : pattern -> expression -> case

(** Replace all type variables like ['a] with wildcard ([_]) types. *)
val replace_variables_by_underscores : core_type -> core_type

(** Create a binding for a derived function, adding a type annotation if required. *)
val constrained_function_binding
  :  location (** location to use for the binding *)
  -> type_declaration (** type declaration used to derive the function *)
  -> core_type (** type of the function *)
  -> tps:string loc list (** names of type parameters in the declaration *)
  -> func_name:string (** name to bind the function to *)
  -> expression (** expression representing the function *)
  -> value_binding

(** Wraps an expression in layers of non-recursive [let] bindings, with the bindings
    sorted from outermost to innermost. *)
val with_let : loc:location -> binds:value_binding list list -> expression -> expression

(** Constructs a lambda of a fresh variable. Passes a reference to that variable as [arg]
    to construct the lambda's body. *)
val fresh_lambda : loc:location -> (arg:expression -> expression) -> expression

(** Conservative approximation of which expressions are syntactically values, i.e.
    constants, variables, or lambdas. When [true], these expressions have no effects
    (other than possibly closure allocation) and can be used in [let rec] definitions.
    When [false], they may need to be eta-expanded or wrapped in [lazy]. *)
val is_value_expression : expression -> bool

(** Shadows [Ppxlib.really_recursive] with a version that respects the [[@opaque]]
    attribute. *)
val really_recursive_respecting_opaque : rec_flag -> type_declaration list -> rec_flag
