(** Some helpers to annotate the AST so merlin can decide which branches to look at and
    which branches to ignore. *)

open! Import

(** {2 Annotations merlin understand} *)

(** Adding this [[@merlin.hide]] attribute on a piece of AST "hides" it from merlin: it
    tells merlin not to consider that branch if another piece of AST with the same
    location exist. *)
val hide_attribute  : attribute

(** Adding this [[@merlin.focus]] attribute on a piece of AST tells merlin to prefer it to
    any other piece of AST when several have the same location. *)
val focus_attribute : attribute

(** {2 Helpers}

    The following functions add the corresponding attribute (defined above) to specific
    pieces of AST. *)

val hide_pattern  : pattern -> pattern
val focus_pattern : pattern -> pattern

val hide_expression  : expression -> expression
val focus_expression : expression -> expression
