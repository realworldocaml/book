(** Check the validity of an ATD file beyond syntax.

    For example, this checks that each type definition is unique.
*)

(** Check the validity of an ATD file. Raises an exception on the first
    error encountered. *)
val check : Ast.module_body -> unit
