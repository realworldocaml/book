open Ppxlib

(** Lift a lexing position to a expression *)
val lift_position : loc:Location.t -> Parsetree.expression

(** Lift a lexing position to a string expression *)
val lift_position_as_string : loc:Location.t -> Parsetree.expression

(** Same as setting the directory name with [-dirname], for tests *)
val set_dirname : string option -> unit

(** Prepend the directory name if [-dirname] was passed on the command line and the
    filename is relative. *)
val expand_filename : string -> string
