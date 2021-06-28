(** The collection of core types known by ATD. *)

val list : (string * int * Ast.type_def option) list

val make_table : unit -> (string, int * Ast.type_def option) Hashtbl.t
