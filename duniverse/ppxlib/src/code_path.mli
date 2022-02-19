open! Import

type t
(** Type for path to AST nodes *)

val file_path : t -> string
(** Return the path to the .ml or .mli file for this code path. *)

val main_module_name : t -> string
(** Return the module name corresponding to the file to which this code path
    leads to. *)

val submodule_path : t -> string list
(** Return the path within the main module this code path represents as a list
    of module names. *)

val value : t -> string option
(** Return the name of the value to which this code path leads or [None] if it
    leads to the toplevel of a module or submodule. *)

val fully_qualified_path : t -> string
(** Return the fully qualified path to the module or value this code path leads
    to, eg ["Some_main_module.Some_submodule.some_value"]. Note that the fully
    qualified path doesn't descend into expressions which means it will always
    stop at the first value description or value binding. *)

val to_string_path : t -> string
(** Return the string version of this code path as built by
    [Ast_traverse.map_with_path]. Used for compatibility with path from version
    0.5.0 and lower. *)

(**/**)

(** Undocumented section *)

val top_level : file_path:string -> t
(** [top_level ~file_path] returns the code path for any toplevel item in the
    file at [file_path]. *)

val enter_expr : t -> t
(** Return a new code path that now descends into an expression. This is used to
    delimit the "toplevel" path. It's required because of first class modules
    and toplevel expressions [Pstr_eval ...]. *)

val enter_module : loc:Location.t -> string -> t -> t
(** Return a new code path updated with the given module name and location. *)

val enter_value : loc:Location.t -> string -> t -> t
(** Return a new code path updated with the given variable name and location. *)

val with_string_path :
  (loc:Location.t -> path:string -> 'a) -> loc:Location.t -> path:t -> 'a
(** Wrap a [fun ~loc ~path] expecting a string path into one expecting a [t]. *)
