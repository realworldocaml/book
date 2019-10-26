(* This is an internal module; it shouldn't be used by anything not in core_extended *)
(* some of the core functions of the "Shell" go in here because they are
   needed by other modules which are in turn required by the full shell module *)

val extra_path : string list ref

val whoami : ?real:bool -> unit -> string
val is_executable : string -> bool
val which : ?use_extra_path:bool -> string -> string option
val path_expand : ?use_extra_path:bool -> string -> string
