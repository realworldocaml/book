open Import
type t
val of_exn : exn -> t option
val register_error_of_exn : (exn -> t option) -> unit
val message : t -> string
val set_message : t -> string -> t
val make : loc:Location.t -> string -> sub:(Location.t * string) list -> t
val to_extension : t -> Import.Parsetree.extension
val raise : t -> 'a
val update_loc : t -> Location.t -> t
val get_location : t -> Location.t
