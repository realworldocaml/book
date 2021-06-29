module Base : sig
  (** Type for the location independent parts of the expansion context *)
  type t

  (** Return the code path for the given context
  In Driver, Deriving and Extension, the context is initialized so that the [file_path] component of
  the [code_path] is determined from the first location found in the input AST. That means that:
  - It's the empty string in empty structures or signatures
  - It can be altered by line directives *)
  val code_path : t -> Code_path.t

  (** Return the input name for the given context.
  In Driver, Deriving and Extension, the context argument is initialized so that the [input_name]
  matches the input filename passed to the driver on the command line. That means that:
  - It has a value even for empty files
  - It is not affected by line directives
  - It is ["_none_"] when using [Driver.map_structure] or [Driver.map_signature] *)
  val input_name : t -> string

  (** Can be used within a ppx preprocessor to know which tool is
      calling it ["ocamlc"], ["ocamlopt"], ["ocamldep"], ["ocaml"], ... . *)
  val tool_name : t -> string

  (**/**)
  (** Undocumented section *)

  (** Build a new base context at the top level of the given file with the given
      calling tool name.
  *)
  val top_level :
    tool_name:string ->
    file_path:string ->
    input_name:string ->
    t

  (** Proxy functions to update the wrapped code path. See code_path.mli for details. *)
  val enter_expr : t -> t
  val enter_module : loc:Location.t -> string -> t -> t
  val enter_value : loc:Location.t -> string -> t -> t
end

module Extension : sig
  (** Type of expansion contexts for extensions *)
  type t

  (** Return the location of the extension point being expanded *)
  val extension_point_loc : t -> Location.t

  (** Return the code path for the given context *)
  val code_path : t -> Code_path.t

  (** Can be used within a ppx preprocessor to know which tool is
      calling it ["ocamlc"], ["ocamlopt"], ["ocamldep"], ["ocaml"], ... . *)
  val tool_name : t -> string

  (** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
  val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)

  (**/**)
  (** Undocumented section *)

  (** Build a new expansion context with the given extension point location and base context *)
  val make : extension_point_loc:Location.t -> base:Base.t -> unit -> t
end

module Deriver : sig
  (** Type of expansion contexts for derivers *)
  type t

  (** Return the location of the item to which the deriver is being applied *)
  val derived_item_loc : t -> Location.t

  (** Return the code path for the given context *)
  val code_path : t -> Code_path.t

  (** Can be used within a ppx preprocessor to know which tool is
      calling it ["ocamlc"], ["ocamlopt"], ["ocamldep"], ["ocaml"], ... . *)
  val tool_name : t -> string

  (** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
  val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)

  (** Whether the derived code is going to be inlined in the source *)
  val inline : t -> bool

  (**/**)
  (** Undocumented section *)

  (** Build a new expansion context with the given item location and code path *)
  val make : derived_item_loc:Location.t -> inline:bool -> base:Base.t -> unit -> t
end
