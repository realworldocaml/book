module Base : sig
  (** Type for the location independent parts of the expansion context *)
  type t

  (**/*)
  (** Undocumented section *)

  (** Build a new base context at the top level of the given file with the given
      ocaml-mirgate-parsetree configuration.
  *)
  val top_level :
    omp_config:Migrate_parsetree.Driver.config ->
    file_path:string ->
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

  (** Return the ocaml-migrate-parsetree configuration for the given expansion context *)
  val omp_config : t -> Migrate_parsetree.Driver.config

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

  (** Return the ocaml-migrate-parsetree configuration for the given expansion context *)
  val omp_config : t -> Migrate_parsetree.Driver.config

  (** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
  val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)

  (** Whether the derived code is going to be inlined in the source *)
  val inline : t -> bool

  (**/**)
  (** Undocumented section *)

  (** Build a new expansion context with the given item location and code path *)
  val make : derived_item_loc:Location.t -> inline:bool -> base:Base.t -> unit -> t
end
