(** Meta data related interface for a ppx rewriter *)

val add_ppx_context_str :
  tool_name:string ->
  Parsetree.structure_item list ->
  Parsetree.structure_item list
(** Extract information from the current environment and encode it into an
    attribute which is prepended to the list of structure items in order to pass
    the information to an external processor. *)

val drop_ppx_context_str :
  restore:bool -> Parsetree.structure_item list -> Parsetree.structure_item list
(** Drop the ocaml.ppx.context attribute from a structure. If [restore] is true,
    also restore the associated data in the current process. *)

val add_ppx_context_sig :
  tool_name:string ->
  Parsetree.signature_item list ->
  Parsetree.signature_item list
(** Same as [add_ppx_context_str], but for signatures. *)

val drop_ppx_context_sig :
  restore:bool -> Parsetree.signature_item list -> Parsetree.signature_item list
(** Same as [drop_ppx_context_str], but for signatures. *)

val tool_name : unit -> string
(** Can be used within a ppx preprocessor to know which tool is calling it
    ["ocamlc"], ["ocamlopt"], ["ocamldoc"], ["ocamldep"], ["ocaml"], ... *)

(** {1 Cookies} *)

(** Cookies are used to pass information from a ppx processor to a further
    invocation of itself, when called from the OCaml toplevel (or other tools
    that support cookies). *)

val set_cookie : string -> Parsetree.expression -> unit
(* [set_cookie name expr] registers a cookie with name [name] and value [expr]. *)

val get_cookie : string -> Parsetree.expression option
(* Returns the registered cookie with name [name], if any. *)
