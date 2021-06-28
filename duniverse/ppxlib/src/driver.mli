open Import

(** Add one argument to the command line *)
val add_arg : Caml.Arg.key -> Caml.Arg.spec -> doc:string -> unit

(** Error reported by linters *)
module Lint_error : sig
  type t

  val of_string : Location.t -> string -> t
end

module Cookies : sig
  type t

  (** [get cookies name pattern] look for a cookie named [name] and parse it using
      [pattern]. *)
  val get : t -> string -> (expression, 'a -> 'a, 'b) Ast_pattern.t -> 'b option

  (** [set cookies name expr] set cookie [name] to [expr]. *)
  val set : t -> string -> expression -> unit

  (** Register a callback that is called before a rewriting. The handler is expected to
      lookup some cookies and set some global variables.

      This API is a temporary hack to allow to migrate from [add_arg] to the use of
      cookie, until ppxlib has been upgraded to pass cookies through. *)
  val add_handler : (t -> unit) -> unit

  (** Shorthand for: [add_handler (fun t -> f (get t name pattern))] *)
  val add_simple_handler
    :  string
    -> (expression, 'a -> 'a, 'b) Ast_pattern.t
    -> f:('b option -> unit)
    -> unit

  (** Register a callback that is called after a rewriting. The handler is expected to set
      some cookies from some global variables. *)
  val add_post_handler : (t -> unit) -> unit
end

(** [register_transformation name] registers a code transformation.

    [name] is a logical name for the transformation (such as [sexp_conv] or
    [bin_prot]). It is mostly used for debugging purposes.

    [rules] is a list of context independent rewriting rules, such as extension point
    expanders. This is what most code transformation should use. Rules from all registered
    transformations are all applied at the same time, before any other
    transformations. Moreover they are applied in a top-down manner, giving more control
    to extensions on how they interpret their payload.

    For instance:

    - some extensions capture a pretty-print of the payload in their expansion and using
      top-down ensures that the payload is as close as possible to the original code
    - some extensions process other extension in a special way inside their payload. For
      instance [%here] (from ppx_here) will normally expand to a record of type
      [Lexing.position]. However when used inside [%sexp] (from ppx_sexp_value) it will
      expand to the human-readable sexp representation of a source code position.

    [extensions] is a special cases of [rules] and is deprecated. It is only kept for
    backward compatibility.

    [enclose_impl] and [enclose_intf] produces a header and footer for
    implementation/interface files. They are a special case of [impl] and [intf]. The
    header is placed after any initial module-level attributes; the footer is placed after
    everything else. Both functions receive a location that denotes all of the items
    between header and footer, or [None] if the that list of items is empty.

    [impl] is an optional function that is applied on implementation files and [intf] is
    an optional function that is applied on interface files. These two functions are
    applied on the AST of the whole file. They should only be used when the other
    mechanism are not enough. For instance if the transformation expands extension points
    that depend on the context.

    If no rewriter is using [impl] and [intf], then the whole transformation is completely
    independent of the order in which the various rewriter are specified. Moreover the
    resulting driver will be faster as it will do only one pass (excluding safety checks)
    on the whole AST.

    [lint_impl] and [lint_intf] are applied to the unprocessed source. Errors they return
    will be reported to the user as preprocessor warnings.

    Rewritings are applied in the following order:
    - linters ([lint_impl], [lint_intf])
    - preprocessing ([preprocess_impl], [preprocess_intf])
    - context-independent rules ([rules], [extensions])
    - whole-file transformations ([impl], [intf], [enclose_impl], [enclose_intf])
*)
val register_transformation
  :  ?extensions       : Extension.t list (* deprecated, use ~rules instead *)
  -> ?rules            : Context_free.Rule.t list
  -> ?enclose_impl     : (Location.t option -> structure * structure)
  -> ?enclose_intf     : (Location.t option -> signature * signature)
  -> ?impl             : (structure -> structure)
  -> ?intf             : (signature -> signature)
  -> ?lint_impl        : (structure -> Lint_error.t list)
  -> ?lint_intf        : (signature -> Lint_error.t list)
  -> ?preprocess_impl  : (structure -> structure)
  -> ?preprocess_intf  : (signature -> signature)
  -> ?aliases          : string list
  -> string
  -> unit

(** Same as [register_transformation] except that it uses the same AST as the current
    ocaml compiler.

    This is not the intended way of using driver. This is only for ppx rewriters that
    are not written using ppxlib but want to export a driver compatible
    library.
*)
val register_transformation_using_ocaml_current_ast
  :  ?impl : (Migrate_parsetree.OCaml_current.Ast.Parsetree.structure ->
              Migrate_parsetree.OCaml_current.Ast.Parsetree.structure)
  -> ?intf : (Migrate_parsetree.OCaml_current.Ast.Parsetree.signature ->
              Migrate_parsetree.OCaml_current.Ast.Parsetree.signature)
  -> ?aliases : string list
  -> string
  -> unit

(** Same as:

    {[
      register_transformation
        ~name
        ~impl
        ~intf
        ()
    ]}
*)
val register_code_transformation
  :  name:string
  -> ?aliases:string list
  -> impl:(structure -> structure)
  -> intf:(signature -> signature)
  -> unit
  [@@deprecated "[since 2015-11] use register_transformation instead"]

(** Rewriters might call this function to suggest a correction to the code source. When
    they do this, the driver will generate a [file.ml.ppx-corrected] file with the
    suggested replacement. The build system will then show the diff to the user who is
    free to accept the correction or not. *)
val register_correction : loc:Location.t -> repl:string -> unit

(** Hook called before processing a file *)
val register_process_file_hook : (unit -> unit) -> unit

(** Create a new file property.

    A file property represent a piece of information about a file that can be set during
    preprocessing. If the [-output-metadata FILE] command line option was passed to the
    driver, then it will output this information to the given file.

    This mechanism is used to pass information gathered while preprocessing the file to
    the build system. For instance, this is used by ppx_inline_test to tell whether a file
    contains tests or not.

    In the future we could also use this to directly compute the dependencies and pass
    them here, to avoid calling ocamldep separately.
*)
module Create_file_property(Name : sig val name : string end)(T : Sexpable.S) : sig
  val set : T.t -> unit
end

(** Suitable for -pp and also usable as a standalone command line tool.

    If the first command line argument is [-as-ppx] then it will run as a ppx rewriter. *)
val standalone : unit -> unit

(** Suitable for -ppx. Used only for the public release. *)
val run_as_ppx_rewriter : unit -> unit

(** If [true], code transformations should avoid generating code that is not strictly
    necessary, such as extra type annotations. *)
val pretty : unit -> bool

(**/**)
val map_structure : structure -> Migrate_parsetree.Driver.some_structure

val enable_checks : unit -> unit
val enable_location_check : unit -> unit
val disable_location_check : unit -> unit

