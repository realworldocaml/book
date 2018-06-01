(** HTML processing. *)
open Core
open Async

type attributes = (string * string) list [@@deriving sexp]

type element = {
  name : string;
  attrs : attributes;
  childs : item list;
}

and item = [
| `Element of element
| `Data of string
] [@@deriving sexp]

type t = item list [@@deriving sexp]

val of_string : string -> t
val of_file : string -> t Deferred.t

val to_string : t -> string

(** [is_elem_node item name] returns true if [item] is a [name]
    element node. *)
val is_elem_node : item -> string -> bool

(** True if given filename has .html extension. *)
val has_html_extension : string -> bool

(** [html_files_of_dir dir] returns all files in [dir] that have a
    .html extension. The returned paths include [dir]. *)
val html_files_of_dir : string -> string list Deferred.t

(** [get_all_nodes name t] returns all [name] nodes in [t], regardless
    of what depth they occur at. Nested [name] nodes will not be
    returned; they will only be part of their ancestor which is
    returned.
*)
val get_all_nodes : string -> t -> item list

(** [is_nested name t] returns true if elem [name] ever occurs nested
    under itself in [t]. *)
val is_nested : string -> t -> bool

(** Strip document of all data nodes, all elements in
    [exclude_elements] list, and all attributes not in
    [keep_attrs] list. Then print result with indentation. Useful
    to see the structure of a large document. *)
val print_elements_only
  :  ?exclude_elements:string list
  -> ?keep_attrs:string list
  -> item list
  -> unit

(** Filter out pure whitespace Data nodes. *)
val filter_whitespace : item list -> item list

val fold : item list -> init:'a -> f:('a -> item -> 'a) -> 'a

(** [replace_id_node_with t id with_] searches [t] for a node with
    given [id], and replaces that node entirely with [with_] items. *)
val replace_id_node_with : t -> id:string -> with_:(item list) -> t


(******************************************************************************)
(** {2 Constructors} *)
(******************************************************************************)
val div : ?a:attributes -> item list -> item
val span : ?a:attributes -> item list -> item
val p : ?a:attributes -> item list -> item
val pre : ?a:attributes -> item list -> item
val code : ?a:attributes -> item list -> item
val article : ?a:attributes -> item list -> item
val body : ?a:attributes -> item list -> item
val html : ?a:attributes -> item list -> item

val a : ?a:attributes -> item list -> item
val i : ?a:attributes -> item list -> item
val br : item

val ul : ?a:attributes -> item list -> item
val li : ?a:attributes -> item list -> item

val h1 : ?a:attributes -> item list -> item
val h2 : ?a:attributes -> item list -> item
val h3 : ?a:attributes -> item list -> item
val h4 : ?a:attributes -> item list -> item
val h5 : ?a:attributes -> item list -> item
val h6 : ?a:attributes -> item list -> item

val small : ?a:attributes -> item list -> item
val sup : ?a:attributes -> item list -> item

val table : ?a:attributes -> item list -> item
val thead : ?a:attributes -> item list -> item
val th : ?a:attributes -> item list -> item
val tbody : ?a:attributes -> item list -> item
val tr : ?a:attributes -> item list -> item
val td : ?a:attributes -> item list -> item

val dl : ?a:attributes -> item list -> item
val dd : ?a:attributes -> item list -> item

val head : ?a:attributes -> item list -> item
val meta : ?a:attributes -> item list -> item
val title : ?a:attributes -> item list -> item
val script : ?a:attributes -> item list -> item
val link : ?a:attributes -> item list -> item

val nav : ?a:attributes -> item list -> item
val footer : ?a:attributes -> item list -> item


(******************************************************************************)
(** {2 Attributes} *)
(******************************************************************************)

(** Get list of all attribute names occurring anywhere in [t]. *)
val get_all_attributes : t -> string list

(** Check that the given [attributes] contain a value for all those
    named in [required]. If [allowed = `Any] also allow [attributes]
    to contain any additional attributes, or if [`Some others] is
    specified then allow [attributes] to contain at most
    [others]. Providing [`Some \[\]] allows to specify that no
    additional attributes are allowed. Defaults: [required] is empty,
    and [allowed] is `Any. Repeated attributes are never allowed. *)
val check_attrs
  :  ?required : string list
  -> ?allowed : [`Some of string list | `Any]
  -> attributes
  -> unit Or_error.t
