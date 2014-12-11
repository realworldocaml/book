open Core.Std
open Async.Std

(** Html element. *)
type item = Nethtml.document

(** Html document. Note: nethtml's terminology is incorrect. An html
    document is really a list of what it calls a document. *)
type t = item list

type attributes = (string * string) list

val item_of_string : string -> item Or_error.t
val item_of_file : string -> item Or_error.t Deferred.t

val of_string : string -> t
val of_file : string -> t Deferred.t

val to_string : t -> string

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
  -> t
  -> unit


(** {2 Attributes} *)

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


(** {2 HTMLBook Code Blocks} *)

type code_item = [
| `Output of string
| `Prompt of string
| `Input of string
| `Data of string
]

(** Guaranteed that `Prompt always followed by `Input. *)
type code_block = private code_item list

type pre = {
  data_type : string;
  class_ : string option;
  data_code_language : string option;
  code_block : code_block;
}

val code_items_to_code_block : code_item list -> code_block Or_error.t

val parse_pre : item -> pre Or_error.t


(** {2 Import Nodes}

    Nodes of the form <link rel="import"> are handled specially.
*)

type import = {
  data_code_language : string;
  href : string;
}

val parse_import : item -> import Or_error.t
