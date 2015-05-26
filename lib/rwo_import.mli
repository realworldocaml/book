(** Import nodes. We support a custom HTML schema to express that some
    code should be imported from another file. The syntax is:

    {v
    <link rel="import" data-code-lang="lang" href="path" part="N">
    v}

    where ["lang"] is any language we support, ["path"] is a local
    path to a code file, ["N"] is a part number within that file. The
    [part] attribute is optional.
*)
open Core.Std

type t = {
  data_code_language : Rwo_lang.t;
  href : string;
  part : float option;
  childs : Rwo_html.item list;
} with sexp

val of_html : Rwo_html.item -> t Or_error.t

val to_html : t -> Rwo_html.item

(** Return true if given HTML [item] should be parseable as an import node
    and nothing else. A true result doesn't guarantee that
    [parse_import item] will succceed, only that it should because it
    can't be anything else. *)
val is_import_html : Rwo_html.item -> bool

val find_all : Rwo_html.t -> t list

include Comparable.S with type t := t
