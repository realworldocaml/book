(** Import nodes. We support a custom HTML schema to express that some
    code should be imported from another file. The syntax is:

    {v
    <link rel="import" href="path" part="N">
    v}

    where ["path"] is a local
    path to a code file, ["N"] is a part number within that file. The
    [part] attribute is optional.
*)
open Core

type part = string
  [@@deriving sexp]

type t = {
  href : string;
  part : part option;
  alt : string option;
  childs : Html.item list;
} [@@deriving sexp]

val of_html : Html.item -> t Or_error.t

val to_html : t -> Html.item

val lang_of : t -> Lang.t Or_error.t

(** Return true if given HTML [item] should be parseable as an import node
    and nothing else. A true result doesn't guarantee that
    [parse_import item] will succceed, only that it should because it
    can't be anything else. *)
val is_import_html : Html.item -> bool

val find_all : Html.t -> t list

include Comparable.S with type t := t
