(** Pygments support. Call out to the [pygmentize] command line
    tool. *)
open Core

(** Pygments languages, just the ones we need. *)
type lang = [
  | `OCaml
  | `Bash
  | `C
  | `Gas
  | `Java
  | `Json
  | `Scheme
  | `Sexp
]

val of_lang : Lang.t -> lang Or_error.t

(** Run given string through pygmentize. Return a single <pre>
    element.

    By default, the optional argument [pygmentize] is true. Set it to
    false to avoid actually calling pygmentize. In this case, we
    simply return the given string wrapped in an HTML <pre> tag to be
    consistent with what pygmentize returns. Also some characters are
    properly escaped.

    If given, [add_attrs] will be added to the attributes <pre>.
*)
val pygmentize
  :  ?interactive:bool
  -> ?add_attrs:Html.attributes
  -> lang
  -> string
  -> Html.item
