(** Pygments support. Call out to the [pygmentize] command line
    tool. *)
open Core.Std
open Async.Std

(** Pygments languages, just the ones we need. *)
type lang = [
  | `OCaml
  | `Bash
  | `C
  | `Gas
  | `Java
  | `Json
  | `Scheme
]

val of_lang : Rwo_lang.t -> lang Or_error.t

(** Return an HTML [div] element. *)
val pygmentize : lang -> string -> Rwo_html.item Deferred.t
