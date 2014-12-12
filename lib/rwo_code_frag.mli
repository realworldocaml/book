(** A fragment description, intended to be embedded in the book as

    {v
    ```frag
    ((typ xxx)(name xxx)(part 1)(header false))
    ```
    v}

    where the (part X) defaults to 0 and (header) defaults to true. If
    (part X) is specified, then there will be '#' preprocessor
    directives in the [name] file.

    The [name] file should be under the `code/` subdirectory
*)
open Core.Std
open Async.Std

type typ = [
| `OCaml
| `OCaml_toplevel
| `OCaml_rawtoplevel
| `Console
| `JSON
| `ATD
| `Scheme
| `OCaml_syntax
| `C
| `Bash
| `CPP
| `Java
| `Ascii
| `Gas
] with sexp

type t = {
  typ: string;
  name: string;
  part: int;
  header: bool;
} with sexp

val typ_of_string : string -> typ Or_error.t
val typ_to_string : typ -> string

(** Generate filepath for given [t] and extension [ext]. *)
val file_of_t : ext:string -> t -> string

(** [read ~ext t] reads contents of [file_of_t ~ext t]. *)
val read : ext:string -> t -> string Deferred.t

val typ_to_docbook_language : typ -> string

val of_string : string -> t Or_error.t

(** [extract_ocaml_parts filename buf] hunts through OCaml code [buf]
    and splits out sections delimited by (* part X *). The [filename]
    is for error messages only. *)
val extract_ocaml_parts
  :  string
  -> string
  -> (int * string) list Or_error.t

(** [extract_ocaml_part filename part buf] is like above but extracts
    only requested [part]. *)
val extract_ocaml_part : string -> int -> string -> string Or_error.t

(** [run_through_pygmentize lang contents] *)
val run_through_pygmentize : string -> string -> Cow.Xml.t Or_error.t

(** Concat lines of toplevel phrases so that each toplevel phrase
    always starts with a double semicolon. *)
val concat_toplevel_phrases : string list -> string list Or_error.t

(** [wrap_in_pretty_box ~part typ file buf] *)
val wrap_in_pretty_box
  :  part:int
  -> string
  -> string
  -> Cow.Xml.t
  -> Cow.Xml.t
