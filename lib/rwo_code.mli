(** Code blocks. Book can contain code blocks for various languages. A
    code block might be a complete compiliation unit, but more
    generally it is only a part of a larger compilation unit.

    Code is kept in separate files and imported into the HTMLBook
    source with <link rel="import"> nodes. Each source file must
    represent a complete compliation unit if you intend to
    auto-evaluate the code.

    The syntax for indicating the start of a new part in a source file
    depends on the language the code is for. Let N indicate a part
    number >= 1. It is assumed that the beginning of the file
    indicates part 0, and a file without parts is considered to have
    the single part numbered 0.

    - [`OCaml_toplevel] - A line in the form "#part N".

    - [`OCaml] - A line with a comment in the form "(* part N *)".

    - Code for other languages cannot be split into parts, i.e. we
    consider the entire file to be a single part, numbered 0.

    In all functions taking an optional [?part] argument, the default
    value is 0.
*)
open Core.Std
open Async.Std

type lang = [
| `OCaml
| `OCaml_toplevel
| `OCaml_rawtoplevel
| `OCaml_syntax
| `Console
| `JSON
| `ATD
| `Scheme
| `C
| `Bash
| `CPP
| `Java
| `Ascii
| `Gas
] with sexp

(** Map from file to part number within that file to a code block of
    type ['a]. A parser might set ['a = string] and return the
    contents of a code block as raw text. Alternatively, ['a] might be
    a specialized type representing the result of evaluating a code
    block. *)
type 'a t

(** A phrase is a complete statement that can be evaluated (possibly
    requiring some context). For each, we provide:

    - [input] - The [input] phrase that was evaluated.

    - [output] - Main [output] an evaluator provides to display the
    result of evaluating a phrase. Should be the empty string if an
    exception is raised, or to indicate no evaluation was done.

    - [stdout/stderr] - Anything printed to stdout/stderr as a side
    effect of evaluating the phrase. Empty string when no evaluation
    was done.
*)
type phrase = {
  input : string;
  output : string;
  stdout : string;
  stderr : string;
} with sexp


(******************************************************************************)
(** {2 Add Code Files} *)
(******************************************************************************)

(** [add_file_exn t ~run file] adds the results of [run file] to
    [t]. *)
val add_file_exn
  :  'a t
  -> lang:lang
  -> run:(string -> (int * 'a) list Deferred.t)
  -> string
  -> 'a t Deferred.t

(** Split given file contents into its parts. The [filename] is only
    for error messages. *)
val split_parts_exn
  :  lang:lang
  -> filename:string
  -> string
  -> (int * string) list

(** Split given file into its parts. The function
    [split_parts_file_exn ~lang] is suitable as the [run] argument of
    [add_file_exn]. *)
val split_parts_of_file_exn
  :  lang:lang
  -> filename:string
  -> (int * string) list Deferred.t

(** Split the given string on the delimiter ";;". The delimiter is
    retained in the output strings. Give [`Eol] to consider only ";;"
    occuring at the end of a line, or [`Anywhere] to allow it
    anywhere. *)
val split_ocaml_toplevel_phrases
  :  [`Eol | `Anywhere]
  -> string
  -> string list Or_error.t

(** Run given file through the OCaml toplevel. This function is
    suitable as the [run] argument of [add_file_exn]. Temporary files
    are written to [repo_root/_build] and a system call to
    [repo_root/_build/app/rwo_runtop] is made. The answer is a list
    consisting of pairs, each containing the part number and evaluated
    phrases for that part. Default: [repo_root = "."]. *)
val run_ocaml_toplevel_file_exn
  :  ?repo_root:string
  -> string
  -> (int * phrase list) list Deferred.t

(** [run_file_exn file lang] evaluates [file] through an appropriate
    evaluator for [lang]. This function is suitable as the [run]
    argument of [add_file_exn]. *)
val run_file_exn
  :  ?repo_root:string
  -> string
  -> lang:lang
  -> (int * phrase list) list Deferred.t


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)
val phrases_to_html : lang -> phrase list -> Rwo_html.item

(** [run_through_pygmentize lang contents] *)
val run_through_pygmentize : lang -> string -> Cow.Xml.t Or_error.t

(** Concat lines of toplevel phrases so that each toplevel phrase
    always starts with a double semicolon. *)
val concat_toplevel_phrases : string list -> string list Or_error.t

(** [wrap_in_pretty_box ~part lang file buf] *)
val wrap_in_pretty_box
  :  part:int
  -> string
  -> string
  -> Cow.Xml.t
  -> Cow.Xml.t


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
val empty : _ t
val find : 'a t -> ?part:int -> file:string -> 'a option
val find_exn : 'a t -> ?part:int -> file:string -> 'a
val files_langs_parts : _ t -> (string * lang * int) list
val file_is_mem : _ t -> string -> bool
val lang_of_file : _ t -> string -> lang option


(******************************************************************************)
(** {Lang Operations} *)
(******************************************************************************)
val lang_of_string : string -> lang Or_error.t
val lang_to_string : lang -> string
val lang_to_docbook_language : lang -> string Or_error.t
val lang_to_pygmentize_language : lang -> string Or_error.t
