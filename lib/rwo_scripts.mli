(** Collection of OCaml scripts. *)
open Core.Std
open Async.Std

type phrase = {input : string; outcome : Oloop.Output.merged Oloop.Outcome.t}
type part = float * phrase list

type script = [
| `OCaml of Oloop.Script.t
| `OCaml_toplevel of part list
| `OCaml_rawtoplevel of Oloop.Script.t
| `Other of string
]

type t = script String.Map.t (** key is filename *)

val eval_script
  :  Rwo_lang.t
  -> filename:string
  -> script Or_error.t Deferred.t

val add_script
  :  t
  -> Rwo_lang.t
  -> filename:(string * string)
  -> t Or_error.t Deferred.t
(** [add_script t (dir,file)] adds the script at path [dir/file] to
    [t]. Only [file] is used as the key in the returned map. *)

val of_html : filename:string -> Rwo_html.t -> t Or_error.t Deferred.t
(** Return all scripts found in given HTML. *)


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)
val phrases_to_html : phrase list -> Rwo_html.item


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
val empty : t

val find
  :  t
  -> ?part:float
  -> filename:string
  ->
  [
  | `OCaml of string
  | `OCaml_toplevel of phrase list
  | `OCaml_rawtoplevel of string
  | `Other of string
  ] option

val find_exn
  :  t
  -> ?part:float
  -> filename:string
  ->
  [
  | `OCaml of string
  | `OCaml_toplevel of phrase list
  | `OCaml_rawtoplevel of string
  | `Other of string
  ]

val file_is_mem : t -> string -> bool
