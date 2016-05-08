(** Collection of scripts. *)
open Core.Std
open Async.Std

type part = string

type oloop_script
type oloop_script_evaluated
type oloop_script_evaluated_phrase

type script = [
| `OCaml of oloop_script
| `OCaml_toplevel of oloop_script_evaluated
| `OCaml_rawtoplevel of oloop_script
| `Other of string
]

(** One part of a script. *)
type script_part = [
| `OCaml of string
| `OCaml_toplevel of oloop_script_evaluated_phrase list
| `OCaml_rawtoplevel of string
| `Other of string
]

type t = script String.Map.t (** key is filename *)

val of_html : filename:string -> Rwo_html.t -> t Or_error.t Deferred.t
(** Return all scripts found in given HTML. *)


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)
(** Returns a list of <pre> elements. *)
val phrases_to_html
  :  ?pygmentize:bool
  -> oloop_script_evaluated_phrase list
  -> Rwo_html.t Deferred.t

(** Returns a single <div class="highlight"> element. *)
val script_part_to_html
  :  ?pygmentize:bool
  -> script_part
  -> Rwo_html.item Deferred.t


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
(*val empty : t
  val find : t -> ?part:string -> filename:string -> script_part option
  val file_is_mem : t -> string -> bool*)
val find_exn : t -> ?part:string -> filename:string -> script_part
