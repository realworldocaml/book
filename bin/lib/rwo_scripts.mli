(** Collection of scripts. *)
open Core
open Async

type part = string
  [@@deriving sexp]

type script = [
  | `OCaml of Rwo_expect.Raw_script.t
  | `OCaml_toplevel of Rwo_expect.Document.t
  | `OCaml_rawtoplevel of Rwo_expect.Raw_script.t
  | `Other of string
]

(** One part of a script. *)
type script_part = [
  | `OCaml of Rwo_expect.Raw_script.part
  | `OCaml_toplevel of Rwo_expect.Chunk.t list
  | `OCaml_rawtoplevel of Rwo_expect.Raw_script.part
  | `Other of string
]

type t = script String.Map.t (** key is filename *)

val eval_script
  :  Rwo_lang.t
  -> run_nondeterministic:bool
  -> filename:string -> script Or_error.t Deferred.t
(** Return the [script] representation of a parsed file *)

val eval_script_to_sexp
  : Rwo_lang.t
  -> run_nondeterministic:bool
  -> filename:string -> Sexp.t Or_error.t Deferred.t
(** Return the sexp representation of a parsed file *)
  
val of_html
  :  ?code_dir:string
  -> run_nondeterministic:bool
  -> filename:string
  -> Rwo_html.t
  -> t Or_error.t Deferred.t
(** Return all scripts found in given HTML. [code_dir] defaults to [examples]. *)


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)

(** Returns a list of <pre> elements. *)
val phrases_to_html
  :  ?pygmentize:bool
  -> Rwo_expect.Chunk.t list
  -> Rwo_html.t Deferred.t

(** Returns a single <div class="highlight"> element. *)
val script_part_to_html
  :  ?pygmentize:bool
  -> script_part
  -> Rwo_html.item Deferred.t


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
val empty : t
val file_is_mem : t -> string -> bool
val find_exn : t -> ?part:string -> filename:string -> script_part
val exn_of_filename : string -> string -> script_part
