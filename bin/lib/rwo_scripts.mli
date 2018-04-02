(** Collection of scripts. *)
open Core
open Async

type part = string
  [@@deriving sexp]

type script = [
  | `OCaml of Rwo_expect.Raw_script.t
  | `OCaml_toplevel of Rwo_expect.Mlt.t
  | `OCaml_rawtoplevel of Rwo_expect.Raw_script.t
  | `Shell of Rwo_expect.Cram.t
  | `Other of string
]

(** One part of a script. *)
type script_part = [
  | `OCaml of Rwo_expect.Raw_script.part
  | `OCaml_toplevel of Rwo_expect.Chunk.t list
  | `OCaml_rawtoplevel of Rwo_expect.Raw_script.part
  | `Shell of Rwo_expect.Cram.t
  | `Other of string
]

type t = script String.Map.t (** key is filename *)

val of_html
  :  ?code_dir:string
  -> filename:string
  -> Rwo_html.t
  -> t Or_error.t Deferred.t
(** Return all scripts found in given HTML. [code_dir] defaults to [examples]. *)


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)

(** Returns a single <div class="highlight"> element. *)
val script_part_to_html: script_part -> Rwo_html.item


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
val empty : t
val file_is_mem : t -> string -> bool
val find_exn : t -> ?part:string -> filename:string -> script_part
val exn_of_filename : string -> string -> script_part
