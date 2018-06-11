(** Collection of scripts. *)
open Core
open Async

type part = string
  [@@deriving sexp]

type script = [
  | `OCaml of Expect.Raw_script.t
  | `OCaml_toplevel of Expect.Mlt.t
  | `OCaml_rawtoplevel of Expect.Raw_script.t
  | `Shell of Expect.Cram.t
  | `Other of string
]

(** One part of a script. *)
type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t list
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Shell of Expect.Cram.t
  | `Other of string
]

type t = script String.Map.t (** key is filename *)

val of_html
  :  ?code_dir:string
  -> filename:string
  -> Html.t
  -> t Or_error.t Deferred.t
(** Return all scripts found in given HTML. [code_dir] defaults to [examples]. *)


(******************************************************************************)
(** {2 Printers} *)
(******************************************************************************)

(** Returns a single <div class="highlight"> element. *)
val script_part_to_html: script_part -> Html.item


(******************************************************************************)
(** {2 Map-style Operations } *)
(******************************************************************************)
val empty : t
val file_is_mem : t -> string -> bool
val find_exn : t -> ?part:string -> filename:string -> script_part
val exn_of_filename : string -> string -> script_part
