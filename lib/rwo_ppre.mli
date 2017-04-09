(** HTML p tag followed by pre tag.

    O'Reilly's HTMLBook version of edition 1 represented code sections
    in the form

    {v <p></p><pre></pre> v}

    to encode some information about where code was imported from,
    index terms, and for the captions of the online version. We needed
    to parse such sections to extract the code from their source
    files, and may also need to generate such sections when converting
    back to pure HTMLBook.
*)
open Core
open Async

(** An item within a <pre> node*)
type code_item = [
| `Output of string
| `Prompt of string
| `Input of string
| `Data of string
]

(** Guaranteed that `Prompt always followed by `Input. *)
type code_block = private code_item list

(** Parsed <pre> node. *)
type pre = {
  data_type : string;
  data_code_language : string option;
  code_block : code_block;
}

(** Parsed <p> node. Only for <p> nodes occurring immediately before a
    <pre> node. *)
type p = {
  a_href : string; (** href value in main <a> *)
  a_class : string; (** class value in main <a> *)
  em_data : string; (** data of <em> node under main <a> *)
  data1 : string option; (** data node before main <a> *)
  part : Rwo_import.part option; (** part number from data node after main <a> *)
  a2 : Rwo_html.item option; (** 2nd <a> node, i.e. 1st after main <a> *)
  a3 : Rwo_html.item option; (** 3rd <a> node, i.e. 2nd after main <a> *)
}

(** HTML in the form <p></p><pre></pre>. *)
type t = {p:p; pre:pre}

(** Transform code sections according to [f]. Return error if input
    html doesn't conform to the expected schema for code sections. *)
val map : Rwo_html.t -> f:(t -> Rwo_html.item) -> Rwo_html.t Or_error.t

(** This conversion needed to implement [extract_code_from_1e_exn]. *)
val to_import : t -> Rwo_import.t Or_error.t

(** [extract_code_from_1e n] extracts code from chapter [n] of the
    HTMLBook sources provided by O'Reilly for edition 1. All <pre>
    nodes are replaced with <link rel="import"> nodes and extracted
    code is written to external files. Raise exception in case of any
    error. *)
val extract_code_from_1e_exn : int -> unit Deferred.t
