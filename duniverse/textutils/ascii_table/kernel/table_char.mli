(** [Table_char] holds a box-drawing character *)

open! Core
open! Import

type t =
  { ascii : char
  ; utf8 : string
  }
[@@deriving sexp_of]

val connect : ?top:unit -> ?bottom:unit -> ?left:unit -> ?right:unit -> unit -> t
val to_buffer : t -> Buffer.t -> [ `Ascii | `Unicode ] -> unit
