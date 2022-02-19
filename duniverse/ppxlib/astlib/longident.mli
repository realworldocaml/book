(** Long identifiers, used in parsetrees. *)

(** The long identifier type *)
type t = Ocaml_common.Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten : t -> string list
(** Flatten a long identifier built upon [Lident] and [Ldot]. Raise when hitting
    [Lapply].*)

val parse : string -> t
(** Parse a string into a long identifier built upon [Lident] and [Ldot]. *)
