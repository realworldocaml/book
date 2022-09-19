open! Import

type t =
  | Parse_error of Parse_error.t
  | Of_sexp_error of Of_sexp_error.t
[@@deriving_inline sexp_of]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Sexplib0.Sexp.t
end
[@@ocaml.doc "@inline"]

[@@@end]

(** Similar to [Parse_error.report] *)
val report : Format.formatter -> filename:string -> t -> unit
