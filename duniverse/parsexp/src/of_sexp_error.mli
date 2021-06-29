open! Import

type t [@@deriving_inline sexp_of]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end
[@@ocaml.doc "@inline"]

[@@@end]

(** Exception raised by the user function *)
val user_exn : t -> exn

(** S-expression that failed to be converted *)
val sub_sexp : t -> Sexp.t

(** Position of [sub_sexp t] in the original source, if found *)
val location : t -> Positions.range option

(** Similar to [Parse_error.report] *)
val report : Format.formatter -> filename:string -> t -> unit

(** Exception raised in case of a conversion error *)
exception Of_sexp_error of t

val raise : user_exn:exn -> sub_sexp:Sexp.t -> location:Positions.range option -> 'a
