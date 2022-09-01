open! Base

module type S = sig
  type sexp
  type 'a writer = Config.t -> 'a -> sexp -> unit

  (** [pp_formatter conf fmt sexp] will mutate the fmt with functions such as
      [set_formatter_tag_functions] *)
  val pp_formatter : Caml.Format.formatter writer

  val pp_formatter'
    :  next:(unit -> sexp option)
    -> Config.t
    -> Caml.Format.formatter
    -> unit

  val pp_buffer : Buffer.t writer
  val pp_out_channel : Caml.out_channel writer
  val pp_blit : (string, unit) Blit.sub writer

  (** [pretty_string] needs to allocate. If you care about performance, using one of the
      [pp_*] functions above is advised. *)
  val pretty_string : Config.t -> sexp -> string

  val sexp_to_string : sexp -> string
end

(** Pretty-printing of S-expressions *)
module type Sexp_pretty = sig
  module Config = Config

  module type S = S

  include S with type sexp := Sexp.t
  module Sexp_with_layout : S with type sexp := Sexplib.Sexp.With_layout.t_or_comment

  module Normalize : sig
    type t =
      (* Contains a sexp with associated comments. *)
      | Sexp of sexp * string list
      | Comment of comment

    and comment =
      | Line_comment of string
      (* Does not contain the "#|" "|#"; contains its indentation size. *)
      | Block_comment of int * string list
      | Sexp_comment of comment list * sexp

    and sexp =
      | Atom of string
      | List of t list

    val of_sexp_or_comment : Config.t -> Sexplib.Sexp.With_layout.t_or_comment -> t
  end

  val sexp_to_sexp_or_comment : Sexp.t -> Sexplib.Sexp.With_layout.t_or_comment
end
