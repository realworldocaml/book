open! Base
open Import

module Name : sig
  (** Strongly-typed filename *)
  type t [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t

    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val relative_to : dir:string -> t -> string

  include Identifiable.S with type t := t
end

val initial_dir : unit -> string

module Location : sig
  (** Location within a file *)
  type t =
    { filename : Name.t
    ; line_number : int
    ; line_start : int
    ; start_pos : int
    ; end_pos : int
    }
  [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t

    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val beginning_of_file : Name.t -> t

  include Comparable.S with type t := t
end

module Digest : sig
  type t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val of_string : string -> t
  val to_string : t -> string
end
