open Base

module Name : sig
  (** Strongly-typed filename *)
  type t [@@deriving sexp, compare]
  val relative_to : dir:string -> t -> string
  include Identifiable.S with type t := t
end

val initial_dir : unit -> string

module Location : sig
  (** Location within a file *)
  type t =
    { filename    : Name.t
    ; line_number : int
    ; line_start  : int
    ; start_pos   : int
    ; end_pos     : int
    } [@@deriving sexp, compare]

  val beginning_of_file : Name.t -> t

  include Comparable.S with type t := t
end

module Digest : sig
  type t [@@deriving sexp_of, compare]
  val of_string : string -> t
  val to_string : t -> string
end
