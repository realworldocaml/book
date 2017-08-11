module Chunk : sig
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t [@@deriving sexp]

  val code : t -> string
  val warnings : t -> string
  val responses : t -> response list
  val stdout : t -> string
  val evaluated : t -> bool
end

module Part : sig
  type t [@@deriving sexp]
  val name : t -> string
  val chunks : t -> Chunk.t list
end

module Document : sig
  type t [@@deriving sexp]
  val parts : t -> Part.t list
  val matched : t -> bool
end
