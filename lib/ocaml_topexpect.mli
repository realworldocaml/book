module Chunk : sig
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t [@@deriving sexp]
  val v : ocaml_code:string -> toplevel_responses:response list -> t

  val code : t -> string
  val warnings : t -> string
  val responses : t -> response list
  val stdout : t -> string
  val evaluated : t -> bool

end

module Part : sig
  type t [@@deriving sexp]
  val v : name:string -> chunks:Chunk.t list -> t

  val name : t -> string
  val chunks : t -> Chunk.t list
end

module Document : sig
  type t [@@deriving sexp]
  val v : parts:Part.t list -> matched:bool -> t

  val parts : t -> Part.t list
  val matched : t -> bool
end
