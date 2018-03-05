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

module Lexbuf: sig
  type t
  val v: fname:string -> string -> t
  val of_file: string -> t
  val position_mapper: Lexing.position -> Ast_mapper.mapper
end

module Phrase: sig
  type t

  val result: t -> (Parsetree.toplevel_phrase, exn) result
  val start: t -> Lexing.position
  val is_findlib_directive: t -> bool

  val read: Lexbuf.t -> t option

  type 'a kind =
    | Code of 'a
    | Expect of { location: Location.t;
                  responses: Chunk.response list;
                  nondeterministic: bool }
    | Part of { location: Location.t; name: string }

  val kind: t -> unit kind

  type v = (t * Chunk.response list kind) list
  (** The type for evaluated phrases. *)

  val read_all: Lexbuf.t -> v
  (** [read_all d] is the list of phrases where [Code] parameters are
      filled with what is in the corresponding [Expext] segments. No
      code is evaluated during that stage. *)

  val document: Lexbuf.t -> matched:bool -> v -> Document.t

  val validate: run_nondeterministic:bool -> v -> bool * v

  val output: out_channel -> Lexbuf.t -> v -> unit
end
