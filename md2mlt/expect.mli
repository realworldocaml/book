module Raw_script : sig
  type part = { name : string; content : string; }
  val dump_part: part Fmt.t
  val pp_part: part Fmt.t
  type t = part list
  val of_file : filename:string -> t
end

module Chunk: sig
  include (module type of Ocaml_topexpect.Chunk
            with type t = Ocaml_topexpect.Chunk.t)
  val dump: t Fmt.t
  val pp: t Fmt.t
end

module Part = Ocaml_topexpect.Part

module Mlt : sig
  include (module type of Ocaml_topexpect.Document)
  val of_file: filename:string -> t
end

module Cram: sig
  type t
  val dump: t Fmt.t
  val pp: t Fmt.t
  val is_empty: t -> bool
  val to_html: t -> string
  val part: string -> t -> t option
  val of_file: filename:string -> t
end
