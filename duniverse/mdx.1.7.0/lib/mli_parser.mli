open! Compat

module Code_block : sig
  type t = { location : Odoc_model.Location_.span; contents : string }
end

val docstring_code_blocks : string -> Code_block.t list
(** Parse an mli file as a string and return a list of the code blocks that appear inside
    its docstrings. *)

val parse_mli : string -> (Document.line list, [ `Msg of string ]) Result.result
(** Slice an mli file into its [Text] and [Block] parts. *)
