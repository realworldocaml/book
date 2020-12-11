open! Compat

module Code_block : sig
  type t = { location : Odoc_model.Location_.span; contents : string }
end

val parse_mli : string -> (Document.line list, [ `Msg of string ]) Result.result
(** Slice an mli file into its [Text] and [Block] parts. *)
