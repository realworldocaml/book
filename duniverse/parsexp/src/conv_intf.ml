open! Import

module type S = sig
  type 'a res
  type chunk_to_conv
  type parsed_sexp

  val parse_string : string -> (chunk_to_conv -> 'a) -> ('a res, Conv_error.t) result
  val parse_string_exn : string -> (chunk_to_conv -> 'a) -> 'a res

  val conv
    :  parsed_sexp * Positions.t
    -> (chunk_to_conv -> 'a)
    -> ('a res, Of_sexp_error.t) result

  val conv_exn : parsed_sexp * Positions.t -> (chunk_to_conv -> 'a) -> 'a res

  (** Convenience function for merging parsing and conversion errors.

      For instance if you have a [load] function as follow:

      {[
        val load : string -> (Sexp.t list * Positions.t, Parse_error.t) result
      ]}

      then you can create a [load_conv] function as follow:

      {[
        let load_conv : string -> (Sexp.t -> 'a) -> ('a list, Conv_error.t) result
          = fun filename f -> conv_combine (load filename) f
      ]}
  *)
  val conv_combine
    :  (parsed_sexp * Positions.t, Parse_error.t) result
    -> (chunk_to_conv -> 'a)
    -> ('a res, Conv_error.t) result
end

module type Mode = sig
  type parsed_sexp
  type 'a res
  type chunk_to_conv

  val apply_f : parsed_sexp -> f:(chunk_to_conv -> 'r) -> 'r res
  val find : Positions.t -> parsed_sexp -> sub:Sexp.t -> Positions.range option
end

module type Conv = sig
  module type Mode = Mode
  module type S = S

  module Make
      (Mode : Mode)
      (Sexp_parser : Parser.S with type parsed_value = Mode.parsed_sexp)
      (Positions_parser : Parser.S with type parsed_value = Positions.t) :
    S
    with type parsed_sexp := Mode.parsed_sexp
    with type chunk_to_conv := Mode.chunk_to_conv
    with type 'a res := 'a Mode.res
end
