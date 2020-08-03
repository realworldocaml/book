type input = {
  file : string;
  offset_to_location : int -> Odoc_model.Location_.point;
  warnings : Odoc_model.Error.warning_accumulator;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Odoc_model.Location_.with_location
