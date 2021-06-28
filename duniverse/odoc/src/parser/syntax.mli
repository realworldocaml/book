val parse :
  Odoc_model.Error.warning_accumulator ->
  (Token.t Odoc_model.Location_.with_location) Stream.t ->
    Ast.docs
