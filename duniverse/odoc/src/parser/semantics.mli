val ast_to_comment :
  Odoc_model.Error.warning_accumulator ->
  sections_allowed:Ast.sections_allowed ->
  parent_of_sections:Odoc_model.Paths.Identifier.LabelParent.t ->
  Ast.docs ->
    Odoc_model.Comment.docs
