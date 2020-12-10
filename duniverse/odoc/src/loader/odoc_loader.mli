open Result

val read_string :
  Odoc_model.Paths.Identifier.LabelParent.t ->
  Location.t ->
  string ->
    (Odoc_model.Comment.docs_or_stop, Odoc_model.Error.t) result

val read_cmti :
  make_root:(module_name:string -> digest:Digest.t -> Odoc_model.Root.t) ->
  filename:string ->
    (Odoc_model.Lang.Compilation_unit.t, Odoc_model.Error.t) result

val read_cmt :
  make_root:(module_name:string -> digest:Digest.t -> Odoc_model.Root.t) ->
  filename:string ->
    (Odoc_model.Lang.Compilation_unit.t, Odoc_model.Error.t) result

val read_cmi :
  make_root:(module_name:string -> digest:Digest.t -> Odoc_model.Root.t) ->
  filename:string ->
    (Odoc_model.Lang.Compilation_unit.t, Odoc_model.Error.t) result
