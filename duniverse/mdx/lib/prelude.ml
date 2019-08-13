let env_and_file f =
  match Astring.String.cut ~sep:":" f with
  | None        -> None, f
  | Some (e, f) ->
    if Astring.String.exists ((=) ' ') e
    then None  , f
    else Some e, f
