type t = { location : Loc.span; message : string }

let to_string e =
  let { location; message } = e in
  let location_string =
    if location.start.line = location.end_.line then
      Printf.sprintf "line %i, characters %i-%i" location.start.line
        location.start.column location.end_.column
    else
      Printf.sprintf "line %i, character %i to line %i, character %i"
        location.start.line location.start.column location.end_.line
        location.end_.column
  in
  Printf.sprintf "File \"%s\", %s:\n%s" location.file location_string message

let pp fmt v = Format.fprintf fmt "%s" (to_string v)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let kmake k ?suggestion =
  kasprintf (fun message ->
      match suggestion with
      | None -> k message
      | Some suggestion -> k (message ^ "\nSuggestion: " ^ suggestion))

let make ?suggestion format =
  let k message location = { location; message } in
  kmake k ?suggestion format
