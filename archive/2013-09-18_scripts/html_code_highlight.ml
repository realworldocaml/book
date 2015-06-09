open Core.Std

let subst_with_code_frag _attr (c:Cow.Xml.t) : Cow.Xml.t =
  try
    let t = Code_frag.of_string (Cow.Html.to_string c) in
    In_channel.read_all (sprintf "code/_build/%s.%d.html" t.Code_frag.name t.Code_frag.part)
    |> Cow.Html.of_string
  with exn ->
    eprintf "WARNING bare <pre> found. This should be turned into a ```frag:\n %s\n%!" (Exn.to_string exn);
    Code_frag.wrap_in_pretty_box ~part:0 "Unknown" "" c

let () =
  try
    Xml_tree.read_document In_channel.stdin
    |> fun (dtd, doc) -> Xml_tree.map ~tag:"pre" ~f:subst_with_code_frag doc
    |> fun doc -> Xml_tree.write_document Out_channel.stdout dtd doc
  with 
  | Xmlm.Error (_p,e) ->
    print_endline (Xmlm.error_message e)
