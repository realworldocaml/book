(* Rewrite cross-references, untouch notes *)
let () =
  let j = Yojson.Basic.from_channel stdin in
  let p = Pandoc.of_json j in
  let inline = function
    (* handle xrefs like [Foo](bar.html#label){data-type="xref"} *)
    | Pandoc.Link ((i,c,attr),body,(url,title)) as l -> begin
      List.assoc_opt "data-type" attr |> function
      | Some "xref" -> (* this is a cross-reference *)
          let url = Scanf.sscanf url "%s@#%s" (fun _ t -> t) in
          Some [Pandoc.Link ((i,c,attr),body,("#" ^ url,title))]
      | Some _ | None -> Some [l]
    end
    | n -> Some [n] in
  let p = Pandoc.map ~inline p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
