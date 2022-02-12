(* Rewrite Pandoc notes to LaTeX highlight boxes *)
let () =
  let j = Yojson.Basic.from_channel stdin in
  let p = Pandoc.of_json j in
  let tex_block f  = Printf.ksprintf (fun b -> Pandoc.RawBlock ("tex", b)) f in
  let block = function
    | Pandoc.Div ((i,c,attr),b) as d -> begin
       match List.assoc_opt "data-type" attr with
       | Some note ->
           let bl =
               tex_block "\\begin{%sBox}" note :: b @
             [ tex_block "\\end{%sBox}" note ] in
           Some [Pandoc.Div ((i,c,attr), bl)]
       | None -> begin
           List.assoc_opt "text-align" attr |> function
           | Some note ->
              let bl =
                tex_block "\\begin{flush%s}" note :: b @
                [ tex_block "\\end{flush%s}" note ] in
              Some [Pandoc.Div ((i,c,attr), bl)]
           | None -> Some [d]
       end
    end 
    | _ -> None in
  let inline = function
    (* handle xrefs like [Foo](bar.html#label){data-type="xref"} *)
    | Pandoc.Link ((_i,_c,attr),_body,(url,_title)) as l -> begin
      List.assoc_opt "data-type" attr |> function
      | Some "xref" -> (* this is a cross-reference *)
          let url = Scanf.sscanf url "%s@#%s" (fun _ t -> t) in
          Some [Pandoc.RawInline("latex", (Printf.sprintf "\\autoref{%s} (\\nameref{%s})" url url))]
      | Some _ | None -> Some [l]
    end
    | n -> Some [n] in
  let p = Pandoc.map ~block ~inline p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
