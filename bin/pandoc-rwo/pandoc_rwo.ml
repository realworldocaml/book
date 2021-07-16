(* Rewrite Pandoc notes to LaTeX highlight boxes *)
let () =
  let j = Yojson.Basic.from_channel stdin in
  let p = Pandoc.of_json j in
  let tex_block f  = Printf.ksprintf (fun b -> Pandoc.RawBlock ("tex", b)) f in
  let f = function
    | Pandoc.Div ((i,c,attr),b) as d -> begin
       List.assoc_opt "data-type" attr |> function
       | Some note ->
          let bl =
              tex_block "\\begin{%sBox}" note :: b @
            [ tex_block "\\end{%sBox}" note ] in
          Some [Pandoc.Div ((i,c,attr), bl)]
       | None -> Some [d]
    end 
    | _ -> None in
  let p = Pandoc.map ~block:f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
