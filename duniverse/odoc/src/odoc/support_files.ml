let iter_files f ?(without_theme = false) output_directory =
  let file name content =
    let name =
      Fs.File.create ~directory:output_directory ~name
      |> Fs.File.to_string
    in
    f name content
  in

  if not without_theme then begin
    file "odoc.css" Css_file.content
  end;
  file "highlight.pack.js" Highlight_js.content

let write =
  iter_files begin fun name content ->
    let channel = open_out name in
    output_string channel content;
    close_out channel
  end

let print_filenames =
  iter_files (fun name _content -> print_endline name)
