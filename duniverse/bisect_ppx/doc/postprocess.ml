open Soup

let () =
  (* Read a template containing surrounding <html>, <head>, and <body> tags. *)
  let soup = read_file "doc/template.html" |> parse in

  (* Read OMD output from STDIN, and insert it into the template. *)
  read_channel stdin
  |> parse |> children |> iter (insert_before (soup $ "#css-attribution"));

  (* OMD messes up the badge links in the header. Replace it. *)
  create_element ~inner_text:"Bisect_ppx" "h1" |> replace (soup $ "h1");

  (* OMD doesn't handle an image wrapped in an anchor well. Fix the main
     image. *)
  let gif = soup $ "p img[src]" in
  let container = R.parent gif in
  let link = container $ "a" in

  clear container;
  clear link;
  append_child container link;
  append_child link gif;

  (* OMD seems to interpret <br> tags as text, and turns them into
     paragraphs. Fix that. *)
  soup $$ "p:contains(\"<br\")"
  |> iter (fun e -> create_element "br" |> replace e);

  (* Write the modified HTML to STDOUT. *)
  soup |> to_string |> write_channel stdout
