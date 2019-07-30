(* Reads lines from STDIN. For each line, quotes it as an OCaml string, and
   writes it to STDOUT. This is used to quote CSS and JavaScript for inclusion
   in src/report/reportHTML.ml. Example usage:

     ocaml src/quote_lines.ml < style.css > css.tmp *)

let () =
  try while true do read_line () |> Printf.printf "%S;\n"; done
  with End_of_file -> ()
