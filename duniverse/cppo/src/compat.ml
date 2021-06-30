if Filename.check_suffix Sys.argv.(1) ".ml" &&
   Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) < (4, 03) then
  print_endline "\
module String = struct
  include String
  let capitalize_ascii = capitalize
end"
