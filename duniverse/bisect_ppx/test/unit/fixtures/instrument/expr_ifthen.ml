let () =
  if true then
    print_endline "abc"
  else
    print_endline "def"

let () =
  if true then
    (print_string "abc"; print_newline ())
  else
    (print_string "def"; print_newline ())

let () =
  if true then
    (print_string "abc"; print_newline ())
