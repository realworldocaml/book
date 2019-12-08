let () =
  print_endline "before";
  (try
    print_endline "abc";
    print_endline "def"
  with _ -> print_endline "ABC"; print_endline "DEF");
  print_endline "after"

let () =
  print_endline "before";
  (try
    print_endline "abc"
  with _ -> print_endline "ABC");
  print_endline "after"
