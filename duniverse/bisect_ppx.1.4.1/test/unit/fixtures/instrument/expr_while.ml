let () =
  print_endline "before";
  while true do
    print_endline "abc";
    print_endline "def";
    print_endline "ghi"
  done;
  print_endline "after"

let () =
  print_endline "before";
  while true do
    print_endline "abc"
  done;
  print_endline "after"
