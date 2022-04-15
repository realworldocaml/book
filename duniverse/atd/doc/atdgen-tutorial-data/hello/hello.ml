open Hello_t
let () =
  let date = { year = 1970; month = 1; day = 1 } in
  print_endline (Hello_j.string_of_date date)
