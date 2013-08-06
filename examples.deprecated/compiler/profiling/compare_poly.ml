let cmp a b =
  if a > b then a else b

let () = ignore(cmp 1 2); ignore(cmp Sys.argv.(0) "b")
