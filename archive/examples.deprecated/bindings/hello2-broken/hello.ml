external add_numbers: int -> int -> int = "caml_add_numbers"
let () = Printf.printf "From OCaml: %d\n" (add_numbers 10 15)
