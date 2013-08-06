external add_numbers: int -> int -> int * int * int32 = "caml_add_numbers"
let () = 
  let (l,r,v) = add_numbers 10 15 in
  Printf.printf "From OCaml: %d+%d=%ld\n" l r v
