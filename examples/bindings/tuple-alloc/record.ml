type t = {
  v1: int;
  v2: int;
  res: int32;
}
 
external add_numbers: int -> int -> t = "caml_add_numbers"

let () = 
  let v = add_numbers 10 15 in
  Printf.printf "From OCaml: %d+%d=%ld\n" v.v1 v.v2 v.res
