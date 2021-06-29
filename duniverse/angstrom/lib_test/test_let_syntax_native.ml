open Angstrom

let (_ : int t) =
  let* () = end_of_input in
  return 1

let (_ : int t) =
  let+ (_ : char) = any_char
  and+ (_ : string) = string "foo"
  in
  2
