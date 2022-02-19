let rec until p f = let r = f () in if p r then r else until p f

let guard p err = if p then Ok () else Error err

let ( let* ) = Result.bind

open Sexplib0.Sexp_conv
let sexp_of_z z = sexp_of_string (Z.to_string z)
let z_of_sexp s = Z.of_string (string_of_sexp s)
