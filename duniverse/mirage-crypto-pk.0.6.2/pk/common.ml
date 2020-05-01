let rec until p f = let r = f () in if p r then r else until p f

let guard p err = if p then Ok () else Error err

(* The Sexplib hack... *)
module Z_sexp = struct
  type t = Z.t

  open Sexplib.Conv
  let sexp_of_t z = sexp_of_string (Z.to_string z)
  let t_of_sexp s = Z.of_string (string_of_sexp s)
end
