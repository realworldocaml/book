include Int64

exception DoesNotFit

let to_int n =
  if n < of_int Stdlib.min_int || n > of_int Stdlib.max_int then
    raise DoesNotFit
  else to_int n

let t_of_sexp s = of_string @@ Int64.to_string @@ Sexplib.Conv.int64_of_sexp s

let sexp_of_t n = Sexplib.Conv.sexp_of_int64 @@ Int64.of_string @@ to_string n

let size_in_bytes = 8
