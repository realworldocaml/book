open! Import

include Invariant_intf

let raise_s = Error.raise_s

let invariant here t sexp_of_t f : unit =
  try
    f ()
  with exn ->
    raise_s
      (Sexp.message "invariant failed"
         [ ""   , Source_code_position0.sexp_of_t here
         ; "exn", sexp_of_exn exn
         ; ""   , sexp_of_t t ])
;;

let check_field t f field =
  try
    f (Field.get field t)
  with exn ->
    raise_s (Sexp.message "problem with field"
               [ "field", sexp_of_string (Field.name field)
               ; "exn"  , sexp_of_exn exn
               ])
;;
