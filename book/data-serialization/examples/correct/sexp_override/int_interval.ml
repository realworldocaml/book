type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let create x y =
  if x > y then Empty else Range (x,y)

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  begin match t with
    | Empty -> ()
    | Range (x,y) ->
      if y < x then of_sexp_error "Upper and lower bound of Range swapped" sexp
  end;
  t
