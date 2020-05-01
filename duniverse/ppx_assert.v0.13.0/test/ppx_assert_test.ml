open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib
open Conv

module Sexp = struct
  include Sexp
  let of_string = Sexplib.Sexp.of_string
end

let () =
  Printexc.register_printer (fun exc ->
    match sexp_of_exn_opt exc with
    | None -> None
    | Some sexp ->
      Some (Sexp.to_string_hum ~indent:2 sexp))

let re_pos = Str.regexp "[a-zA-Z0-9_/]*\\.ml:[0-9]*:[0-9]*"
let hide_position_details str =
  Str.global_replace re_pos "F:L:C" str

let test_exn exn str =
  let sexp_str = Sexp.of_string str in
  let sexp_exn =
    match sexp_of_exn_opt exn with
    | None -> assert false
    | Some sexp ->
      Sexp.of_string (hide_position_details (Sexp.to_string sexp))
  in
  [%test_eq: Sexp.t] sexp_exn sexp_str

let%test_unit _ = [%test_eq: int] 1 1
let%test _ =
  try [%test_eq: int * int] ~here:[[%here]] ~message:"int tuple" (5, 5) (5, 6); false
  with e ->
    test_exn e "(runtime.ml.E \"int tuple: comparison failed\"
                 ((5 5) vs (5 6)
                  (Loc F:L:C)
                  (Stack (F:L:C))))";
    true

let%test_unit _ = [%test_result: int] (1 + 2) ~message:"size" ~expect:3
let%test _ =
  try [%test_result: int * int] ~here:[[%here]] (5, 5) ~expect:(5, 6); false
  with e ->
    test_exn e "(runtime.ml.E \"got unexpected result\"
                 ((expected (5 6)) (got (5 5))
                  (Loc F:L:C)
                  (Stack (F:L:C))))";
    true

let%test _ =
  try [%test_pred: float] ~message:"price" ((=) 3.) 5.; false
  with e ->
    test_exn e "(runtime.ml.E \"price: predicate failed\"
                 ((Value 5) (Loc F:L:C)))";
    true

let%test_unit _ = [%test_eq: int] ~equal:(fun x y -> x mod 2 = y mod 2) 4 6

(* An example where the list of positions that <:test_eq< ... >> takes comes in handy,
   because the position of <:test_eq< ... >> itself is not very informative. *)
let test_is_zero ~here x = [%test_eq: int] 0 x ~here:([%here] :: here)

let test_odds n ~here =
  for i = 0 to n do
    let odd = 2 * i + 1 in
    test_is_zero ~here:([%here] :: here) (odd - odd)
  done

let test_evens n ~here =
  for i = 0 to n do
    let even = 2 * i in
    test_is_zero ~here:([%here] :: here) (even - even)
  done

let test_all n =
  test_odds n ~here:[[%here]];
  test_evens n ~here:[[%here]]

let%test_unit _ = test_all 10

let _ = ([%test_result: int] : [%test_result: int])
let _ = ([%test_eq: int] : [%test_eq: int])
let _ = ([%test_pred: int] : [%test_pred: int])
