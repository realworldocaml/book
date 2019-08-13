open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib
open Ppx_sexp_conv_lib.Conv

let () =
  Printexc.register_printer (fun exc ->
    match sexp_of_exn_opt exc with
    | None -> None
    | Some sexp ->
      Some (Sexp.to_string_hum ~indent:2 sexp))

let%bench "test_eq" =
  [%test_eq: int] 0 0

let%bench "test_pred" =
  [%test_pred: int] (fun i -> i = 0) 0

let%bench "test_result" =
  [%test_result: int] ~expect:0 0
