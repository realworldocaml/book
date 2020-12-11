open! Base
open! Import

let%expect_test "[Base.Hash.state] is still immediate" =
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (Base.Hash.create ())));
  [%expect {| |}]

let%expect_test _ =
  print_s [%sexp (Caml.Obj.is_int (Caml.Obj.repr (Base.Hash.create ~seed:1 ())) : bool)];
  [%expect {| true |}];
;;
