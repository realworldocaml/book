open! Base
open Expect_test_helpers_core

let t1 = Type_equal.Id.create ~name:"t1" [%sexp_of: _]

let%expect_test "Type_equal.Id.to_sexp allocation" =
  require_no_allocation [%here] (fun () ->
    ignore (Type_equal.Id.to_sexp t1 : 'a -> Sexp.t))
;;
