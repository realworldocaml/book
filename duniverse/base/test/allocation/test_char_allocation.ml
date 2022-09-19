open! Base
open Expect_test_helpers_core

let%expect_test _ =
  let x = Sys.opaque_identity 'a' in
  let y = Sys.opaque_identity 'b' in
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (Char.Caseless.equal x y) : bool));
  [%expect {||}]
;;
