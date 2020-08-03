(* Check that ignored attributes inside dropped tests do not trigger an error *)

let%test_module _ =
  (module struct
    [@@@attribute_not_handled_by_anything]
  end)

