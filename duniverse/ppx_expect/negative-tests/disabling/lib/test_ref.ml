type t = Init | Set_by_inline_test
[@@deriving sexp, compare]

let inner = ref Init

let%expect_test _ =
  let module M = struct
    inner := Set_by_inline_test
  end in ()

let%test_unit _ = inner := Set_by_inline_test

let value () = !inner
