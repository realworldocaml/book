open Base
open Base_quickcheck

type complex =
  { real : float; imaginary : float }
[@@deriving compare, quickcheck, sexp]

let add x y =
  { real      = x.real      +. y.real
  ; imaginary = x.imaginary +. y.imaginary
  }

let%expect_test "commutativity" =
  Test.run_exn
    (module struct
      type t = complex * complex
      [@@deriving quickcheck, sexp]
    end)
    ~f:(fun (x, y) ->
      [%test_eq: complex]
        (add x y)
        (add y x))
