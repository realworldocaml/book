open! Core_kernel

let%test_unit "negation flips the sign" =
  Quickcheck.test ~sexp_of:[%sexp_of: int]
    Int.gen
    ~f:(fun x ->
        assert (Int.sign (Int.neg x) = Sign.flip (Int.sign x)))
