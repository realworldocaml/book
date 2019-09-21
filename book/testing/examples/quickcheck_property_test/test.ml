open Core_kernel

let%test_unit "negation flips the sign" =
  Quickcheck.test ~sexp_of:[%sexp_of: int]
    (Int.gen_incl Int.min_value Int.max_value)
    ~f:(fun x ->
        [%test_eq: Sign.t]
          (Int.sign (Int.neg x))
          (Sign.flip (Int.sign x)))
