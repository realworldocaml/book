let%test_module "overflow_bounds" =
  (module struct
    module Pow_overflow_bounds = Base.Int_math.Private.Pow_overflow_bounds

    let%test _ = Pow_overflow_bounds.overflow_bound_max_int_value = Caml.max_int
    let%test _ = Pow_overflow_bounds.overflow_bound_max_int64_value = Int64.max_int

    module Big_int = struct
      include Big_int

      let ( > ) = gt_big_int
      let ( = ) = eq_big_int
      let ( ^ ) = power_big_int_positive_int
      let ( + ) = add_big_int
      let one = unit_big_int
      let to_string = string_of_big_int
    end

    let test_overflow_table tbl conv max_val =
      assert (Array.length tbl = 64);
      let max_val = conv max_val in
      StdLabels.Array.iteri tbl ~f:(fun i max_base ->
        let max_base = conv max_base in
        let overflows b = Big_int.(b ^ i > max_val) in
        let is_ok =
          if i = 0
          then Big_int.(max_base = max_val)
          else (not (overflows max_base)) && overflows Big_int.(max_base + one)
        in
        if not is_ok
        then
          Base.Printf.failwithf
            "overflow table check failed for %s (index %d)"
            (Big_int.to_string max_base)
            i
            ())
    ;;

    let%test_unit _ =
      test_overflow_table
        Pow_overflow_bounds.int_positive_overflow_bounds
        Big_int.big_int_of_int
        Caml.max_int
    ;;

    let%test_unit _ =
      test_overflow_table
        Pow_overflow_bounds.int64_positive_overflow_bounds
        Big_int.big_int_of_int64
        Int64.max_int
    ;;
  end)
;;
