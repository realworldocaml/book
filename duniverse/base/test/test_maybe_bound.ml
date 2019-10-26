open! Import
open! Maybe_bound

let%test_unit "bounds_crossed" =
  let a, b, c, d = Incl 1, Excl 1, Incl 3, Excl 3 in
  let cases = [
    a, a, false;
    a, b, false;
    a, c, false;
    a, d, false;
    b, a, false;
    b, b, false;
    b, c, false;
    b, d, false;
    c, a, true;
    c, b, true;
    c, c, false;
    c, d, false;
    d, a, true;
    d, b, true;
    d, c, false;
    d, d, false;
  ] in
  List.iter cases ~f:(fun (lower, upper, expect) ->
    let actual = bounds_crossed ~lower ~upper ~compare in
    assert ([%compare.equal: bool] expect actual));
;;

let%test_module "is_lower_bound" =
  (module struct
    let compare = Int.compare

    let%test _ = is_lower_bound Unbounded ~of_:Int.min_value ~compare

    let%test _ = not (is_lower_bound (Incl 2) ~of_:1 ~compare)
    let%test _ =      is_lower_bound (Incl 2) ~of_:2 ~compare
    let%test _ =      is_lower_bound (Incl 2) ~of_:3 ~compare

    let%test _ = not (is_lower_bound (Excl 2) ~of_:1 ~compare)
    let%test _ = not (is_lower_bound (Excl 2) ~of_:2 ~compare)
    let%test _ =      is_lower_bound (Excl 2) ~of_:3 ~compare
  end)

let%test_module "is_upper_bound" =
  (module struct
    let compare = Int.compare

    let%test _ = is_upper_bound Unbounded ~of_:Int.max_value ~compare

    let%test _ =      is_upper_bound (Incl 2) ~of_:1 ~compare
    let%test _ =      is_upper_bound (Incl 2) ~of_:2 ~compare
    let%test _ = not (is_upper_bound (Incl 2) ~of_:3 ~compare)

    let%test _ =      is_upper_bound (Excl 2) ~of_:1 ~compare
    let%test _ = not (is_upper_bound (Excl 2) ~of_:2 ~compare)
    let%test _ = not (is_upper_bound (Excl 2) ~of_:3 ~compare)
  end)

let%test_module "check_range" =
  (module struct
    let compare = Int.compare

    let tests (lower, upper) cases =
      List.iter cases ~f:(fun (n, comparison) ->
        [%test_result: interval_comparison]
          ~expect:comparison
          (compare_to_interval_exn n ~lower ~upper ~compare);
        [%test_result: bool]
          ~expect:(match comparison with In_range -> true | _ -> false)
          (interval_contains_exn n ~lower ~upper ~compare))

    let%test_unit _ =
      tests (Unbounded, Unbounded)
        [ (Int.min_value, In_range)
        ; (0,             In_range)
        ; (Int.max_value, In_range)
        ]

    let%test_unit _ =
      tests (Incl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Incl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]
  end)

