open! Import
open! Hash_set

let%test_module "Set Intersection" =
  (module struct
    let run_test first_contents second_contents ~expect =
      let of_list lst =
        let s = create (module String) in
        List.iter lst ~f:(add s);
        s
      in
      let s1 = of_list first_contents in
      let s2 = of_list second_contents in
      let expect = of_list expect in
      let result = inter s1 s2 in
      iter result ~f:(fun x -> assert (mem expect x));
      iter expect ~f:(fun x -> assert (mem result x));
      let equal x y = 0 = String.compare x y in
      assert (List.equal equal (to_list result) (to_list expect));
      assert (length result = length expect);
      (* Make sure the sets are unmodified by the inter *)
      assert (List.length first_contents = length s1);
      assert (List.length second_contents = length s2)
    ;;

    let%test_unit "First smaller" =
      run_test [ "0"; "3"; "99" ] [ "0"; "1"; "2"; "3" ] ~expect:[ "0"; "3" ]
    ;;

    let%test_unit "Second smaller" =
      run_test [ "a"; "b"; "c"; "d" ] [ "b"; "d" ] ~expect:[ "b"; "d" ]
    ;;

    let%test_unit "No intersection" =
      run_test ~expect:[] [ "a"; "b"; "c"; "d" ] [ "1"; "2"; "3"; "4" ]
    ;;
  end)
;;

let%expect_test "sexp" =
  let ints = List.init 20 ~f:(fun x -> x * x) in
  let int_hash_set = Hash_set.of_list (module Int) ints in
  print_s [%sexp (int_hash_set : int Hash_set.t)];
  [%expect {| (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361) |}];
  let strs = List.init 20 ~f:(fun x -> Int.to_string x) in
  let str_hash_set = Hash_set.of_list (module String) strs in
  print_s [%sexp (str_hash_set : string Hash_set.t)];
  [%expect {| (0 1 10 11 12 13 14 15 16 17 18 19 2 3 4 5 6 7 8 9) |}]
;;

let%expect_test "to_array" =
  let empty_array = to_array (Hash_set.of_list (module Int) []) in
  print_s [%sexp (empty_array : int Array.t)];
  [%expect {| () |}];
  let array_from_to_array = to_array (Hash_set.of_list (module Int) [ 1; 2; 3; 4; 5 ]) in
  print_s [%sexp (array_from_to_array : int Array.t)];
  [%expect {| (1 3 2 4 5) |}];
  let array_via_to_list =
    to_list (Hash_set.of_list (module Int) [ 1; 2; 3; 4; 5 ]) |> Array.of_list
  in
  print_s [%sexp (array_via_to_list : int Array.t)];
  [%expect {| (1 3 2 4 5) |}]
;;

let%expect_test "union" =
  let print_union s1 s2 =
    let s1 = Hash_set.of_list (module Int) s1 in
    let s2 = Hash_set.of_list (module Int) s2 in
    print_s [%sexp (Hash_set.union s1 s2 : int Hash_set.t)]
  in
  print_union [ 0; 1; 2 ] [ 3; 4; 5 ];
  [%expect {| (0 1 2 3 4 5) |}];
  print_union [ 0; 1; 2 ] [ 1; 2; 3 ];
  [%expect {| (0 1 2 3) |}]
;;
