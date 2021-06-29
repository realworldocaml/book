open! Import
open! Array

let%test_module "Binary_searchable" =
  (module Test_binary_searchable.Test1 (struct
       include Array

       module For_test = struct
         let of_array = Fn.id
       end
     end))
;;

let%test_module "Blit" =
  (module Test_blit.Test1
       (struct
         type 'a z = 'a

         include Array

         let create_bool ~len = create ~len false
       end)
       (Array))
;;

let%test_module "Sort" =
  (module struct
    open Private.Sort

    let%test_module "Intro_sort.five_element_sort" =
      (module struct
        (* run [five_element_sort] on all permutations of an array of five elements *)

        let rec sprinkle x xs =
          (x :: xs)
          ::
          (match xs with
           | [] -> []
           | x' :: xs' -> List.map (sprinkle x xs') ~f:(fun sprinkled -> x' :: sprinkled))
        ;;

        let rec permutations = function
          | [] -> [ [] ]
          | x :: xs ->
            List.concat_map (permutations xs) ~f:(fun perms -> sprinkle x perms)
        ;;

        let all_perms = permutations [ 1; 2; 3; 4; 5 ]

        let%test _ = List.length all_perms = 120
        let%test _ = not (List.contains_dup ~compare:[%compare: int list] all_perms)

        let%test _ =
          List.for_all all_perms ~f:(fun l ->
            let arr = Array.of_list l in
            Intro_sort.five_element_sort arr ~compare:[%compare: int] 0 1 2 3 4;
            [%compare.equal: int t] arr [| 1; 2; 3; 4; 5 |])
        ;;
      end)
    ;;

    module Test (M : Private.Sort.Sort) = struct
      let random_data ~length ~range =
        let arr = Array.create ~len:length 0 in
        for i = 0 to length - 1 do
          arr.(i) <- Random.int range
        done;
        arr
      ;;

      let assert_sorted arr =
        M.sort arr ~left:0 ~right:(Array.length arr - 1) ~compare:[%compare: int];
        let len = Array.length arr in
        let rec loop i prev =
          if i = len
          then true
          else if arr.(i) < prev
          then false
          else loop (i + 1) arr.(i)
        in
        loop 0 (-1)
      ;;

      let%test _ = assert_sorted (random_data ~length:0 ~range:100)
      let%test _ = assert_sorted (random_data ~length:1 ~range:100)
      let%test _ = assert_sorted (random_data ~length:100 ~range:1_000)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:1)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:10)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:1_000_000)
    end

    let%test_module _ = (module Test (Insertion_sort))
    let%test_module _ = (module Test (Heap_sort))
    let%test_module _ = (module Test (Intro_sort))

    let%expect_test "Array.sort [||] only allocates when computing bounds" =
      require_allocation_does_not_exceed (Minor_words 3) [%here] (fun () ->
        Array.sort ~compare:Int.compare [||]);
      [%expect {||}]
    ;;

    let%expect_test "Array.sort [| 5; 2; 3; 4; 1 |] only allocates when computing bounds"
      =
      let arr = [| 5; 2; 3; 4; 1 |] in
      require_allocation_does_not_exceed (Minor_words 3) [%here] (fun () ->
        Array.sort ~compare:Int.compare arr);
      [%expect {||}]
    ;;
  end)
;;

let%test _ = is_sorted [||] ~compare:[%compare: int]
let%test _ = is_sorted [| 0 |] ~compare:[%compare: int]
let%test _ = is_sorted [| 0; 1; 2; 2; 4 |] ~compare:[%compare: int]
let%test _ = not (is_sorted [| 0; 1; 2; 3; 2 |] ~compare:[%compare: int])

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      assert (Bool.equal expect (is_sorted_strictly (of_list t) ~compare:[%compare: int])))
    [ [], true
    ; [ 1 ], true
    ; [ 1; 2 ], true
    ; [ 1; 1 ], false
    ; [ 2; 1 ], false
    ; [ 1; 2; 3 ], true
    ; [ 1; 1; 3 ], false
    ; [ 1; 2; 2 ], false
    ]
;;

let%test _ = foldi [||] ~init:13 ~f:(fun _ _ _ -> failwith "bad") = 13
let%test _ = foldi [| 13 |] ~init:17 ~f:(fun i ac x -> ac + i + x) = 30
let%test _ = foldi [| 13; 17 |] ~init:19 ~f:(fun i ac x -> ac + i + x) = 50
let%test _ = counti [| 0; 1; 2; 3; 4 |] ~f:(fun idx x -> idx = x) = 5
let%test _ = counti [| 0; 1; 2; 3; 4 |] ~f:(fun idx x -> idx = 4 - x) = 1

let%test_unit _ =
  for i = 0 to 5 do
    let l1 = List.init i ~f:Fn.id in
    let l2 = List.rev (to_list (of_list_rev l1)) in
    assert ([%compare.equal: int list] l1 l2)
  done
;;

let%test_unit _ =
  [%test_result: int array]
    (filter_opt [| Some 1; None; Some 2; None; Some 3 |])
    ~expect:[| 1; 2; 3 |]
;;

let%test_unit _ =
  [%test_result: int array] (filter_opt [| Some 1; None; Some 2 |]) ~expect:[| 1; 2 |]
;;

let%test_unit _ = [%test_result: int array] (filter_opt [| Some 1 |]) ~expect:[| 1 |]
let%test_unit _ = [%test_result: int array] (filter_opt [| None |]) ~expect:[||]
let%test_unit _ = [%test_result: int array] (filter_opt [||]) ~expect:[||]

let%expect_test _ =
  print_s ([%sexp_of: int array] (map2_exn [| 1; 2; 3 |] [| 2; 3; 4 |] ~f:( + )));
  [%expect {| (3 5 7) |}]
;;

let%expect_test "map2_exn raise" =
  require_does_raise [%here] (fun () -> map2_exn [| 1; 2; 3 |] [| 2; 3; 4; 5 |] ~f:( + ));
  [%expect {| (Invalid_argument "length mismatch in Array.map2_exn: 3 <> 4") |}]
;;

let%test_unit _ =
  [%test_result: int]
    (fold2_exn [||] [||] ~init:13 ~f:(fun _ -> failwith "fail"))
    ~expect:13
;;

let%test_unit _ =
  [%test_result: (int * string) list]
    (fold2_exn [| 1 |] [| "1" |] ~init:[] ~f:(fun ac a b -> (a, b) :: ac))
    ~expect:[ 1, "1" ]
;;

let%test_unit _ =
  [%test_result: int array] (filter [| 0; 1 |] ~f:(fun n -> n < 2)) ~expect:[| 0; 1 |]
;;

let%test_unit _ =
  [%test_result: int array] (filter [| 0; 1 |] ~f:(fun n -> n < 1)) ~expect:[| 0 |]
;;

let%test_unit _ =
  [%test_result: int array] (filter [| 0; 1 |] ~f:(fun n -> n < 0)) ~expect:[||]
;;

let%test_unit _ = [%test_result: bool] (exists [||] ~f:(fun _ -> true)) ~expect:false

let%test_unit _ =
  [%test_result: bool] (exists [| 0; 1; 2; 3 |] ~f:(fun x -> 4 = x)) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (exists [| 0; 1; 2; 3 |] ~f:(fun x -> 2 = x)) ~expect:true
;;

let%test_unit _ = [%test_result: bool] (existsi [||] ~f:(fun _ _ -> true)) ~expect:false

let%test_unit _ =
  [%test_result: bool] (existsi [| 0; 1; 2; 3 |] ~f:(fun i x -> i <> x)) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (existsi [| 0; 1; 3; 3 |] ~f:(fun i x -> i <> x)) ~expect:true
;;

let%test_unit _ = [%test_result: bool] (for_all [||] ~f:(fun _ -> false)) ~expect:true

let%test_unit _ =
  [%test_result: bool] (for_all [| 1; 2; 3 |] ~f:Int.is_positive) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool] (for_all [| 0; 1; 3; 3 |] ~f:Int.is_positive) ~expect:false
;;

let%test_unit _ = [%test_result: bool] (for_alli [||] ~f:(fun _ _ -> false)) ~expect:true

let%test_unit _ =
  [%test_result: bool] (for_alli [| 0; 1; 2; 3 |] ~f:(fun i x -> i = x)) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool] (for_alli [| 0; 1; 3; 3 |] ~f:(fun i x -> i = x)) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (exists2_exn [||] [||] ~f:(fun _ _ -> true)) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (exists2_exn [| 0; 2; 4; 6 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x <> y))
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (exists2_exn [| 0; 2; 4; 8 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x <> y))
    ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (exists2_exn [| 2; 2; 4; 6 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x <> y))
    ~expect:true
;;

let%test_unit _ =
  [%test_result: bool] (for_all2_exn [||] [||] ~f:(fun _ _ -> false)) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (for_all2_exn [| 0; 2; 4; 6 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x = y))
    ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (for_all2_exn [| 0; 2; 4; 8 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x = y))
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (for_all2_exn [| 2; 2; 4; 6 |] [| 0; 2; 4; 6 |] ~f:(fun x y -> x = y))
    ~expect:false
;;

let%test_unit _ = [%test_result: bool] (equal ( = ) [||] [||]) ~expect:true
let%test_unit _ = [%test_result: bool] (equal ( = ) [| 1 |] [| 1 |]) ~expect:true
let%test_unit _ = [%test_result: bool] (equal ( = ) [| 1; 2 |] [| 1; 2 |]) ~expect:true
let%test_unit _ = [%test_result: bool] (equal ( = ) [||] [| 1 |]) ~expect:false
let%test_unit _ = [%test_result: bool] (equal ( = ) [| 1 |] [||]) ~expect:false
let%test_unit _ = [%test_result: bool] (equal ( = ) [| 1 |] [| 1; 2 |]) ~expect:false
let%test_unit _ = [%test_result: bool] (equal ( = ) [| 1; 2 |] [| 1; 3 |]) ~expect:false

let%test_unit _ =
  [%test_result: (int * int) option]
    (findi [| 1; 2; 3; 4 |] ~f:(fun i x -> i = 2 * x))
    ~expect:None
;;

let%test_unit _ =
  [%test_result: (int * int) option]
    (findi [| 1; 2; 1; 4 |] ~f:(fun i x -> i = 2 * x))
    ~expect:(Some (2, 1))
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [| 0; 5; 2; 1; 4 |] ~f:(fun i x -> if i = x then Some (i + x) else None))
    ~expect:(Some 0)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [| 3; 5; 2; 1; 4 |] ~f:(fun i x -> if i = x then Some (i + x) else None))
    ~expect:(Some 4)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [| 3; 5; 1; 1; 4 |] ~f:(fun i x -> if i = x then Some (i + x) else None))
    ~expect:(Some 8)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [| 3; 5; 1; 1; 2 |] ~f:(fun i x -> if i = x then Some (i + x) else None))
    ~expect:None
;;

let%test_unit _ =
  List.iter
    ~f:(fun (l, expect) ->
      let t = of_list l in
      assert (Poly.equal expect (find_consecutive_duplicate t ~equal:Poly.equal)))
    [ [], None
    ; [ 1 ], None
    ; [ 1; 1 ], Some (1, 1)
    ; [ 1; 2 ], None
    ; [ 1; 2; 1 ], None
    ; [ 1; 2; 2 ], Some (2, 2)
    ; [ 1; 1; 2; 2 ], Some (1, 1)
    ]
;;

let%test_unit _ = [%test_result: int option] (random_element [||]) ~expect:None
let%test_unit _ = [%test_result: int option] (random_element [| 0 |]) ~expect:(Some 0)

let%test_unit _ =
  List.iter [ [||]; [| 1 |]; [| 1; 2; 3; 4; 5 |] ] ~f:(fun t ->
    [%test_result: int array] (Sequence.to_array (to_sequence t)) ~expect:t)
;;

let test_fold_map array ~init ~f ~expect =
  [%test_result: int array] (folding_map array ~init ~f) ~expect:(snd expect);
  [%test_result: int * int array] (fold_map array ~init ~f) ~expect
;;

let test_fold_mapi array ~init ~f ~expect =
  [%test_result: int array] (folding_mapi array ~init ~f) ~expect:(snd expect);
  [%test_result: int * int array] (fold_mapi array ~init ~f) ~expect
;;

let%test_unit _ =
  test_fold_map
    [| 1; 2; 3; 4 |]
    ~init:0
    ~f:(fun acc x ->
      let y = acc + x in
      y, y)
    ~expect:(10, [| 1; 3; 6; 10 |])
;;

let%test_unit _ =
  test_fold_map
    [||]
    ~init:0
    ~f:(fun acc x ->
      let y = acc + x in
      y, y)
    ~expect:(0, [||])
;;

let%test_unit _ =
  test_fold_mapi
    [| 1; 2; 3; 4 |]
    ~init:0
    ~f:(fun i acc x ->
      let y = acc + (i * x) in
      y, y)
    ~expect:(20, [| 0; 2; 8; 20 |])
;;

let%test_unit _ =
  test_fold_mapi
    [||]
    ~init:0
    ~f:(fun i acc x ->
      let y = acc + (i * x) in
      y, y)
    ~expect:(0, [||])
;;

let%test "equal does not allocate" =
  let arr1 = [| 1; 2; 3; 4 |] in
  let arr2 = [| 1; 2; 4; 3 |] in
  require_no_allocation [%here] (fun () -> not (equal Int.equal arr1 arr2))
;;

let%test "foldi does not allocate" =
  let arr = [| 1; 2; 3; 4 |] in
  let f i x y = i + x + y in
  require_no_allocation [%here] (fun () -> 16 = foldi ~init:0 ~f arr)
;;
