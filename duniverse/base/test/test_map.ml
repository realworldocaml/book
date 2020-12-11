open! Import
open! Map

let%expect_test "Finished_or_unfinished <-> Continue_or_stop" =
  (* These functions are implemented using [Caml.Obj.magic]. It is important to test them
     comprehensively. *)
  List.iter2_exn Continue_or_stop.all Finished_or_unfinished.all ~f:(fun c_or_s f_or_u ->
    print_s [%sexp (c_or_s : Continue_or_stop.t), (f_or_u : Finished_or_unfinished.t)];
    require_equal
      [%here]
      (module Continue_or_stop)
      c_or_s
      (Finished_or_unfinished.to_continue_or_stop f_or_u);
    require_equal
      [%here]
      (module Finished_or_unfinished)
      f_or_u
      (Finished_or_unfinished.of_continue_or_stop c_or_s));
  [%expect {|
    (Continue Finished)
    (Stop Unfinished) |}]
;;

let%test _ =
  invariants (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:(fun x -> x, x))
;;

let%test _ =
  invariants (Poly.of_increasing_iterator_unchecked ~len:20 ~f:(fun x -> x, x))
;;

module M = M

let add12 t = add_exn t ~key:1 ~data:2

type int_map = int Map.M(Int).t [@@deriving compare, hash, sexp]

let%expect_test "[add_exn] success" =
  print_s [%sexp (add12 (empty (module Int)) : int_map)];
  [%expect {| ((1 2)) |}]
;;

let%expect_test "[add_exn] failure" =
  show_raise (fun () -> add12 (add12 (empty (module Int))));
  [%expect {| (raised ("[Map.add_exn] got key already present" (key 1))) |}]
;;

let%expect_test "[add] success" =
  print_s [%sexp (add (empty (module Int)) ~key:1 ~data:2 : int_map Or_duplicate.t)];
  [%expect {| (Ok ((1 2))) |}]
;;

let%expect_test "[add] duplicate" =
  print_s
    [%sexp (add (add12 (empty (module Int))) ~key:1 ~data:2 : int_map Or_duplicate.t)];
  [%expect {| Duplicate |}]
;;

let%expect_test "[Map.of_alist_multi] preserves value ordering" =
  print_s
    [%sexp
      (Map.of_alist_multi (module String) [ "a", 1; "a", 2; "b", 1; "b", 3 ]
       : int list Map.M(String).t)];
  [%expect {|
    ((a (1 2))
     (b (1 3))) |}]
;;

let%expect_test "find_exn" =
  let map = Map.of_alist_exn (module String) [ "one", 1; "two", 2; "three", 3 ] in
  let test_success key =
    require_does_not_raise [%here] (fun () ->
      print_s [%sexp (Map.find_exn map key : int)])
  in
  test_success "one";
  [%expect {| 1 |}];
  test_success "two";
  [%expect {| 2 |}];
  test_success "three";
  [%expect {| 3 |}];
  let test_failure key = require_does_raise [%here] (fun () -> Map.find_exn map key) in
  test_failure "zero";
  [%expect {| (Not_found_s ("Map.find_exn: not found" zero)) |}];
  test_failure "four";
  [%expect {| (Not_found_s ("Map.find_exn: not found" four)) |}]
;;

let%expect_test "[t_of_sexp] error on duplicate" =
  let sexp = Sexplib.Sexp.of_string "((0 a)(1 b)(2 c)(1 d))" in
  (match [%of_sexp: string Map.M(String).t] sexp with
   | t -> print_cr [%here] [%message "did not raise" (t : string Map.M(String).t)]
   | exception (Sexp.Of_sexp_error _ as exn) -> print_s (sexp_of_exn exn)
   | exception exn -> print_cr [%here] [%message "wrong kind of exception" (exn : exn)]);
  [%expect {| (Of_sexp_error "Map.t_of_sexp_direct: duplicate key" (invalid_sexp 1)) |}]
;;

let%expect_test "combine_errors" =
  let test list =
    let input =
      list
      |> List.map ~f:(Result.map_error ~f:Error.of_string)
      |> List.mapi ~f:(fun k x -> Int.succ k, x)
      |> Map.of_alist_exn (module Int)
    in
    let output = Map.combine_errors input in
    print_s [%sexp (output : string Map.M(Int).t Or_error.t)]
  in
  (* empty *)
  test [];
  [%expect {| (Ok ()) |}];
  (* singletons *)
  test [ Ok "one" ];
  test [ Error "one" ];
  [%expect {|
    (Ok ((1 one)))
    (Error ((1 one))) |}];
  (* multiple ok *)
  test [ Ok "one"; Ok "two"; Ok "three" ];
  [%expect {|
    (Ok (
      (1 one)
      (2 two)
      (3 three))) |}];
  (* multiple errors *)
  test [ Error "one"; Error "two"; Error "three" ];
  [%expect {|
    (Error (
      (1 one)
      (2 two)
      (3 three))) |}];
  (* one error among oks *)
  test [ Error "one"; Ok "two"; Ok "three" ];
  test [ Ok "one"; Error "two"; Ok "three" ];
  test [ Ok "one"; Ok "two"; Error "three" ];
  [%expect {|
    (Error ((1 one)))
    (Error ((2 two)))
    (Error ((3 three))) |}];
  (* one ok among errors *)
  test [ Ok "one"; Error "two"; Error "three" ];
  test [ Error "one"; Ok "two"; Error "three" ];
  test [ Error "one"; Error "two"; Ok "three" ];
  [%expect
    {|
    (Error (
      (2 two)
      (3 three)))
    (Error (
      (1 one)
      (3 three)))
    (Error (
      (1 one)
      (2 two))) |}]
;;

module Poly = struct
  let%test _ = length Poly.empty = 0

  let%test _ =
    let a = Poly.of_alist_exn [] in
    Poly.equal Base.Poly.equal a Poly.empty
  ;;

  let%test _ =
    let a = Poly.of_alist_exn [ "a", 1 ] in
    let b = Poly.of_alist_exn [ 1, "b" ] in
    length a = length b
  ;;
end

let%test_module "[symmetric_diff]" =
  (module struct
    let%expect_test "examples" =
      let test alist1 alist2 =
        Map.symmetric_diff
          ~data_equal:Int.equal
          (Map.of_alist_exn (module String) alist1)
          (Map.of_alist_exn (module String) alist2)
        |> Sequence.to_list
        |> [%sexp_of: (string, int) Symmetric_diff_element.t list]
        |> print_s
      in
      test [] [];
      [%expect {| () |}];
      test [ "one", 1 ] [];
      [%expect {| ((one (Left 1))) |}];
      test [] [ "two", 2 ];
      [%expect {| ((two (Right 2))) |}];
      test [ "one", 1; "two", 2 ] [ "one", 1; "two", 2 ];
      [%expect {| () |}];
      test [ "one", 1; "two", 2 ] [ "one", 1; "two", 3 ];
      [%expect {| ((two (Unequal (2 3)))) |}]
    ;;

    module String_to_int_map = struct
      type t = int Map.M(String).t [@@deriving equal, sexp_of]

      open Base_quickcheck

      let quickcheck_generator =
        Generator.map_t_m (module String) Generator.string Generator.int
      ;;

      let quickcheck_observer = Observer.map_t Observer.string Observer.int
      let quickcheck_shrinker = Shrinker.map_t Shrinker.string Shrinker.int
    end

    let apply_diff_left_to_right map (key, elt) =
      match elt with
      | `Right data | `Unequal (_, data) -> Map.set map ~key ~data
      | `Left _ -> Map.remove map key
    ;;

    let apply_diff_right_to_left map (key, elt) =
      match elt with
      | `Left data | `Unequal (data, _) -> Map.set map ~key ~data
      | `Right _ -> Map.remove map key
    ;;

    let%expect_test "reconstructing in both directions" =
      let test (map1, map2) =
        let diff = Map.symmetric_diff map1 map2 ~data_equal:Int.equal in
        require_equal
          [%here]
          (module String_to_int_map)
          (Sequence.fold diff ~init:map1 ~f:apply_diff_left_to_right)
          map2;
        require_equal
          [%here]
          (module String_to_int_map)
          map1
          (Sequence.fold diff ~init:map2 ~f:apply_diff_right_to_left)
      in
      Base_quickcheck.Test.run_exn
        ~f:test
        (module struct
          type t = String_to_int_map.t * String_to_int_map.t
          [@@deriving quickcheck, sexp_of]
        end)
    ;;

    let%expect_test "vs [fold_symmetric_diff]" =
      let test (map1, map2) =
        require_compare_equal
          [%here]
          (module struct
            type t = (string, int) Symmetric_diff_element.t list
            [@@deriving compare, sexp_of]
          end)
          (Map.symmetric_diff map1 map2 ~data_equal:Int.equal
           |> Sequence.fold ~init:[] ~f:(Fn.flip List.cons))
          (Map.fold_symmetric_diff
             map1
             map2
             ~data_equal:Int.equal
             ~init:[]
             ~f:(Fn.flip List.cons))
      in
      Base_quickcheck.Test.run_exn
        ~f:test
        (module struct
          type t = String_to_int_map.t * String_to_int_map.t
          [@@deriving quickcheck, sexp_of]
        end)
    ;;
  end)
;;

let%test_module "of_alist_multi key equality" =
  (module struct
    module Key = struct
      module T = struct
        type t = string * int [@@deriving sexp_of]

        let compare = [%compare: string * _]
      end

      include T
      include Comparator.Make (T)
    end

    let alist = [ ("a", 1), 1; ("a", 2), 3; ("b", 0), 0; ("a", 3), 2 ]

    let%expect_test "of_alist_multi chooses the first key" =
      print_s [%sexp (Map.of_alist_multi (module Key) alist : int list Map.M(Key).t)];
      [%expect {| (((a 1) (1 3 2)) ((b 0) (0))) |}]
    ;;

    let%test_unit "of_{alist,sequence}_multi have the same behaviour" =
      [%test_result: int list Map.M(Key).t]
        ~expect:(Map.of_alist_multi (module Key) alist)
        (Map.of_sequence_multi (module Key) (Sequence.of_list alist))
    ;;
  end)
;;
