open! Core_kernel
open! Expect_test_helpers_core
open! Rope

let%test _ = is_empty empty
let%test _ = not (is_empty (of_string "non-empty"))

(* Note that it's possible to hit the length restriction while only using logarithmic
   amounts of memory. *)
let%test "length overflow" =
  let x = of_string "x" in
  Exn.does_raise (fun () -> Fn.apply_n_times ~n:Int.num_bits (fun x -> x ^ x) x)
;;

let%test_unit _ =
  [%test_result: string] ~expect:"" (concat ~sep:(of_string ", ") [] |> to_string);
  [%test_result: string]
    ~expect:"one, two, three"
    (concat ~sep:(of_string ", ") [ of_string "one"; of_string "two"; of_string "three" ]
     |> to_string)
;;

let%test_unit _ =
  let r = (of_string "abc" ^ of_string "def") ^ of_string "ghi" ^ of_string "jkl" in
  let buffer = Buffer.create 12 in
  add_to_buffer r buffer;
  [%test_result: String.t] ~expect:"abcdefghijkl" (Buffer.contents buffer)
;;

let%test_unit "no stack overflow" =
  [%test_result: string]
    (to_string
       (List.fold_left
          ~init:(of_string "")
          ~f:( ^ )
          (List.init 1000000 ~f:(fun _x -> of_string "x"))))
    ~expect:(String.make 1000000 'x')
;;

let%test_unit _ = [%test_result: string] (to_string (of_string "")) ~expect:""
let%test_unit _ = [%test_result: string] (to_string (of_string "x")) ~expect:"x"

let%test_unit _ =
  [%test_result: string]
    (to_string (of_string "ab" ^ of_string "cd" ^ of_string "efg"))
    ~expect:"abcdefg"
;;

let%test_unit _ =
  let rec go = function
    | 0 -> of_string "0"
    | n -> go (n - 1) ^ of_string (string_of_int n) ^ go (n - 1)
  in
  [%test_result: string] (to_string (go 4)) ~expect:"0102010301020104010201030102010"
;;

let%expect_test "to_char_sequence" =
  let t = of_string "abc" ^ (of_string "def" ^ of_string "ghi") ^ of_string "jkl" in
  print_endline (to_char_sequence t |> Sequence.to_list |> String.of_char_list);
  [%expect "abcdefghijkl"]
;;

let a2 = of_string "aa"
let a3 = of_string "aaa"
let aba = of_string "aba"
let ab = of_string "ab"
let ba = of_string "ba"

let%expect_test "is_prefix" =
  let test a b =
    require_equal
      [%here]
      (module Bool)
      (String.is_prefix (to_string a) ~prefix:(to_string b))
      (is_prefix a ~prefix:b)
  in
  test a2 a3;
  test a3 a2;
  test (a2 ^ a2) a2;
  test (a2 ^ a2) a3;
  test (a3 ^ a3) (a2 ^ a2);
  test (a3 ^ a3) (a2 ^ a2 ^ a2);
  test ((a2 ^ a2) ^ a2) (a3 ^ a3);
  test a3 (a2 ^ a2);
  test aba aba;
  test ab ba;
  test (ab ^ ab) aba;
  test (ab ^ ba) aba;
  test (aba ^ ba) (ab ^ ab);
  test (aba ^ ba ^ ba) (ab ^ ab ^ ab);
  test (aba ^ ba ^ ba) (ab ^ ab ^ ba);
  [%expect {| |}]
;;

let%expect_test "to_string and For_testing.to_string_tailcall produce identical results" =
  Expect_test_helpers_core.require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      ~shrinker:quickcheck_shrinker
      ~sexp_of:For_testing.sexp_of_t
      ~f:(fun t ->
        let to_string = to_string t in
        let to_string_simple = For_testing.to_string_tailcall t in
        match String.equal to_string to_string_simple with
        | true -> ()
        | false ->
          raise_s
            [%sexp
              "mismatch between to_string implementations"
            , { to_string : string; to_string_simple : string }])
      quickcheck_generator);
  [%expect {| |}]
;;
