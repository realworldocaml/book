open! Import

type 'a t = 'a Generator.t

let create = Generator.create
let generate = Generator.generate

let%expect_test "create & generate" =
  let int_up_to_size =
    Generator.create (fun ~size ~random -> Splittable_random.int random ~lo:0 ~hi:size)
  in
  let random = Splittable_random.State.create Random.State.default in
  List.init 30 ~f:(fun size -> Generator.generate int_up_to_size ~size ~random)
  |> [%sexp_of: int list]
  |> print_s;
  [%expect {| (0 0 1 0 4 3 0 4 2 4 5 0 1 9 10 5 13 3 18 11 8 20 15 4 24 3 2 15 6 2) |}];
  require_does_raise [%here] (fun () ->
    Generator.generate
      int_up_to_size
      ~size:(-1)
      ~random:(Splittable_random.State.of_int 0));
  [%expect {| ("Base_quickcheck.Generator.generate: size < 0" (size -1)) |}]
;;

include (Generator : Applicative.S with type 'a t := 'a t)
include (Generator : Monad.S with type 'a t := 'a t)

let%expect_test "return" =
  test_generator (Generator.return ()) m_unit;
  [%expect {| (generator exhaustive) |}];
  test_generator ~mode:`inexhaustive (Generator.return false) m_bool;
  [%expect
    {|
    (generator
     ("generated 1 distinct values in 10_000 iterations"
      ("did not generate these values" (true)))) |}]
;;

let%expect_test "map" =
  test_generator (Generator.map Generator.char ~f:Char.is_print) m_bool;
  [%expect {| (generator exhaustive) |}]
;;

let%expect_test "both" =
  test_generator (Generator.both Generator.bool Generator.bool) (m_pair m_bool m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let%expect_test "bind" =
  test_generator
    ~mode:`inexhaustive
    (Generator.bind Generator.bool ~f:(fun bool ->
       let gen = Generator.return bool in
       Generator.both gen gen))
    (m_pair m_bool m_bool);
  [%expect
    {|
    (generator
     ("generated 2 distinct values in 10_000 iterations"
      ("did not generate these values" ((false true) (true false))))) |}]
;;

let perturb = Generator.perturb

let%expect_test "perturb" =
  let gen =
    Generator.create (fun ~size:_ ~random -> Splittable_random.int random ~lo:0 ~hi:9)
  in
  let size = 0 in
  List.init 10 ~f:(fun salt ->
    let random = Splittable_random.State.of_int 0 in
    let gen = Generator.perturb gen salt in
    List.init 10 ~f:(fun _ -> Generator.generate gen ~size ~random))
  |> [%sexp_of: int list list]
  |> print_s;
  [%expect
    {|
    ((0 4 0 6 0 1 4 9 1 6)
     (2 4 9 9 1 2 5 0 8 5)
     (7 3 6 0 5 0 0 2 5 5)
     (8 5 6 5 4 2 1 9 1 2)
     (8 0 7 4 9 0 5 8 4 5)
     (0 1 5 1 5 9 4 7 4 7)
     (3 8 6 8 5 6 9 9 6 5)
     (0 4 1 3 5 6 6 6 3 5)
     (5 0 9 7 6 5 9 1 6 0)
     (0 6 8 8 1 7 7 9 4 0)) |}]
;;

let size = Generator.size

let%expect_test "size" =
  test_generator Generator.size (m_nat ~up_to:30);
  [%expect {| (generator exhaustive) |}]
;;

let sizes = Generator.sizes

let%expect_test "sizes" =
  test_generator ~mode:`inexhaustive (Generator.sizes ()) (m_list (m_nat ~up_to:10));
  [%expect
    {|
    (generator
     ("generated 3_724 distinct values in 10_000 iterations"
      ("did not generate these values" ((0 10) (10 0))))) |}];
  (* The most common size lists: *)
  show_distribution
    (Generator.sizes ())
    (module struct
      type t = int list [@@deriving compare, sexp_of]
    end);
  [%expect
    {|
    ((24.63% ())
     (1.65% (0))
     (1.12% (2))
     (1.06% (1))
     (91bp (0 0))
     (87bp (6))
     (83bp (0 0 0 0))
     (77bp (8))
     (73bp (5))
     (73bp (4))
     (72bp (7))
     (68bp (3))
     (64bp (15))
     (62bp (25))
     (61bp (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
     (60bp (14))
     (60bp (10))
     (60bp (0 0 0 0 0 0 0 0))
     (59bp (11))
     (58bp (13))) |}];
  (* The most common lengths of a size list: *)
  show_distribution (Generator.sizes () |> Generator.map ~f:List.length) (module Int);
  [%expect
    {|
    ((24.63% 0)
     (19.39% 1)
     (9.58% 2)
     (8.72% 3)
     (4.96% 4)
     (4.13% 5)
     (3.77% 6)
     (3.52% 7)
     (2.63% 8)
     (2.15% 9)
     (1.75% 16)
     (1.67% 10)
     (1.61% 12)
     (1.37% 13)
     (1.32% 11)
     (1.27% 17)
     (1.25% 15)
     (1.18% 14)
     (1.06% 18)
     (76bp 21)) |}];
  (* The most common number of non-zero sizes in a size list: *)
  show_distribution
    (Generator.sizes () |> Generator.map ~f:(List.count ~f:(Int.( <> ) 0)))
    (module Int);
  [%expect
    {|
    ((32.65% 0)
     (23.74% 1)
     (12.01% 2)
     (10.74% 3)
     (6.18% 4)
     (5.22% 5)
     (4.64% 6)
     (2.69% 7)
     (1.38% 8)
     (56bp 9)
     (13bp 10)
     (6bp 11)) |}]
;;

let with_size = Generator.with_size

let%expect_test "with_size" =
  test_generator
    ~mode:`inexhaustive
    (Generator.with_size ~size:0 Generator.size)
    (m_nat ~up_to:10);
  [%expect
    {|
    (generator
     ("generated 1 distinct values in 10_000 iterations"
      ("did not generate these values" (1 2 3 4 5 6 7 8 9 10)))) |}]
;;

let filter = Generator.filter

let%expect_test "filter" =
  let is_even int = int % 2 = 0 in
  test_generator
    ~mode:`inexhaustive
    (Generator.filter ~f:is_even Generator.size)
    (m_nat ~up_to:30);
  [%expect
    {|
    (generator
     ("generated 16 distinct values in 10_000 iterations"
      ("did not generate these values" (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29)))) |}]
;;

let filter_map = Generator.filter_map

let%expect_test "filter_map" =
  let exactly_half int = if int % 2 = 0 then Some (int / 2) else None in
  test_generator (Generator.filter_map ~f:exactly_half Generator.size) (m_nat ~up_to:15);
  [%expect {| (generator exhaustive) |}]
;;

let of_list = Generator.of_list

let%expect_test "of_list" =
  test_generator (Generator.of_list Bool.all) m_bool;
  [%expect {| (generator exhaustive) |}]
;;

let of_weighted_list = Generator.of_weighted_list

let%expect_test "of_weighted_list" =
  test_generator
    ~mode:`inexhaustive
    (Generator.of_weighted_list (List.init 31 ~f:(fun size -> Float.of_int size, size)))
    (m_nat ~up_to:30);
  [%expect
    {|
    (generator
     ("generated 30 distinct values in 10_000 iterations"
      ("did not generate these values" (0)))) |}]
;;

let union = Generator.union

let%expect_test "union" =
  test_generator
    (Generator.union (List.init 31 ~f:(fun size -> Generator.return size)))
    (m_nat ~up_to:30);
  [%expect {| (generator exhaustive) |}]
;;

let weighted_union = Generator.weighted_union

let%expect_test "weighted_union" =
  test_generator
    ~mode:`inexhaustive
    (Generator.weighted_union
       (List.init 31 ~f:(fun size -> Float.of_int size, Generator.return size)))
    (m_nat ~up_to:30);
  [%expect
    {|
    (generator
     ("generated 30 distinct values in 10_000 iterations"
      ("did not generate these values" (0)))) |}]
;;

let fixed_point = Generator.fixed_point

let%expect_test "fixed_point" =
  test_generator
    ~mode:`inexhaustive
    (Generator.fixed_point (fun generator ->
       Generator.bind Generator.bool ~f:(function
         | false -> Generator.return 0
         | true -> Generator.map generator ~f:Int.succ)))
    (m_nat ~up_to:30);
  [%expect
    {|
    (generator
     ("generated 15 distinct values in 10_000 iterations"
      ("did not generate these values"
       (14 15 16 17 19 20 21 22 23 24 25 26 27 28 29 30)))) |}];
  (* [fixed_point] should only have to call its argument once *)
  let recursive_calls = ref 0 in
  let values_generated = ref 0 in
  Test.with_sample_exn
    ~f:(Sequence.iter ~f:(fun () -> Int.incr values_generated))
    (Generator.fixed_point (fun _ ->
       Int.incr recursive_calls;
       Generator.unit));
  print_s [%message "" (recursive_calls : int ref) (values_generated : int ref)];
  require_equal [%here] (module Int) !recursive_calls 1;
  require_equal [%here] (module Int) !values_generated 10_000;
  [%expect {| ((recursive_calls 1) (values_generated 10000)) |}]
;;

let recursive_union = Generator.recursive_union

let%expect_test "recursive_union" =
  test_generator
    ~mode:`inexhaustive
    (Generator.recursive_union
       [ Generator.of_list [ "a"; "bc"; "def" ]
         |> Generator.map ~f:(fun atom -> Sexp.Atom atom)
       ]
       ~f:(fun sexp ->
         [ Generator.list sexp |> Generator.map ~f:(fun list -> Sexp.List list) ]))
    m_sexp;
  [%expect
    {|
    (generator
     ("generated 2_519 distinct values in 10_000 iterations"
      ("did not generate these values" ((a bc def (a) (bc) (def) (a bc def)))))) |}];
  (* [recursive_union] should only have to call its argument once *)
  let recursive_calls = ref 0 in
  let values_generated = ref 0 in
  Test.with_sample_exn
    ~f:(Sequence.iter ~f:(fun () -> Int.incr values_generated))
    (Generator.recursive_union [ Generator.unit ] ~f:(fun _ ->
       Int.incr recursive_calls;
       [ Generator.unit ]));
  print_s [%message "" (recursive_calls : int ref) (values_generated : int ref)];
  require_equal [%here] (module Int) !recursive_calls 1;
  require_equal [%here] (module Int) !values_generated 10_000;
  [%expect {| ((recursive_calls 1) (values_generated 10000)) |}]
;;

let weighted_recursive_union = Generator.weighted_recursive_union

let%expect_test "weighted_recursive_union" =
  test_generator
    ~mode:`inexhaustive
    (Generator.weighted_recursive_union
       [ ( 2.
         , Generator.of_list [ "a"; "bc"; "def" ]
           |> Generator.map ~f:(fun atom -> Sexp.Atom atom) )
       ]
       ~f:(fun sexp ->
         [ 1., Generator.list sexp |> Generator.map ~f:(fun list -> Sexp.List list) ]))
    m_sexp;
  [%expect
    {|
    (generator
     ("generated 1_520 distinct values in 10_000 iterations"
      ("did not generate these values" ((a bc def (a) (bc) (def) (a bc def)))))) |}];
  (* [weighted_recursive_union] should only have to call its argument once *)
  let recursive_calls = ref 0 in
  let values_generated = ref 0 in
  Test.with_sample_exn
    ~f:(Sequence.iter ~f:(fun () -> Int.incr values_generated))
    (Generator.weighted_recursive_union [ 1., Generator.unit ] ~f:(fun _ ->
       Int.incr recursive_calls;
       [ 2., Generator.unit ]));
  print_s [%message "" (recursive_calls : int ref) (values_generated : int ref)];
  require_equal [%here] (module Int) !recursive_calls 1;
  require_equal [%here] (module Int) !values_generated 10_000;
  [%expect {| ((recursive_calls 1) (values_generated 10000)) |}]
;;

let fn = Generator.fn

let%expect_test "fn" =
  test_generator (Generator.fn Observer.bool Generator.bool) (m_arrow m_bool m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let unit = Generator.unit

let%expect_test "unit" =
  test_generator Generator.unit m_unit;
  [%expect {| (generator exhaustive) |}]
;;

let bool = Generator.bool

let%expect_test "bool" =
  test_generator Generator.bool m_bool;
  [%expect {| (generator exhaustive) |}]
;;

let option = Generator.option

let%expect_test "option" =
  test_generator (Generator.option Generator.bool) (m_option m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let either = Generator.either

let%expect_test "either" =
  test_generator
    (Generator.either Generator.bool Generator.bool)
    (m_either m_bool m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let result = Generator.result

let%expect_test "result" =
  test_generator
    (Generator.result Generator.bool Generator.bool)
    (m_result m_bool m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let map_t_m = Generator.map_t_m
let map_tree_using_comparator = Generator.map_tree_using_comparator

let%expect_test "map_t_m" =
  test_generator
    (Generator.map_t_m (module Bool) Generator.bool Generator.bool)
    (m_map (module Bool) m_bool m_bool);
  [%expect {| (generator "generated 9 distinct values in 10_000 iterations") |}]
;;

let set_t_m = Generator.set_t_m
let set_tree_using_comparator = Generator.set_tree_using_comparator

let%expect_test "set_t_m" =
  test_generator
    (Generator.set_t_m (module Bool) Generator.bool)
    (m_set (module Bool) m_bool);
  [%expect {| (generator exhaustive) |}]
;;

let small_positive_or_zero_int = Generator.small_positive_or_zero_int

let%expect_test "small_positive_or_zero_int" =
  test_generator Generator.small_positive_or_zero_int (m_nat ~up_to:31);
  [%expect {| (generator exhaustive) |}]
;;

let small_strictly_positive_int = Generator.small_strictly_positive_int

let%expect_test "small_strictly_positive_int" =
  test_generator
    ~mode:`inexhaustive
    Generator.small_strictly_positive_int
    (m_nat ~up_to:31);
  [%expect
    {|
    (generator
     ("generated 31 distinct values in 10_000 iterations"
      ("did not generate these values" (0)))) |}]
;;

let int = Generator.int

let%expect_test ("int"[@tags "64-bits-only"]) =
  test_generator Generator.int (m_int (module Int));
  [%expect {| (generator "generated 8_006 distinct values in 10_000 iterations") |}]
;;

let int_uniform = Generator.int_uniform

let%expect_test ("int_uniform"[@tags "64-bits-only"]) =
  test_generator ~mode:`inexhaustive Generator.int_uniform (m_int (module Int));
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values"
       (-4611686018427387904 -1 0 1 4611686018427387903)))) |}]
;;

let int_inclusive = Generator.int_inclusive

let%expect_test "int_inclusive" =
  test_generator (Generator.int_inclusive 0 10) (m_nat ~up_to:10);
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int_inclusive 0 10) (module Int);
  [%expect
    {|
    ((12.96% 0)
     (12.63% 10)
     (8.62% 9)
     (8.53% 5)
     (8.46% 1)
     (8.44% 8)
     (8.25% 4)
     (8.21% 2)
     (8.18% 6)
     (8.15% 7)
     (7.57% 3)) |}]
;;

let int_uniform_inclusive = Generator.int_uniform_inclusive

let%expect_test "int_uniform_inclusive" =
  test_generator (Generator.int_uniform_inclusive 0 10) (m_nat ~up_to:10);
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int_uniform_inclusive 0 10) (module Int);
  [%expect
    {|
    ((9.55% 5)
     (9.54% 0)
     (9.21% 9)
     (9.18% 1)
     (9.14% 4)
     (9.01% 7)
     (8.95% 10)
     (8.89% 8)
     (8.88% 3)
     (8.86% 2)
     (8.79% 6)) |}]
;;

let int_log_inclusive = Generator.int_log_inclusive

let%expect_test "int_log_inclusive" =
  test_generator (Generator.int_log_inclusive 0 10) (m_nat ~up_to:10);
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int_log_inclusive 0 10) (module Int);
  [%expect
    {|
    ((22.24% 0)
     (18.67% 1)
     (10.43% 10)
     (9.47% 3)
     (9.12% 2)
     (6.03% 8)
     (6% 9)
     (4.82% 5)
     (4.59% 4)
     (4.37% 7)
     (4.26% 6)) |}]
;;

let int_log_uniform_inclusive = Generator.int_log_uniform_inclusive

let%expect_test "int_log_uniform_inclusive" =
  test_generator (Generator.int_log_uniform_inclusive 0 10) (m_nat ~up_to:10);
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int_log_uniform_inclusive 0 10) (module Int);
  [%expect
    {|
    ((19.96% 0)
     (19.8% 1)
     (10.19% 3)
     (9.98% 2)
     (6.74% 9)
     (6.64% 8)
     (6.59% 10)
     (5.11% 6)
     (5.08% 5)
     (5.07% 7)
     (4.84% 4)) |}]
;;

let int32 = Generator.int32

let%expect_test "int32" =
  test_generator Generator.int32 (m_int (module Int32));
  [%expect {| (generator "generated 6_769 distinct values in 10_000 iterations") |}]
;;

let int32_uniform = Generator.int32_uniform

let%expect_test "int32_uniform" =
  test_generator ~mode:`inexhaustive Generator.int32_uniform (m_int (module Int32));
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values" (-2147483648 -1 0 1 2147483647)))) |}]
;;

let int32_inclusive = Generator.int32_inclusive

let%expect_test "int32_inclusive" =
  test_generator (Generator.int32_inclusive 0l 10l) (m_nat' ~up_to:10 (module Int32));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int32_inclusive 0l 10l) (module Int32);
  [%expect
    {|
    ((12.96% 0)
     (12.63% 10)
     (8.62% 9)
     (8.53% 5)
     (8.46% 1)
     (8.44% 8)
     (8.25% 4)
     (8.21% 2)
     (8.18% 6)
     (8.15% 7)
     (7.57% 3)) |}]
;;

let int32_uniform_inclusive = Generator.int32_uniform_inclusive

let%expect_test "int32_uniform_inclusive" =
  test_generator
    (Generator.int32_uniform_inclusive 0l 10l)
    (m_nat' ~up_to:10 (module Int32));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int32_uniform_inclusive 0l 10l) (module Int32);
  [%expect
    {|
    ((9.55% 5)
     (9.54% 0)
     (9.21% 9)
     (9.18% 1)
     (9.14% 4)
     (9.01% 7)
     (8.95% 10)
     (8.89% 8)
     (8.88% 3)
     (8.86% 2)
     (8.79% 6)) |}]
;;

let int32_log_inclusive = Generator.int32_log_inclusive

let%expect_test "int32_log_inclusive" =
  test_generator (Generator.int32_log_inclusive 0l 10l) (m_nat' ~up_to:10 (module Int32));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int32_log_inclusive 0l 10l) (module Int32);
  [%expect
    {|
    ((22.24% 0)
     (18.67% 1)
     (10.43% 10)
     (9.47% 3)
     (9.12% 2)
     (6.03% 8)
     (6% 9)
     (4.82% 5)
     (4.59% 4)
     (4.37% 7)
     (4.26% 6)) |}]
;;

let int32_log_uniform_inclusive = Generator.int32_log_uniform_inclusive

let%expect_test "int32_log_uniform_inclusive" =
  test_generator
    (Generator.int32_log_uniform_inclusive 0l 10l)
    (m_nat' ~up_to:10 (module Int32));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int32_log_uniform_inclusive 0l 10l) (module Int32);
  [%expect
    {|
    ((19.96% 0)
     (19.8% 1)
     (10.19% 3)
     (9.98% 2)
     (6.74% 9)
     (6.64% 8)
     (6.59% 10)
     (5.11% 6)
     (5.08% 5)
     (5.07% 7)
     (4.84% 4)) |}]
;;

let int63 = Generator.int63

let%expect_test "int63" =
  test_generator Generator.int63 (m_int (module Int63));
  [%expect {| (generator "generated 8_006 distinct values in 10_000 iterations") |}]
;;

let int63_uniform = Generator.int63_uniform

let%expect_test "int63_uniform" =
  test_generator ~mode:`inexhaustive Generator.int63_uniform (m_int (module Int63));
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values"
       (-4611686018427387904 -1 0 1 4611686018427387903)))) |}]
;;

let int63_inclusive = Generator.int63_inclusive

let%expect_test "int63_inclusive" =
  test_generator
    (Generator.int63_inclusive Int63.zero (Int63.of_int_exn 10))
    (m_nat' ~up_to:10 (module Int63));
  [%expect {| (generator exhaustive) |}];
  show_distribution
    (Generator.int63_inclusive Int63.zero (Int63.of_int_exn 10))
    (module Int63);
  [%expect
    {|
    ((12.96% 0)
     (12.63% 10)
     (8.62% 9)
     (8.53% 5)
     (8.46% 1)
     (8.44% 8)
     (8.25% 4)
     (8.21% 2)
     (8.18% 6)
     (8.15% 7)
     (7.57% 3)) |}]
;;

let int63_uniform_inclusive = Generator.int63_uniform_inclusive

let%expect_test "int63_uniform_inclusive" =
  test_generator
    (Generator.int63_uniform_inclusive Int63.zero (Int63.of_int_exn 10))
    (m_nat' ~up_to:10 (module Int63));
  [%expect {| (generator exhaustive) |}];
  show_distribution
    (Generator.int63_uniform_inclusive Int63.zero (Int63.of_int_exn 10))
    (module Int63);
  [%expect
    {|
    ((9.55% 5)
     (9.54% 0)
     (9.21% 9)
     (9.18% 1)
     (9.14% 4)
     (9.01% 7)
     (8.95% 10)
     (8.89% 8)
     (8.88% 3)
     (8.86% 2)
     (8.79% 6)) |}]
;;

let int63_log_inclusive = Generator.int63_log_inclusive

let%expect_test "int63_log_inclusive" =
  test_generator
    (Generator.int63_log_inclusive Int63.zero (Int63.of_int_exn 10))
    (m_nat' ~up_to:10 (module Int63));
  [%expect {| (generator exhaustive) |}];
  show_distribution
    (Generator.int63_log_inclusive Int63.zero (Int63.of_int_exn 10))
    (module Int63);
  [%expect
    {|
    ((22.24% 0)
     (18.67% 1)
     (10.43% 10)
     (9.47% 3)
     (9.12% 2)
     (6.03% 8)
     (6% 9)
     (4.82% 5)
     (4.59% 4)
     (4.37% 7)
     (4.26% 6)) |}]
;;

let int63_log_uniform_inclusive = Generator.int63_log_uniform_inclusive

let%expect_test "int63_log_uniform_inclusive" =
  test_generator
    (Generator.int63_log_uniform_inclusive Int63.zero (Int63.of_int_exn 10))
    (m_nat' ~up_to:10 (module Int63));
  [%expect {| (generator exhaustive) |}];
  show_distribution
    (Generator.int63_log_uniform_inclusive Int63.zero (Int63.of_int_exn 10))
    (module Int63);
  [%expect
    {|
    ((19.96% 0)
     (19.8% 1)
     (10.19% 3)
     (9.98% 2)
     (6.74% 9)
     (6.64% 8)
     (6.59% 10)
     (5.11% 6)
     (5.08% 5)
     (5.07% 7)
     (4.84% 4)) |}]
;;

let int64 = Generator.int64

let%expect_test "int64" =
  test_generator Generator.int64 (m_int (module Int64));
  [%expect {| (generator "generated 8_047 distinct values in 10_000 iterations") |}]
;;

let int64_uniform = Generator.int64_uniform

let%expect_test "int64_uniform" =
  test_generator ~mode:`inexhaustive Generator.int64_uniform (m_int (module Int64));
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values"
       (-9223372036854775808 -1 0 1 9223372036854775807)))) |}]
;;

let int64_inclusive = Generator.int64_inclusive

let%expect_test "int64_inclusive" =
  test_generator (Generator.int64_inclusive 0L 10L) (m_nat' ~up_to:10 (module Int64));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int64_inclusive 0L 10L) (module Int64);
  [%expect
    {|
    ((12.96% 0)
     (12.63% 10)
     (8.62% 9)
     (8.53% 5)
     (8.46% 1)
     (8.44% 8)
     (8.25% 4)
     (8.21% 2)
     (8.18% 6)
     (8.15% 7)
     (7.57% 3)) |}]
;;

let int64_uniform_inclusive = Generator.int64_uniform_inclusive

let%expect_test "int64_uniform_inclusive" =
  test_generator
    (Generator.int64_uniform_inclusive 0L 10L)
    (m_nat' ~up_to:10 (module Int64));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int64_uniform_inclusive 0L 10L) (module Int64);
  [%expect
    {|
    ((9.55% 5)
     (9.54% 0)
     (9.21% 9)
     (9.18% 1)
     (9.14% 4)
     (9.01% 7)
     (8.95% 10)
     (8.89% 8)
     (8.88% 3)
     (8.86% 2)
     (8.79% 6)) |}]
;;

let int64_log_inclusive = Generator.int64_log_inclusive

let%expect_test "int64_log_inclusive" =
  test_generator (Generator.int64_log_inclusive 0L 10L) (m_nat' ~up_to:10 (module Int64));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int64_log_inclusive 0L 10L) (module Int64);
  [%expect
    {|
    ((22.24% 0)
     (18.67% 1)
     (10.43% 10)
     (9.47% 3)
     (9.12% 2)
     (6.03% 8)
     (6% 9)
     (4.82% 5)
     (4.59% 4)
     (4.37% 7)
     (4.26% 6)) |}]
;;

let int64_log_uniform_inclusive = Generator.int64_log_uniform_inclusive

let%expect_test "int64_log_uniform_inclusive" =
  test_generator
    (Generator.int64_log_uniform_inclusive 0L 10L)
    (m_nat' ~up_to:10 (module Int64));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.int64_log_uniform_inclusive 0L 10L) (module Int64);
  [%expect
    {|
    ((19.96% 0)
     (19.8% 1)
     (10.19% 3)
     (9.98% 2)
     (6.74% 9)
     (6.64% 8)
     (6.59% 10)
     (5.11% 6)
     (5.08% 5)
     (5.07% 7)
     (4.84% 4)) |}]
;;

let nativeint = Generator.nativeint

let%expect_test ("nativeint"[@tags "64-bits-only"]) =
  test_generator Generator.nativeint (m_int (module Nativeint));
  [%expect {| (generator "generated 8_047 distinct values in 10_000 iterations") |}]
;;

let nativeint_uniform = Generator.nativeint_uniform

let%expect_test ("nativeint_uniform"[@tags "64-bits-only"]) =
  test_generator
    ~mode:`inexhaustive
    Generator.nativeint_uniform
    (m_int (module Nativeint));
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values"
       (-9223372036854775808 -1 0 1 9223372036854775807)))) |}]
;;

let nativeint_inclusive = Generator.nativeint_inclusive

let%expect_test "nativeint_inclusive" =
  test_generator
    (Generator.nativeint_inclusive 0n 10n)
    (m_nat' ~up_to:10 (module Nativeint));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.nativeint_inclusive 0n 10n) (module Nativeint);
  [%expect
    {|
    ((12.96% 0)
     (12.63% 10)
     (8.62% 9)
     (8.53% 5)
     (8.46% 1)
     (8.44% 8)
     (8.25% 4)
     (8.21% 2)
     (8.18% 6)
     (8.15% 7)
     (7.57% 3)) |}]
;;

let nativeint_uniform_inclusive = Generator.nativeint_uniform_inclusive

let%expect_test "nativeint_uniform_inclusive" =
  test_generator
    (Generator.nativeint_uniform_inclusive 0n 10n)
    (m_nat' ~up_to:10 (module Nativeint));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.nativeint_uniform_inclusive 0n 10n) (module Nativeint);
  [%expect
    {|
    ((9.55% 5)
     (9.54% 0)
     (9.21% 9)
     (9.18% 1)
     (9.14% 4)
     (9.01% 7)
     (8.95% 10)
     (8.89% 8)
     (8.88% 3)
     (8.86% 2)
     (8.79% 6)) |}]
;;

let nativeint_log_inclusive = Generator.nativeint_log_inclusive

let%expect_test "nativeint_log_inclusive" =
  test_generator
    (Generator.nativeint_log_inclusive 0n 10n)
    (m_nat' ~up_to:10 (module Nativeint));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.nativeint_log_inclusive 0n 10n) (module Nativeint);
  [%expect
    {|
    ((22.24% 0)
     (18.67% 1)
     (10.43% 10)
     (9.47% 3)
     (9.12% 2)
     (6.03% 8)
     (6% 9)
     (4.82% 5)
     (4.59% 4)
     (4.37% 7)
     (4.26% 6)) |}]
;;

let nativeint_log_uniform_inclusive = Generator.nativeint_log_uniform_inclusive

let%expect_test "nativeint_log_uniform_inclusive" =
  test_generator
    (Generator.nativeint_log_uniform_inclusive 0n 10n)
    (m_nat' ~up_to:10 (module Nativeint));
  [%expect {| (generator exhaustive) |}];
  show_distribution (Generator.nativeint_log_uniform_inclusive 0n 10n) (module Nativeint);
  [%expect
    {|
    ((19.96% 0)
     (19.8% 1)
     (10.19% 3)
     (9.98% 2)
     (6.74% 9)
     (6.64% 8)
     (6.59% 10)
     (5.11% 6)
     (5.08% 5)
     (5.07% 7)
     (4.84% 4)) |}]
;;

let float = Generator.float

let%expect_test "float" =
  test_generator ~mode:`inexhaustive Generator.float m_float;
  [%expect
    {|
    (generator
     ("generated 9_127 distinct values in 10_000 iterations"
      ("did not generate these values"
       (-1.7976931348623157E+308 1.7976931348623157E+308)))) |}]
;;

let float_without_nan = Generator.float_without_nan

let%expect_test "float_without_nan" =
  test_generator ~mode:`inexhaustive Generator.float_without_nan m_float;
  [%expect
    {|
    (generator
     ("generated 9_170 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN -1.7976931348623157E+308 1.7976931348623157E+308)))) |}]
;;

let float_finite = Generator.float_finite

let%expect_test "float_finite" =
  test_generator ~mode:`inexhaustive Generator.float_finite m_float;
  [%expect
    {|
    (generator
     ("generated 9_252 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN -INF -1.7976931348623157E+308 1.7976931348623157E+308 INF)))) |}]
;;

let float_strictly_positive = Generator.float_strictly_positive

let%expect_test "float_strictly_positive" =
  test_generator ~mode:`inexhaustive Generator.float_strictly_positive m_float;
  [%expect
    {|
    (generator
     ("generated 9_171 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072014E-308
        -2.2250738585072009E-308
        -4.94065645841247E-324
        0
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_strictly_negative = Generator.float_strictly_negative

let%expect_test "float_strictly_negative" =
  test_generator ~mode:`inexhaustive Generator.float_strictly_negative m_float;
  [%expect
    {|
    (generator
     ("generated 9_171 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        0
        4.94065645841247E-324
        2.2250738585072009E-308
        2.2250738585072014E-308
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_positive_or_zero = Generator.float_positive_or_zero

let%expect_test "float_positive_or_zero" =
  test_generator ~mode:`inexhaustive Generator.float_positive_or_zero m_float;
  [%expect
    {|
    (generator
     ("generated 9_095 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072014E-308
        -2.2250738585072009E-308
        -4.94065645841247E-324
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_negative_or_zero = Generator.float_negative_or_zero

let%expect_test "float_negative_or_zero" =
  test_generator ~mode:`inexhaustive Generator.float_negative_or_zero m_float;
  [%expect
    {|
    (generator
     ("generated 9_095 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        4.94065645841247E-324
        2.2250738585072009E-308
        2.2250738585072014E-308
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_inclusive = Generator.float_inclusive

let%expect_test "float_inclusive" =
  test_generator ~mode:`inexhaustive (Generator.float_inclusive (-1.) 1.) m_float;
  [%expect
    {|
    (generator
     ("generated 9_075 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072014E-308
        -2.2250738585072009E-308
        -4.94065645841247E-324
        0
        4.94065645841247E-324
        2.2250738585072009E-308
        2.2250738585072014E-308
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_uniform_exclusive = Generator.float_uniform_exclusive

let%expect_test "float_uniform_exclusive" =
  test_generator ~mode:`inexhaustive (Generator.float_uniform_exclusive (-1.) 1.) m_float;
  [%expect
    {|
    (generator
     ("generated 10_000 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072014E-308
        -2.2250738585072009E-308
        -4.94065645841247E-324
        0
        4.94065645841247E-324
        2.2250738585072009E-308
        2.2250738585072014E-308
        1.7976931348623157E+308
        INF)))) |}]
;;

let float_of_class = Generator.float_of_class

let%expect_test "float_of_class" =
  test_generator ~mode:`inexhaustive (Generator.float_of_class Normal) m_float;
  [%expect
    {|
    (generator
     ("generated 9_360 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072009E-308
        -4.94065645841247E-324
        0
        4.94065645841247E-324
        2.2250738585072009E-308
        1.7976931348623157E+308
        INF)))) |}];
  test_generator ~mode:`inexhaustive (Generator.float_of_class Subnormal) m_float;
  [%expect
    {|
    (generator
     ("generated 7_852 distinct values in 10_000 iterations"
      ("did not generate these values"
       (NAN
        -INF
        -1.7976931348623157E+308
        -2.2250738585072014E-308
        0
        2.2250738585072014E-308
        1.7976931348623157E+308
        INF)))) |}]
;;

let char = Generator.char

let%expect_test "char" =
  test_generator Generator.char m_char;
  [%expect {| (generator "generated 249 distinct values in 10_000 iterations") |}]
;;

let char_lowercase = Generator.char_lowercase

let%expect_test "char_lowercase" =
  test_generator ~mode:`inexhaustive Generator.char_lowercase m_char;
  [%expect
    {|
    (generator
     ("generated 26 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! 0 9 A Z ~ "\255")))) |}]
;;

let char_uppercase = Generator.char_uppercase

let%expect_test "char_uppercase" =
  test_generator ~mode:`inexhaustive Generator.char_uppercase m_char;
  [%expect
    {|
    (generator
     ("generated 26 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! 0 9 a z ~ "\255")))) |}]
;;

let char_digit = Generator.char_digit

let%expect_test "char_digit" =
  test_generator ~mode:`inexhaustive Generator.char_digit m_char;
  [%expect
    {|
    (generator
     ("generated 10 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! A Z a z ~ "\255")))) |}]
;;

let char_alpha = Generator.char_alpha

let%expect_test "char_alpha" =
  test_generator ~mode:`inexhaustive Generator.char_alpha m_char;
  [%expect
    {|
    (generator
     ("generated 52 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! 0 9 ~ "\255")))) |}]
;;

let char_alphanum = Generator.char_alphanum

let%expect_test "char_alphanum" =
  test_generator ~mode:`inexhaustive Generator.char_alphanum m_char;
  [%expect
    {|
    (generator
     ("generated 62 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! ~ "\255")))) |}]
;;

let char_whitespace = Generator.char_whitespace

let%expect_test "char_whitespace" =
  test_generator ~mode:`inexhaustive Generator.char_whitespace m_char;
  [%expect
    {|
    (generator
     ("generated 6 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" ! 0 9 A Z a z ~ "\255")))) |}]
;;

let char_print = Generator.char_print

let%expect_test "char_print" =
  test_generator ~mode:`inexhaustive Generator.char_print m_char;
  [%expect
    {|
    (generator
     ("generated 95 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" "\255")))) |}]
;;

let char_uniform_inclusive = Generator.char_uniform_inclusive

let%expect_test "char_uniform_inclusive" =
  test_generator ~mode:`inexhaustive (Generator.char_uniform_inclusive 'A' 'Z') m_char;
  [%expect
    {|
    (generator
     ("generated 26 distinct values in 10_000 iterations"
      ("did not generate these values" ("\000" "\t" " " ! 0 9 a z ~ "\255")))) |}]
;;

let string = Generator.string

let%expect_test "string" =
  test_generator ~mode:`inexhaustive Generator.string m_string;
  [%expect
    {|
    (generator
     ("generated 8_583 distinct values in 10_000 iterations"
      ("did not generate these values" (" " "\000\000" "  " 00 AA __ zz)))) |}]
;;

let string_non_empty = Generator.string_non_empty

let%expect_test "string_non_empty" =
  test_generator ~mode:`inexhaustive Generator.string_non_empty m_string;
  [%expect
    {|
    (generator
     ("generated 8_936 distinct values in 10_000 iterations"
      ("did not generate these values" ("" " " "\000\000" "  " 00 AA __ zz)))) |}]
;;

let string_with_length = Generator.string_with_length

let%expect_test "string_with_length" =
  test_generator ~mode:`inexhaustive (Generator.string_with_length ~length:2) m_string;
  [%expect
    {|
    (generator
     ("generated 5_239 distinct values in 10_000 iterations"
      ("did not generate these values" ("" "\000" " " 0 A _ z "\000\000" "  " __)))) |}]
;;

let string_of = Generator.string_of

let%expect_test "string_of" =
  test_generator
    ~mode:`inexhaustive
    (Generator.string_of (Generator.filter Generator.char ~f:Char.is_lowercase))
    m_string;
  [%expect
    {|
    (generator
     ("generated 8_320 distinct values in 10_000 iterations"
      ("did not generate these values"
       ("\000" " " 0 A _ "\000\000" "  " 00 AA __)))) |}]
;;

let string_non_empty_of = Generator.string_non_empty_of

let%expect_test "string_non_empty_of" =
  test_generator
    ~mode:`inexhaustive
    (Generator.string_non_empty_of (Generator.filter Generator.char ~f:Char.is_lowercase))
    m_string;
  [%expect
    {|
    (generator
     ("generated 8_569 distinct values in 10_000 iterations"
      ("did not generate these values"
       ("" "\000" " " 0 A _ "\000\000" "  " 00 AA __ zz)))) |}]
;;

let string_with_length_of = Generator.string_with_length_of

let%expect_test "string_with_length_of" =
  test_generator
    ~mode:`inexhaustive
    (Generator.string_with_length_of
       ~length:2
       (Generator.filter Generator.char ~f:Char.is_lowercase))
    m_string;
  [%expect
    {|
    (generator
     ("generated 676 distinct values in 10_000 iterations"
      ("did not generate these values"
       ("" "\000" " " 0 A _ z "\000\000" "  " 00 AA __)))) |}]
;;

let sexp = Generator.sexp

let%expect_test "sexp" =
  test_generator ~mode:`inexhaustive Generator.sexp m_sexp;
  [%expect
    {|
    (generator
     ("generated 7_175 distinct values in 10_000 iterations"
      ("did not generate these values"
       (bc def (a bc def) (a bc def (a) (bc) (def) (a bc def)) (bc) (def))))) |}]
;;

let sexp_of = Generator.sexp_of

let%expect_test "sexp_of" =
  test_generator
    ~mode:`inexhaustive
    (Generator.sexp_of (Generator.of_list [ "a"; "bc"; "def" ]))
    m_sexp;
  [%expect
    {|
    (generator
     ("generated 4_917 distinct values in 10_000 iterations"
      ("did not generate these values"
       ((a bc def) (a bc def (a) (bc) (def) (a bc def)))))) |}]
;;

let list = Generator.list

let%expect_test "list" =
  test_generator (Generator.list Generator.bool) (m_list m_bool);
  [%expect {| (generator "generated 2_248 distinct values in 10_000 iterations") |}]
;;

let list_non_empty = Generator.list_non_empty

let%expect_test "list_non_empty" =
  test_generator
    ~mode:`inexhaustive
    (Generator.list_non_empty Generator.bool)
    (m_list m_bool);
  [%expect
    {|
    (generator
     ("generated 2_672 distinct values in 10_000 iterations"
      ("did not generate these values" (())))) |}]
;;

let list_with_length = Generator.list_with_length

let%expect_test "list_with_length" =
  test_generator
    ~mode:`inexhaustive
    (Generator.list_with_length ~length:2 Generator.bool)
    (m_list m_bool);
  [%expect
    {|
    (generator
     ("generated 4 distinct values in 10_000 iterations"
      ("did not generate these values" (() (false) (true))))) |}]
;;

let list_filtered = Generator.list_filtered

let%expect_test "list_filtered" =
  let original_list = List.range 1 4 ~start:`inclusive ~stop:`inclusive in
  test_generator
    (Generator.list_filtered original_list)
    (module struct
      type t = int list [@@deriving compare, sexp_of]

      let examples = [ [] ] @ List.map original_list ~f:List.return @ [ original_list ]
    end);
  [%expect {| (generator "generated 16 distinct values in 10_000 iterations") |}]
;;

let list_permutations = Generator.list_permutations

let%expect_test "list_permutations" =
  let original_list = List.range 1 4 ~start:`inclusive ~stop:`inclusive in
  test_generator
    (Generator.list_permutations original_list)
    (module struct
      type t = int list [@@deriving compare, sexp_of]

      let examples = [ original_list; List.rev original_list ]
    end);
  [%expect {| (generator "generated 24 distinct values in 10_000 iterations") |}]
;;

let of_lazy = Generator.of_lazy

let%expect_test "of_lazy, forced" =
  test_generator (Generator.of_lazy (lazy Generator.size)) (m_nat ~up_to:30);
  [%expect {| (generator exhaustive) |}]
;;

let%expect_test "of_lazy, unforced" =
  test_generator
    (Generator.weighted_union
       [ Float.max_finite_value, Generator.size
       ; Float.min_positive_subnormal_value, Generator.of_lazy (lazy (assert false))
       ])
    (m_nat ~up_to:30);
  [%expect {| (generator exhaustive) |}]
;;

let bigstring = Generator.bigstring
let float32_vec = Generator.float32_vec
let float64_vec = Generator.float64_vec

let%expect_test "[bigstring], [float32_vec], [float64_vec]" =
  let test
        (type elt pack layout)
        (t : (elt, pack, layout) Bigarray.Array1.t Generator.t)
        sexp_of_elt
    =
    let module M = struct
      type t = (elt, pack, layout) Bigarray.Array1.t

      let compare = Poly.compare
      let sexp_of_t = [%sexp_of: (elt, _, _) Private.Bigarray_helpers.Array1.t]
      let examples = []
    end
    in
    test_generator t (module M)
  in
  test bigstring [%sexp_of: char];
  [%expect {| (generator "generated 5_751 distinct values in 10_000 iterations") |}];
  test float32_vec [%sexp_of: float];
  [%expect {| (generator "generated 6_670 distinct values in 10_000 iterations") |}];
  test float64_vec [%sexp_of: float];
  [%expect {| (generator "generated 7_520 distinct values in 10_000 iterations") |}]
;;

let float32_mat = Generator.float32_mat
let float64_mat = Generator.float64_mat

let%expect_test "[float32_mat], [float64_mat]" =
  let test
        (type elt pack layout)
        (t : (elt, pack, layout) Bigarray.Array2.t Generator.t)
        sexp_of_elt
    =
    let module M = struct
      type t = (elt, pack, layout) Bigarray.Array2.t

      let compare = Poly.compare
      let sexp_of_t = [%sexp_of: (elt, _, _) Private.Bigarray_helpers.Array2.t]
      let examples = []
    end
    in
    test_generator t (module M)
  in
  test float32_mat [%sexp_of: float];
  [%expect {| (generator "generated 6_875 distinct values in 10_000 iterations") |}];
  test float64_mat [%sexp_of: float];
  [%expect {| (generator "generated 7_022 distinct values in 10_000 iterations") |}]
;;
