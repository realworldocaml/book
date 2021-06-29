open! Import

module Example = struct
  let natural_number_shrinker =
    Shrinker.create (function
      | 0 -> Sequence.empty
      | n -> Sequence.singleton (n - 1))
  ;;
end

open Example

type 'a t = 'a Shrinker.t

let create = Shrinker.create
let shrink = Shrinker.shrink

let%expect_test "create & shrink" =
  List.init 10 ~f:(fun size -> size, Shrinker.shrink natural_number_shrinker size)
  |> [%sexp_of: (int * int Sequence.t) list]
  |> print_s;
  [%expect
    {|
    ((0 ())
     (1 (0))
     (2 (1))
     (3 (2))
     (4 (3))
     (5 (4))
     (6 (5))
     (7 (6))
     (8 (7))
     (9 (8))) |}]
;;

let atomic = Shrinker.atomic

let%expect_test "atomic" =
  test_shrinker ~mode:`atomic Shrinker.atomic m_bool;
  [%expect {| (shrinker atomic) |}]
;;

let map = Shrinker.map

let%expect_test "shrinker" =
  test_shrinker
    (Shrinker.map natural_number_shrinker ~f:Int.pred ~f_inverse:Int.succ)
    (m_nat ~up_to:10);
  [%expect
    {|
    (shrinker
     ((0 => -1)
      (1 => 0)
      (2 => 1)
      (3 => 2)
      (4 => 3)
      (5 => 4)
      (6 => 5)
      (7 => 6)
      (8 => 7)
      (9 => 8)
      (10 => 9))) |}]
;;

let filter = Shrinker.filter

let%expect_test "shrinker" =
  test_shrinker
    (Shrinker.filter (Shrinker.list natural_number_shrinker) ~f:(fun list ->
       not (List.is_empty list)))
    (m_list (m_nat ~up_to:3));
  [%expect
    {|
    (shrinker
     (((1) => (0))
      ((2) => (1))
      ((3) => (2))
      ((0 3) => (3))
      ((0 3) => (0))
      ((0 3) => (0 2))
      ((1 2) => (2))
      ((1 2) => (0 2))
      ((1 2) => (1))
      ((1 2) => (1 1))
      ((2 1) => (1))
      ((2 1) => (1 1))
      ((2 1) => (2))
      ((2 1) => (2 0))
      ((3 0) => (0))
      ((3 0) => (2 0))
      ((3 0) => (3)))) |}]
;;

let filter_map = Shrinker.filter_map

let%expect_test "shrinker" =
  test_shrinker
    (Shrinker.filter_map
       (Shrinker.list natural_number_shrinker)
       ~f:(function
         | [] -> None
         | list -> Some (List.map list ~f:Int.pred))
       ~f_inverse:(List.map ~f:Int.succ))
    (m_list (m_nat ~up_to:3));
  [%expect
    {|
    (shrinker
     (((0) => (-1))
      ((1) => (0))
      ((2) => (1))
      ((3) => (2))
      ((0 3) => (3))
      ((0 3) => (-1 3))
      ((0 3) => (0))
      ((0 3) => (0 2))
      ((1 2) => (2))
      ((1 2) => (0 2))
      ((1 2) => (1))
      ((1 2) => (1 1))
      ((2 1) => (1))
      ((2 1) => (1 1))
      ((2 1) => (2))
      ((2 1) => (2 0))
      ((3 0) => (0))
      ((3 0) => (2 0))
      ((3 0) => (3))
      ((3 0) => (3 -1)))) |}]
;;

let fixed_point = Shrinker.fixed_point

let%expect_test "fixed_point" =
  test_shrinker
    (Shrinker.fixed_point (fun shrinker ->
       Shrinker.map
         (Shrinker.option shrinker)
         ~f:(function
           | None -> 0
           | Some n -> n + 1)
         ~f_inverse:(function
           | 0 -> None
           | n -> Some (n - 1))))
    (m_nat ~up_to:4);
  [%expect
    {|
    (shrinker
     ((1 => 0)
      (2 => 0)
      (2 => 1)
      (3 => 0)
      (3 => 1)
      (3 => 2)
      (4 => 0)
      (4 => 1)
      (4 => 2)
      (4 => 3))) |}]
;;

let both = Shrinker.both

let%expect_test "both" =
  test_shrinker
    (Shrinker.both natural_number_shrinker natural_number_shrinker)
    (m_pair (m_nat ~up_to:4) (m_nat ~up_to:4));
  [%expect
    {|
    (shrinker
     (((0 1) => (0 0))
      ((0 2) => (0 1))
      ((0 3) => (0 2))
      ((0 4) => (0 3))
      ((1 0) => (0 0))
      ((1 1) => (0 1))
      ((1 1) => (1 0))
      ((1 2) => (0 2))
      ((1 2) => (1 1))
      ((1 3) => (0 3))
      ((1 3) => (1 2))
      ((1 4) => (0 4))
      ((1 4) => (1 3))
      ((2 0) => (1 0))
      ((2 1) => (1 1))
      ((2 1) => (2 0))
      ((2 2) => (1 2))
      ((2 2) => (2 1))
      ((2 3) => (1 3))
      ((2 3) => (2 2))
      ((2 4) => (1 4))
      ((2 4) => (2 3))
      ((3 0) => (2 0))
      ((3 1) => (2 1))
      ((3 1) => (3 0))
      ((3 2) => (2 2))
      ((3 2) => (3 1))
      ((3 3) => (2 3))
      ((3 3) => (3 2))
      ((3 4) => (2 4))
      ((3 4) => (3 3))
      ((4 0) => (3 0))
      ((4 1) => (3 1))
      ((4 1) => (4 0))
      ((4 2) => (3 2))
      ((4 2) => (4 1))
      ((4 3) => (3 3))
      ((4 3) => (4 2))
      ((4 4) => (3 4))
      ((4 4) => (4 3)))) |}]
;;

let unit = Shrinker.unit

let%expect_test "unit" =
  test_shrinker ~mode:`atomic Shrinker.unit m_unit;
  [%expect {| (shrinker atomic) |}]
;;

let bool = Shrinker.bool

let%expect_test "bool" =
  test_shrinker ~mode:`atomic Shrinker.bool m_bool;
  [%expect {| (shrinker atomic) |}]
;;

let char = Shrinker.char

let%expect_test "char" =
  test_shrinker ~mode:`atomic Shrinker.char m_char;
  [%expect {| (shrinker atomic) |}]
;;

let string = Shrinker.string

let%expect_test "string" =
  test_shrinker Shrinker.string m_string;
  [%expect
    {|
    (shrinker
     ((A => "")
      (z => "")
      (0 => "")
      (_ => "")
      (" " => "")
      ("\000" => "")
      (AA => A)
      (AA => A)
      (zz => z)
      (zz => z)
      (00 => 0)
      (00 => 0)
      (__ => _)
      (__ => _)
      ("  " => " ")
      ("  " => " ")
      ("\000\000" => "\000")
      ("\000\000" => "\000"))) |}]
;;

let int = Shrinker.int

let%expect_test "int" =
  test_shrinker ~mode:`atomic Shrinker.int (m_int (module Int));
  [%expect {| (shrinker atomic) |}]
;;

let int32 = Shrinker.int32

let%expect_test "int32" =
  test_shrinker ~mode:`atomic Shrinker.int32 (m_int (module Int32));
  [%expect {| (shrinker atomic) |}]
;;

let int63 = Shrinker.int63

let%expect_test "int63" =
  test_shrinker ~mode:`atomic Shrinker.int63 (m_int (module Int63));
  [%expect {| (shrinker atomic) |}]
;;

let int64 = Shrinker.int64

let%expect_test "int64" =
  test_shrinker ~mode:`atomic Shrinker.int64 (m_int (module Int64));
  [%expect {| (shrinker atomic) |}]
;;

let nativeint = Shrinker.nativeint

let%expect_test "nativeint" =
  test_shrinker ~mode:`atomic Shrinker.nativeint (m_int (module Nativeint));
  [%expect {| (shrinker atomic) |}]
;;

let float = Shrinker.float

let%expect_test "float" =
  test_shrinker ~mode:`atomic Shrinker.float m_float;
  [%expect {| (shrinker atomic) |}]
;;

let sexp = Shrinker.sexp

let%expect_test "sexp" =
  test_shrinker Shrinker.sexp m_sexp;
  [%expect
    {|
    (shrinker
     (((a) => ())
      ((a) => a)
      ((bc) => ())
      ((bc) => bc)
      ((def) => ())
      ((def) => def)
      ((a bc def) => (bc def))
      ((a bc def) => a)
      ((a bc def) => (a def))
      ((a bc def) => bc)
      ((a bc def) => (a bc))
      ((a bc def) => def)
      ((a bc def (a) (bc) (def) (a bc def)) => (bc def (a) (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => a)
      ((a bc def (a) (bc) (def) (a bc def)) => (a def (a) (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => bc)
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc (a) (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => def)
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a))
      ((a bc def (a) (bc) (def) (a bc def))
       =>
       (a bc def () (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (bc))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (def))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def a (bc) (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) () (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) bc (def) (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) () (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) def (a bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) (bc def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) a))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) (a def)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) bc))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) (a bc)))
      ((a bc def (a) (bc) (def) (a bc def)) => (a bc def (a) (bc) (def) def)))) |}]
;;

let option = Shrinker.option

let%expect_test "option" =
  test_shrinker (Shrinker.option natural_number_shrinker) (m_option (m_nat ~up_to:4));
  [%expect
    {|
    (shrinker
     (((0) => ())
      ((1) => ())
      ((1) => (0))
      ((2) => ())
      ((2) => (1))
      ((3) => ())
      ((3) => (2))
      ((4) => ())
      ((4) => (3)))) |}]
;;

let list = Shrinker.list

let%expect_test "list" =
  test_shrinker (Shrinker.list natural_number_shrinker) (m_list (m_nat ~up_to:4));
  [%expect
    {|
    (shrinker
     (((0) => ())
      ((1) => ())
      ((1) => (0))
      ((2) => ())
      ((2) => (1))
      ((3) => ())
      ((3) => (2))
      ((4) => ())
      ((4) => (3))
      ((0 4) => (4))
      ((0 4) => (0))
      ((0 4) => (0 3))
      ((1 3) => (3))
      ((1 3) => (0 3))
      ((1 3) => (1))
      ((1 3) => (1 2))
      ((2 2) => (2))
      ((2 2) => (1 2))
      ((2 2) => (2))
      ((2 2) => (2 1))
      ((3 1) => (1))
      ((3 1) => (2 1))
      ((3 1) => (3))
      ((3 1) => (3 0))
      ((4 0) => (0))
      ((4 0) => (3 0))
      ((4 0) => (4)))) |}]
;;

let either = Shrinker.either

let%expect_test "either" =
  test_shrinker
    (Shrinker.either natural_number_shrinker natural_number_shrinker)
    (m_either (m_nat ~up_to:4) (m_nat ~up_to:4));
  [%expect
    {|
    (shrinker
     (((First 1) => (First 0))
      ((First 2) => (First 1))
      ((First 3) => (First 2))
      ((First 4) => (First 3))
      ((Second 1) => (Second 0))
      ((Second 2) => (Second 1))
      ((Second 3) => (Second 2))
      ((Second 4) => (Second 3)))) |}]
;;

let result = Shrinker.result

let%expect_test "result" =
  test_shrinker
    (Shrinker.result natural_number_shrinker natural_number_shrinker)
    (m_result (m_nat ~up_to:4) (m_nat ~up_to:4));
  [%expect
    {|
    (shrinker
     (((Ok 1) => (Ok 0))
      ((Ok 2) => (Ok 1))
      ((Ok 3) => (Ok 2))
      ((Ok 4) => (Ok 3))
      ((Error 1) => (Error 0))
      ((Error 2) => (Error 1))
      ((Error 3) => (Error 2))
      ((Error 4) => (Error 3)))) |}]
;;

let map_t = Shrinker.map_t
let map_tree_using_comparator = Shrinker.map_tree_using_comparator

let%expect_test "map_t" =
  test_shrinker
    (Shrinker.map_t natural_number_shrinker natural_number_shrinker)
    (m_map (module Int) (m_nat ~up_to:2) (m_nat ~up_to:2));
  [%expect
    {|
    (shrinker
     ((((0 0) (1 0) (2 0)) => ((1 0) (2 0)))
      (((0 0) (1 0) (2 0)) => ((0 0) (2 0)))
      (((0 0) (1 0) (2 0)) => ((0 0) (1 0)))
      (((0 1) (1 1) (2 1)) => ((1 1) (2 1)))
      (((0 1) (1 1) (2 1)) => ((0 0) (1 1) (2 1)))
      (((0 1) (1 1) (2 1)) => ((0 1) (2 1)))
      (((0 1) (1 1) (2 1)) => ((0 1) (1 0) (2 1)))
      (((0 1) (1 1) (2 1)) => ((0 1) (1 1)))
      (((0 1) (1 1) (2 1)) => ((0 1) (1 1) (2 0)))
      (((0 2) (1 2) (2 2)) => ((1 2) (2 2)))
      (((0 2) (1 2) (2 2)) => ((0 1) (1 2) (2 2)))
      (((0 2) (1 2) (2 2)) => ((0 2) (2 2)))
      (((0 2) (1 2) (2 2)) => ((0 2) (1 1) (2 2)))
      (((0 2) (1 2) (2 2)) => ((0 2) (1 2)))
      (((0 2) (1 2) (2 2)) => ((0 2) (1 2) (2 1))))) |}]
;;

let set_t = Shrinker.set_t
let set_tree_using_comparator = Shrinker.set_tree_using_comparator

let%expect_test "set_t" =
  test_shrinker
    (Shrinker.set_t natural_number_shrinker)
    (m_set (module Int) (m_nat ~up_to:5));
  [%expect
    {|
    (shrinker
     (((0) => ())
      ((1) => ())
      ((1) => (0))
      ((2) => ())
      ((2) => (1))
      ((3) => ())
      ((3) => (2))
      ((4) => ())
      ((4) => (3))
      ((5) => ())
      ((5) => (4))
      ((0 5) => (5))
      ((0 5) => (0 4))
      ((0 5) => (0))
      ((1 4) => (4))
      ((1 4) => (0 4))
      ((1 4) => (1))
      ((1 4) => (1 3))
      ((2 3) => (3))
      ((2 3) => (1 3))
      ((2 3) => (2)))) |}]
;;

let of_lazy = Shrinker.of_lazy

let%expect_test "of_lazy, forced" =
  test_shrinker (Shrinker.of_lazy (lazy Shrinker.string)) m_string;
  [%expect
    {|
    (shrinker
     ((A => "")
      (z => "")
      (0 => "")
      (_ => "")
      (" " => "")
      ("\000" => "")
      (AA => A)
      (AA => A)
      (zz => z)
      (zz => z)
      (00 => 0)
      (00 => 0)
      (__ => _)
      (__ => _)
      ("  " => " ")
      ("  " => " ")
      ("\000\000" => "\000")
      ("\000\000" => "\000"))) |}]
;;

let%expect_test "of_lazy, unforced" =
  test_shrinker
    (Shrinker.either Shrinker.string (Shrinker.of_lazy (lazy (assert false))))
    (m_biject
       m_string
       ~f:(fun string -> Either.First string)
       ~f_inverse:(function
         | Either.First string -> string
         | Either.Second (_ : (int, string) Type_equal.t) -> .));
  [%expect
    {|
    (shrinker
     (("\000" => "")
      (" " => "")
      (0 => "")
      (A => "")
      (_ => "")
      (z => "")
      ("\000\000" => "\000")
      ("\000\000" => "\000")
      ("  " => " ")
      ("  " => " ")
      (00 => 0)
      (00 => 0)
      (AA => A)
      (AA => A)
      (__ => _)
      (__ => _)
      (zz => z)
      (zz => z))) |}]
;;

let bigstring = Shrinker.bigstring
let float32_vec = Shrinker.float32_vec
let float64_vec = Shrinker.float64_vec

let%expect_test "[bigstring], [float32_vec], [float64_vec]" =
  let test (type elt pack layout) t sexp_of_elt examples kind layout =
    let module M = struct
      type t = (elt, pack, layout) Bigarray.Array1.t

      let compare = Poly.compare
      let sexp_of_t = [%sexp_of: (elt, _, _) Private.Bigarray_helpers.Array1.t]

      let examples =
        List.init
          (List.length examples + 1)
          ~f:(fun n ->
            List.take examples n |> Array.of_list |> Bigarray.Array1.of_array kind layout)
      ;;
    end
    in
    test_shrinker t (module M)
  in
  test Shrinker.bigstring [%sexp_of: char] [ 'a'; 'b' ] Char C_layout;
  [%expect {| (shrinker (((a) => ()) ((a b) => (b)) ((a b) => (a)))) |}];
  test Shrinker.float32_vec [%sexp_of: float] [ 1.; 2. ] Float32 Fortran_layout;
  [%expect {| (shrinker (((1) => ()) ((1 2) => (2)) ((1 2) => (1)))) |}];
  test Shrinker.float64_vec [%sexp_of: float] [ 1.; 2. ] Float64 Fortran_layout;
  [%expect {| (shrinker (((1) => ()) ((1 2) => (2)) ((1 2) => (1)))) |}]
;;

let float32_mat = Shrinker.float32_mat
let float64_mat = Shrinker.float64_mat

let%expect_test "[float32_mat]" =
  let test (type pack) t kind =
    let module M = struct
      type t = (float, pack, Bigarray.fortran_layout) Bigarray.Array2.t

      let compare = Poly.compare
      let sexp_of_t = [%sexp_of: (float, _, _) Private.Bigarray_helpers.Array2.t]

      let examples =
        List.init 3 ~f:(fun imax ->
          List.init 3 ~f:(fun jmax ->
            Array.init jmax ~f:(fun j ->
              Array.init imax ~f:(fun i -> Float.of_int (((j + 1) * 10) + i + 1)))
            |> Bigarray.Array2.of_array kind Fortran_layout))
        |> List.concat
      ;;
    end
    in
    test_shrinker t (module M);
    [%expect
      {|
      (shrinker
       (((()) => ())
        ((() ()) => (()))
        ((() ()) => (()))
        (((11)) => ())
        (((11)) => (()))
        (((11) (21)) => ((21)))
        (((11) (21)) => (() ()))
        (((11) (21)) => ((11)))
        (((11 12)) => ())
        (((11 12)) => ((12)))
        (((11 12)) => ((11)))
        (((11 12) (21 22)) => ((21 22)))
        (((11 12) (21 22)) => ((12) (22)))
        (((11 12) (21 22)) => ((11 12)))
        (((11 12) (21 22)) => ((11) (21))))) |}]
  in
  test float32_mat Bigarray.Float32;
  [%expect {| |}];
  test float64_mat Float64;
  [%expect {| |}]
;;
