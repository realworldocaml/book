open! Import

type 'a t = 'a Observer.t

let create = Observer.create
let observe = Observer.observe

let%expect_test ("observe & create"[@tags "64-bits-only"]) =
  let obs =
    Observer.create (fun x ~size ~hash ->
      hash_fold_int
        hash
        (* make sure to use [size] so we can tell it is threaded in properly *)
        (Int.min x size))
  in
  List.init 10 ~f:(fun size ->
    Observer.observe obs 6 ~size ~hash:(Hash.alloc ()) |> Hash.get_hash_value)
  |> [%sexp_of: int list]
  |> print_s;
  [%expect
    {|
    (1058613066
     129913994
     462777137
     883721435
     607293368
     648017920
     809201503
     809201503
     809201503
     809201503) |}]
;;

let opaque = Observer.opaque

let%expect_test "opaque" =
  test_observer ~mode:`opaque Observer.opaque (m_nat ~up_to:10);
  [%expect {| (observer opaque) |}]
;;

let unmap = Observer.unmap

let%expect_test "unmap" =
  test_observer (Observer.unmap Observer.int64 ~f:Int.to_int64) (m_int (module Int));
  [%expect {|
    (observer transparent) |}]
;;

let of_hash_fold = Observer.of_hash_fold

let%expect_test "of_hash_fold" =
  test_observer (Observer.of_hash_fold Sexp.hash_fold_t) m_sexp;
  [%expect {| (observer transparent) |}]
;;

let fixed_point = Observer.fixed_point

let%expect_test "fixed_point" =
  test_observer
    (Observer.fixed_point (fun observer ->
       Observer.unmap (Observer.option observer) ~f:(function
         | 0 -> None
         | n -> Some (n - 1))))
    (m_nat ~up_to:10);
  [%expect {| (observer transparent) |}]
;;

let fn = Observer.fn

let%expect_test "fn" =
  let config = { Test.default_config with test_count = 100 } in
  let first_order = m_arrow m_bool m_bool in
  let (module First_order) = first_order in
  test_observer ~config (Observer.fn Generator.bool Observer.bool) first_order;
  [%expect {| (observer transparent) |}];
  print_s [%sexp (First_order.examples : First_order.t list)];
  [%expect
    {|
    (((false false) (true false))
     ((false false) (true true))
     ((false true) (true false))
     ((false true) (true true))) |}];
  let higher_order = m_arrow first_order m_bool in
  let (module Higher_order) = higher_order in
  test_observer
    ~config
    (Observer.fn (Generator.fn Observer.bool Generator.bool) Observer.bool)
    higher_order;
  [%expect {| (observer transparent) |}]
;;

let both = Observer.both

let%expect_test "both" =
  test_observer (Observer.both Observer.bool Observer.bool) (m_pair m_bool m_bool);
  [%expect {| (observer transparent) |}]
;;

let unit = Observer.unit

let%expect_test "unit" =
  test_observer Observer.unit m_unit;
  [%expect {| (observer transparent) |}]
;;

let bool = Observer.bool

let%expect_test "bool" =
  test_observer Observer.bool m_bool;
  [%expect {| (observer transparent) |}]
;;

let char = Observer.char

let%expect_test "char" =
  test_observer Observer.char m_char;
  [%expect {| (observer transparent) |}]
;;

let string = Observer.string

let%expect_test "string" =
  test_observer Observer.string m_string;
  [%expect {| (observer transparent) |}]
;;

let int = Observer.int

let%expect_test "int" =
  test_observer Observer.int (m_int (module Int));
  [%expect {| (observer transparent) |}]
;;

let int32 = Observer.int32

let%expect_test ("int32"[@tags "64-bits-only"]) =
  test_observer Observer.int32 (m_int (module Int32));
  [%expect {| (observer transparent) |}]
;;

let int63 = Observer.int63

let%expect_test "int63" =
  test_observer Observer.int63 (m_int (module Int63));
  [%expect {| (observer transparent) |}]
;;

let int64 = Observer.int64

let%expect_test "int64" =
  test_observer Observer.int64 (m_int (module Int64));
  [%expect {|
    (observer transparent) |}]
;;

let nativeint = Observer.nativeint

let%expect_test "nativeint" =
  test_observer Observer.nativeint (m_int (module Nativeint));
  [%expect {|
    (observer transparent) |}]
;;

let float = Observer.float

let%expect_test "float" =
  test_observer Observer.float m_float;
  [%expect {| (observer transparent) |}]
;;

let sexp = Observer.sexp

let%expect_test "sexp" =
  test_observer Observer.sexp m_sexp;
  [%expect {| (observer transparent) |}]
;;

let option = Observer.option

let%expect_test "option" =
  [%expect {| |}];
  test_observer (Observer.option Observer.bool) (m_option m_bool);
  [%expect {| (observer transparent) |}]
;;

let list = Observer.list

let%expect_test "list" =
  test_observer (Observer.list Observer.bool) (m_list m_bool);
  [%expect {| (observer transparent) |}]
;;

let either = Observer.either

let%expect_test "either" =
  test_observer (Observer.either Observer.bool Observer.bool) (m_either m_bool m_bool);
  [%expect {| (observer transparent) |}]
;;

let result = Observer.result

let%expect_test "result" =
  test_observer (Observer.result Observer.bool Observer.bool) (m_result m_bool m_bool);
  [%expect {| (observer transparent) |}]
;;

let map_t = Observer.map_t
let map_tree = Observer.map_tree

let%expect_test "map_t" =
  test_observer
    (Observer.map_t Observer.bool Observer.bool)
    (m_map (module Bool) m_bool m_bool);
  [%expect {| (observer transparent) |}]
;;

let set_t = Observer.set_t
let set_tree = Observer.set_tree

let%expect_test "set_t" =
  test_observer (Observer.set_t Observer.bool) (m_set (module Bool) m_bool);
  [%expect {| (observer transparent) |}]
;;

let of_lazy = Observer.of_lazy

let%expect_test "of_lazy, forced" =
  test_observer (Observer.of_lazy (lazy Observer.string)) m_string;
  [%expect {| (observer transparent) |}]
;;

let%expect_test "of_lazy, unforced" =
  test_observer
    (Observer.either Observer.string (Observer.of_lazy (lazy (assert false))))
    (m_biject
       m_string
       ~f:(fun string -> Either.First string)
       ~f_inverse:(function
         | Either.First string -> string
         | Either.Second (_ : (int, string) Type_equal.t) -> .));
  [%expect {| (observer transparent) |}]
;;

let bigstring = Observer.bigstring

let%expect_test "[bigstring]" =
  test_observer
    Observer.bigstring
    (m_biject m_string ~f:Base_bigstring.of_string ~f_inverse:Base_bigstring.to_string);
  [%expect {| (observer transparent) |}]
;;

let float32_vec = Observer.float32_vec
let float64_vec = Observer.float64_vec

let%expect_test "[float32_vec], [float64_vec]" =
  let test observer kind =
    (module struct
      include (val m_float)

      let examples = [ 1.; -1.; Float.nan ]
    end)
    |> m_list
    |> m_biject ~f:Array.of_list ~f_inverse:Array.to_list
    |> m_biject
         ~f:(Bigarray.Array1.of_array kind Fortran_layout)
         ~f_inverse:Private.Bigarray_helpers.Array1.to_array
    |> test_observer observer
  in
  test float32_vec Float32;
  [%expect {| (observer transparent) |}];
  test float64_vec Float64;
  [%expect {| (observer transparent) |}]
;;

let float32_mat = Observer.float32_mat
let float64_mat = Observer.float64_mat

let%expect_test "[float32_mat], [float64_mat]" =
  let test observer kind =
    (module struct
      type t = float array array [@@deriving compare, sexp_of]

      let examples =
        [ [||]; [| [| 0. |] |]; [| [| 0.; 1. |] |]; [| [| 0. |]; [| 1. |] |] ]
      ;;
    end)
    |> m_biject
         ~f:(Bigarray.Array2.of_array kind Fortran_layout)
         ~f_inverse:Private.Bigarray_helpers.Array2.to_array
    |> test_observer observer
  in
  test float32_mat Float32;
  [%expect {| (observer transparent) |}];
  test float64_mat Float64;
  [%expect {| (observer transparent) |}]
;;
