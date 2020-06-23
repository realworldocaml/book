open! Import

module type S = sig
  type t [@@deriving compare, sexp_of]

  val num_bits : int
  val min_value : t
  val minus_one : t
  val zero : t
  val one : t
  val max_value : t
  val to_int64 : t -> int64
  val shift_right : t -> int -> t
  val random : Random.State.t -> t -> t -> t
end

module I : S with type t = int = struct
  include Int

  let random = Random.State.int_incl
end

module Native : S with type t = nativeint = struct
  include Nativeint

  let random = Random.State.nativeint_incl
end

module I32 : S with type t = int32 = struct
  include Int32

  let random = Random.State.int32_incl
end

module I64 : S with type t = int64 = struct
  include Int64

  let random = Random.State.int64_incl
end

module I63 : S with type t = Int63.t = struct
  include Int63

  let random state lo hi = Int63.random_incl ~state lo hi
end

let iter (type a) (module M : S with type t = a) ~f =
  let state = Random.State.make [| 0; 1; 2; 3; 4; 5 |] in
  List.iter ~f [ M.min_value; M.minus_one; M.zero; M.one; M.max_value ];
  for _ = 1 to 10_000 do
    (* skew toward low numbers of bits so that, e.g., choosing a random int64 does
       frequently find a value that can be converted to int32. *)
    let strip_bits = Random.State.int_incl state 0 (M.num_bits - 1) in
    let lo = M.shift_right M.min_value strip_bits in
    let hi = M.shift_right M.max_value strip_bits in
    f (M.random state lo hi)
  done
;;

let try_with f x = Option.try_with (fun () -> f x)

(* Checks that a conversion from [A.t] to [B.t] is total using [of] and [to]. *)
let test_total
      (type a b)
      (module A : S with type t = a)
      (module B : S with type t = b)
      ~of_:b_of_a
      ~to_:a_to_b
  =
  iter
    (module A)
    ~f:(fun a ->
      require_compare_equal [%here] (module B) (b_of_a a) (a_to_b a);
      require_compare_equal [%here] (module Int64) (A.to_int64 a) (B.to_int64 (b_of_a a)))
;;

let truncate int64 ~num_bits =
  Int64.shift_right (Int64.shift_left int64 (64 - num_bits)) (64 - num_bits)
;;

(* Checks that a conversion from [A.t] to [B.t] is partial using [of] and [to], and the
   [_exn] equivalents. In the case where the conversion fails, ensure that the value,
   converted to an [Int64.t] is outside the representable range of [B.t] converted to an
   [Int64.t] as well. *)
let test_partial
      (type a b)
      (module A : S with type t = a)
      (module B : S with type t = b)
      ~of_:b_of_a
      ~of_exn:b_of_a_exn
      ~of_trunc:b_of_a_trunc
      ~to_:a_to_b
      ~to_exn:a_to_b_exn
      ~to_trunc:a_to_b_trunc
  =
  let module B_option = struct
    type t = B.t option [@@deriving compare, sexp_of]
  end
  in
  let convertible_count = ref 0 in
  iter
    (module A)
    ~f:(fun a ->
      require_compare_equal [%here] (module B_option) (b_of_a a) (a_to_b a);
      require_compare_equal [%here] (module B_option) (b_of_a a) (try_with b_of_a_exn a);
      require_compare_equal [%here] (module B_option) (a_to_b a) (try_with a_to_b_exn a);
      match b_of_a a with
      | Some b ->
        Int.incr convertible_count;
        require_compare_equal [%here] (module B) b (b_of_a_trunc a);
        require_compare_equal [%here] (module B) b (a_to_b_trunc a);
        require_compare_equal [%here] (module Int64) (A.to_int64 a) (B.to_int64 b)
      | None ->
        let trunc = truncate (A.to_int64 a) ~num_bits:B.num_bits in
        require_compare_equal [%here] (module Int64) trunc (B.to_int64 (b_of_a_trunc a));
        require_compare_equal [%here] (module Int64) trunc (B.to_int64 (a_to_b_trunc a));
        require
          [%here]
          (Int64.( > ) (A.to_int64 a) (B.to_int64 B.max_value)
           || Int64.( < ) (A.to_int64 a) (B.to_int64 B.min_value))
          ~if_false_then_print_s:(lazy [%message "failed to convert" ~_:(a : A.t)]));
  (* Make sure we stress the conversion a nontrivial number of times. This makes sure the
     random generation is useful and we aren't just testing the hard-coded examples. *)
  require
    [%here]
    (!convertible_count > 100)
    ~if_false_then_print_s:
      (lazy
        [%message
          "did not test successful conversion often enough" (convertible_count : int ref)])
;;

let%expect_test "int <-> nativeint" =
  test_total (module I) (module Native) ~of_:Nativeint.of_int ~to_:Int.to_nativeint;
  [%expect {| |}];
  test_partial
    (module Native)
    (module I)
    ~of_:Int.of_nativeint
    ~of_exn:Int.of_nativeint_exn
    ~of_trunc:Int.of_nativeint_trunc
    ~to_:Nativeint.to_int
    ~to_exn:Nativeint.to_int_exn
    ~to_trunc:Nativeint.to_int_trunc;
  [%expect {| |}]
;;

let%expect_test "int <-> int32" =
  test_partial
    (module I)
    (module I32)
    ~of_:Int32.of_int
    ~of_exn:Int32.of_int_exn
    ~of_trunc:Int32.of_int_trunc
    ~to_:Int.to_int32
    ~to_exn:Int.to_int32_exn
    ~to_trunc:Int.to_int32_trunc;
  [%expect {| |}];
  test_partial
    (module I32)
    (module I)
    ~of_:Int.of_int32
    ~of_exn:Int.of_int32_exn
    ~of_trunc:Int.of_int32_trunc
    ~to_:Int32.to_int
    ~to_exn:Int32.to_int_exn
    ~to_trunc:Int32.to_int_trunc;
  [%expect {| |}]
;;

let%expect_test "nativeint <-> int32" =
  test_partial
    (module Native)
    (module I32)
    ~of_:Int32.of_nativeint
    ~of_exn:Int32.of_nativeint_exn
    ~of_trunc:Int32.of_nativeint_trunc
    ~to_:Nativeint.to_int32
    ~to_exn:Nativeint.to_int32_exn
    ~to_trunc:Nativeint.to_int32_trunc;
  [%expect {| |}];
  test_total (module I32) (module Native) ~of_:Nativeint.of_int32 ~to_:Int32.to_nativeint;
  [%expect {| |}]
;;

let%expect_test "int <-> int64" =
  test_total (module I) (module I64) ~of_:Int64.of_int ~to_:Int.to_int64;
  [%expect {| |}];
  test_partial
    (module I64)
    (module I)
    ~of_:Int.of_int64
    ~of_exn:Int.of_int64_exn
    ~of_trunc:Int.of_int64_trunc
    ~to_:Int64.to_int
    ~to_exn:Int64.to_int_exn
    ~to_trunc:Int64.to_int_trunc;
  [%expect {| |}]
;;

let%expect_test "nativeint <-> int64" =
  test_total (module Native) (module I64) ~of_:Int64.of_nativeint ~to_:Nativeint.to_int64;
  [%expect {| |}];
  test_partial
    (module I64)
    (module Native)
    ~of_:Nativeint.of_int64
    ~of_exn:Nativeint.of_int64_exn
    ~of_trunc:Nativeint.of_int64_trunc
    ~to_:Int64.to_nativeint
    ~to_exn:Int64.to_nativeint_exn
    ~to_trunc:Int64.to_nativeint_trunc;
  [%expect {| |}]
;;

let%expect_test "int32 <-> int64" =
  test_total (module I32) (module I64) ~of_:Int64.of_int32 ~to_:Int32.to_int64;
  [%expect {| |}];
  test_partial
    (module I64)
    (module I32)
    ~of_:Int32.of_int64
    ~of_exn:Int32.of_int64_exn
    ~of_trunc:Int32.of_int64_trunc
    ~to_:Int64.to_int32
    ~to_exn:Int64.to_int32_exn
    ~to_trunc:Int64.to_int32_trunc;
  [%expect {| |}]
;;


let%expect_test "int <-> int63" =
  test_total (module I) (module I63) ~of_:Int63.of_int ~to_:Int63.of_int;
  [%expect {| |}];
  test_partial
    (module I63)
    (module I)
    ~of_:Int63.to_int
    ~of_exn:Int63.to_int_exn
    ~of_trunc:Int63.to_int_trunc
    ~to_:Int63.to_int
    ~to_exn:Int63.to_int_exn
    ~to_trunc:Int63.to_int_trunc;
  [%expect {| |}]
;;

let%expect_test "nativeint <-> int63" =
  test_partial
    (module Native)
    (module I63)
    ~of_:Int63.of_nativeint
    ~of_exn:Int63.of_nativeint_exn
    ~of_trunc:Int63.of_nativeint_trunc
    ~to_:Int63.of_nativeint
    ~to_exn:Int63.of_nativeint_exn
    ~to_trunc:Int63.of_nativeint_trunc;
  [%expect {| |}];
  test_partial
    (module I63)
    (module Native)
    ~of_:Int63.to_nativeint
    ~of_exn:Int63.to_nativeint_exn
    ~of_trunc:Int63.to_nativeint_trunc
    ~to_:Int63.to_nativeint
    ~to_exn:Int63.to_nativeint_exn
    ~to_trunc:Int63.to_nativeint_trunc;
  [%expect {| |}]
;;

let%expect_test "int32 <-> int63" =
  test_total (module I32) (module I63) ~of_:Int63.of_int32 ~to_:Int63.of_int32;
  [%expect {| |}];
  test_partial
    (module I63)
    (module I32)
    ~of_:Int63.to_int32
    ~of_exn:Int63.to_int32_exn
    ~of_trunc:Int63.to_int32_trunc
    ~to_:Int63.to_int32
    ~to_exn:Int63.to_int32_exn
    ~to_trunc:Int63.to_int32_trunc;
  [%expect {| |}]
;;

let%expect_test "int64 <-> int63" =
  test_partial
    (module I64)
    (module I63)
    ~of_:Int63.of_int64
    ~of_exn:Int63.of_int64_exn
    ~of_trunc:Int63.of_int64_trunc
    ~to_:Int63.of_int64
    ~to_exn:Int63.of_int64_exn
    ~to_trunc:Int63.of_int64_trunc;
  [%expect {| |}];
  test_total (module I63) (module I64) ~of_:Int63.to_int64 ~to_:Int63.to_int64;
  [%expect {| |}]
;;
