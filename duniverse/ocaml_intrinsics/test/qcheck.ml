include Base_quickcheck.Export

module type Value = sig
  type t [@@deriving compare, quickcheck, sexp_of]

  (* for implementing count_set_bits_naive *)

  val zero : t
  val one : t
  val num_bits : int
  val ( + ) : t -> t -> t
  val ( lsr ) : t -> int -> t
  val ( land ) : t -> t -> t
  val to_int_exn : t -> int
end

module type Test = sig
  type t

  val count_set_bits : t -> int
  val count_leading_zeros : t -> int
  val count_trailing_zeros : t -> int
  val count_leading_zeros_nonzero_arg : t -> int
  val count_trailing_zeros_nonzero_arg : t -> int
end

module Make (V : Value) (T : Test with type t = V.t) = struct
  let count_set_bits_naive (v : V.t) : int =
    let open V in
    let rec loop n count =
      if V.compare n zero <> 0 then loop (n lsr 1) (count + (n land one)) else count
    in
    loop v zero |> to_int_exn
  ;;

  let%test_unit "count_set_bits" =
    Base_quickcheck.Test.run_exn
      (module V)
      ~f:(fun v ->
        let expect = count_set_bits_naive v in
        let actual = T.count_set_bits v in
        [%test_result: Base.Int.t] ~expect actual)
  ;;

  let count_leading_zeros_naive (v : V.t) : int =
    let open V in
    let rec loop n count =
      if compare n zero <> 0 then loop (n lsr 1) Base.Int.(count - 1) else count
    in
    loop v V.num_bits
  ;;

  let%test_unit "count_leading_zeros" =
    Base_quickcheck.Test.run_exn
      (module V)
      ~f:(fun v ->
        let expect = count_leading_zeros_naive v in
        let actual = T.count_leading_zeros v in
        [%test_result: Base.Int.t] ~expect actual)
  ;;

  let%test_unit "count_leading_zeros_nonzero_arg" =
    Base_quickcheck.Test.run_exn
      (module V)
      ~f:(fun v ->
        if not (V.zero = v)
        then (
          let expect = count_leading_zeros_naive v in
          let actual = T.count_leading_zeros_nonzero_arg v in
          [%test_result: Base.Int.t] ~expect actual))
  ;;

  let count_trailing_zeros_naive (v : V.t) : int =
    let open V in
    let rec loop n count =
      let bit = n land one in
      if Base.Int.compare count V.num_bits = 0 || compare bit zero <> 0
      then count
      else loop (n lsr 1) Base.Int.(count + 1)
    in
    loop v 0
  ;;

  let%test_unit "count_trailing_zeros" =
    Base_quickcheck.Test.run_exn
      (module V)
      ~f:(fun v ->
        let expect = count_trailing_zeros_naive v in
        let actual = T.count_trailing_zeros v in
        [%test_result: Base.Int.t] ~expect actual)
  ;;

  let%test_unit "count_trailing_zeros_nonzero_arg" =
    Base_quickcheck.Test.run_exn
      (module V)
      ~f:(fun v ->
        if not (V.zero = v)
        then (
          let expect = count_trailing_zeros_naive v in
          let actual = T.count_trailing_zeros_nonzero_arg v in
          [%test_result: Base.Int.t] ~expect actual))
  ;;
end

module BI = struct
  include Base.Int

  type t = int [@@deriving quickcheck]
end

module I = struct
  include Ocaml_intrinsics.Int

  let count_trailing_zeros_nonzero_arg = count_trailing_zeros
  let count_leading_zeros_nonzero_arg = count_leading_zeros

  type t = int
end

include Make (BI) (I)

include
  Make
    (BI)
    (struct
      include I

      let count_leading_zeros = Ocaml_intrinsics.Int.count_leading_zeros2
      let count_set_bits = Ocaml_intrinsics.Int.count_set_bits2
    end)

(* (** Test Base.Int implementation of clz,popcount,ctz against the naive implementation.  *)
 * include
 *   Make
 *     (BI)
 *     (struct
 *       type t = Base.Int.t
 *
 *       let count_leading_zeros x = Base.Int.(if x = 0 then num_bits else clz x)
 *       let count_set_bits = Base.Int.popcount
 *
 *       (* Base.Int.ctz is undefined on 0, so just define it here to make the test pass. *)
 *       let count_trailing_zeros x = Base.Int.(if x = 0 then num_bits else ctz x)
 *     end) *)

(** Test Ocaml_intrinsics.Int64 implementation against the naive implementation.  *)
include
  Make
    (struct
      include Base.Int64

      type t = int64 [@@deriving quickcheck]
    end)
    (struct
      include Ocaml_intrinsics.Int64

      type t = int64
    end)

(** Test Base.Int64 implementation of clz,popcount,ctz against the naive implementation.  *)
include
  Make
    (struct
      include Base.Int64

      type t = int64 [@@deriving quickcheck]
    end)
    (struct
      type t = int64

      let count_trailing_zeros x = Base.Int64.(if x = 0L then num_bits else ctz x)
      let count_leading_zeros x = Base.Int64.(if x = 0L then num_bits else clz x)
      let count_trailing_zeros_nonzero_arg = count_trailing_zeros
      let count_leading_zeros_nonzero_arg = count_leading_zeros
      let count_set_bits = Base.Int64.popcount
    end)

(** Test Ocaml_intrinsics.Int32 implementation against the naive implementation.  *)
include
  Make
    (struct
      include Base.Int32

      type t = int32 [@@deriving quickcheck]
    end)
    (struct
      include Ocaml_intrinsics.Int32

      type t = int32
    end)

(** Test Base.Int32 implementation of clz,popcount,ctz against the naive implementation.  *)
include
  Make
    (struct
      include Base.Int32

      type t = int32 [@@deriving quickcheck]
    end)
    (struct
      type t = int32

      let count_trailing_zeros x = Base.Int32.(if x = 0l then num_bits else ctz x)
      let count_leading_zeros x = Base.Int32.(if x = 0l then num_bits else clz x)
      let count_trailing_zeros_nonzero_arg = count_trailing_zeros
      let count_leading_zeros_nonzero_arg = count_leading_zeros
      let count_set_bits = Base.Int32.popcount
    end)

(** Test Ocaml_intrinsics.Nativeint implementation against the naive implementation.  *)
include
  Make
    (struct
      include Base.Nativeint

      type t = nativeint [@@deriving quickcheck]
    end)
    (struct
      include Ocaml_intrinsics.Nativeint

      type t = nativeint
    end)

(** Test Base.Nativeint implementation of clz,popcount,ctz against the naive
    implementation.  *)
include
  Make
    (struct
      include Base.Nativeint

      type t = nativeint [@@deriving quickcheck]
    end)
    (struct
      type t = nativeint

      let count_leading_zeros x = Base.Nativeint.(if x = 0n then num_bits else clz x)
      let count_set_bits = Base.Nativeint.popcount
      let count_trailing_zeros x = Base.Nativeint.(if x = 0n then num_bits else ctz x)
      let count_trailing_zeros_nonzero_arg = count_trailing_zeros
      let count_leading_zeros_nonzero_arg = count_leading_zeros
    end)
