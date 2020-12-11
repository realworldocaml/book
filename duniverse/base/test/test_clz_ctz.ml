open! Import

module E = struct
  type t =
    { clz : int
    ; ctz : int
    }
  [@@deriving compare, sexp_of]
end

module type T = sig
  type t [@@deriving sexp_of]

  val one : t
  val ( lsl ) : t -> int -> t
  val clz : t -> int
  val ctz : t -> int
  val num_bits : int
end

module Make (Int : T) = struct
  let%expect_test "one-hot" =
    let clz_and_ctz int = { E.clz = Int.clz int; ctz = Int.ctz int } in
    for i = 0 to Int.num_bits - 1 do
      [%test_result: E.t]
        ~expect:{ E.clz = Int.num_bits - 1 - i; ctz = i }
        (clz_and_ctz Int.(one lsl i))
    done
  ;;
end

include Make (Nativeint)
include Make (Int63)
include Make (Int63.Private.Emul)

include Make (struct
    include Int

    let%expect_test "zero" =
      (* [clz 0] is guaranteed to be num_bits for int. We compute clz on the tagged
         representation of int's, and the binary representation of the int [0] is
         num_bits 0's followed by a 1 (the tag bit). *)
      [%test_result: int] ~expect:num_bits (clz 0)
    ;;

    (* [ctz 0] is unspecified. On linux it seems to be stable and equal to the system
       word size (which is num_bits + 1).
       ran 2019-02-11 on linux:
       {v
        [%test_result: int] ~expect:(num_bits + 1) (ctz 0)
       v}

       in javascript, it is 32 (which is num_bits):
       ran 2019-02-11 on javascript:
       {v
        [%test_result: int] ~expect:(num_bits) (ctz 0)
       v}

    *)
  end)

include Make (struct
    include Int32

    let clz_and_ctz i32 = { E.clz = clz i32; ctz = ctz i32 }

    let%expect_test "extra examples" =
      [%test_result: E.t] ~expect:{ clz = 31; ctz = 0 } (clz_and_ctz 0b1l);
      [%test_result: E.t] ~expect:{ clz = 30; ctz = 1 } (clz_and_ctz 0b10l);
      [%test_result: E.t] ~expect:{ clz = 30; ctz = 0 } (clz_and_ctz 0b11l);
      [%test_result: E.t] ~expect:{ clz = 25; ctz = 1 } (clz_and_ctz 0b1000010l);
      [%test_result: E.t]
        ~expect:{ clz = 8; ctz = 6 }
        (clz_and_ctz 0b100000010000001001000000l);
      [%test_result: E.t]
        ~expect:{ clz = 0; ctz = 31 }
        (clz_and_ctz 0b10000000000000000000000000000000l);
      [%test_result: E.t]
        ~expect:{ clz = 9; ctz = 6 }
        (clz_and_ctz 0b00000000010000000100000001000000l);
      [%test_result: E.t]
        ~expect:{ clz = 0; ctz = 6 }
        (clz_and_ctz 0b10000000010000000100000001000000l)
    ;;
  end)

include Make (struct
    include Int64

    let clz_and_ctz i64 = { E.clz = clz i64; ctz = ctz i64 }

    let%expect_test "extra examples" =
      [%test_result: E.t] ~expect:{ clz = 63; ctz = 0 } (clz_and_ctz 0b1L);
      [%test_result: E.t] ~expect:{ clz = 62; ctz = 1 } (clz_and_ctz 0b10L);
      [%test_result: E.t] ~expect:{ clz = 62; ctz = 0 } (clz_and_ctz 0b11L);
      [%test_result: E.t] ~expect:{ clz = 57; ctz = 1 } (clz_and_ctz 0b1000010L);
      [%test_result: E.t]
        ~expect:{ clz = 40; ctz = 6 }
        (clz_and_ctz 0b100000010000001001000000L);
      [%test_result: E.t]
        ~expect:{ clz = 0; ctz = 63 }
        (clz_and_ctz 0b1000000000000000000000000000000000000000000000000000000000000000L);
      [%test_result: E.t]
        ~expect:{ clz = 32; ctz = 31 }
        (clz_and_ctz 0b0000000000000000000000000000000010000000000000000000000000000000L);
      [%test_result: E.t]
        ~expect:{ clz = 32; ctz = 6 }
        (clz_and_ctz 0b0000000000000000000000000000000010000000010000000100000001000000L);
      [%test_result: E.t]
        ~expect:{ clz = 33; ctz = 6 }
        (clz_and_ctz 0b0000000000000000000000000000000001000000010000000100000001000000L)
    ;;
  end)
