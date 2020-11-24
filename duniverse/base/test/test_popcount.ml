open! Import

module type T = sig
  type t [@@deriving compare, sexp_of]

  (* for implementing popcount_naive *)

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( lsr ) : t -> int -> t
  val ( land ) : t -> t -> t
  val quickcheck_generator : t Quickcheck.Generator.t
  val to_int_exn : t -> int
  val popcount : t -> int
end

module Make (Int : T) = struct
  let popcount_naive (int : Int.t) : int =
    let open Int in
    let rec loop n count =
      if Int.compare n zero <> 0 then loop (n lsr 1) (count + (n land one)) else count
    in
    loop int zero |> to_int_exn
  ;;

  let%test_unit _ =
    Quickcheck.test Int.quickcheck_generator ~sexp_of:[%sexp_of: Int.t] ~f:(fun int ->
      let expect = popcount_naive int in
      [%test_result: int] ~expect (Int.popcount int))
  ;;
end

include Make (Quickcheck.Int)
include Make (Quickcheck.Int32)
include Make (Quickcheck.Int64)
include Make (Quickcheck.Nativeint)
