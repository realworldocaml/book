open! Base
open! Import
open Base_quickcheck
open Sexplib.Conv
module Array1 = Base_quickcheck.Private.Bigarray_helpers.Array1
module Array2 = Base_quickcheck.Private.Bigarray_helpers.Array2

module type S = sig
  type t [@@deriving sexp_of]

  include Base_quickcheck.Test.S with type t := t

  module Sexpable : sig
    type nonrec t = t [@@deriving sexp]
  end
end

let test ?cr (type t) (module M : S with type t = t) =
  let test original =
    let round_tripped = [%of_sexp: M.Sexpable.t] [%sexp (original : M.Sexpable.t)] in
    match Poly.([%compare.equal: t]) original round_tripped with
    | true -> ()
    | false ->
      print_cr ?cr [%here] [%message "Unequal." (original : M.t) (round_tripped : M.t)]
  in
  require_does_not_raise ?cr [%here] (fun () -> Test.run_exn (module M) ~f:test)
;;

let%expect_test "bigstring" =
  test
    (module struct
      type t = bigstring

      let sexp_of_t = [%sexp_of: (char, _, _) Array1.t]
      let quickcheck_generator = Base_quickcheck.Generator.bigstring
      let quickcheck_shrinker = Base_quickcheck.Shrinker.bigstring

      module Sexpable = struct
        type t = bigstring [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "float32_vec" =
  test
    (module struct
      type t = float32_vec

      let sexp_of_t = [%sexp_of: (float, _, _) Array1.t]
      let quickcheck_generator = Base_quickcheck.Generator.float32_vec
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float32_vec

      module Sexpable = struct
        type t = float32_vec [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "float64_vec" =
  test
    (module struct
      type t = float64_vec

      let sexp_of_t = [%sexp_of: (float, _, _) Array1.t]
      let quickcheck_generator = Base_quickcheck.Generator.float64_vec
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float64_vec

      module Sexpable = struct
        type t = float64_vec [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "vec" =
  test
    (module struct
      type t = vec

      let sexp_of_t = [%sexp_of: (float, _, _) Array1.t]
      let quickcheck_generator = Base_quickcheck.Generator.float64_vec
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float64_vec

      module Sexpable = struct
        type t = vec [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "float32_mat" =
  test
    ~cr:CR_soon
    (module struct
      type t = float32_mat

      let sexp_of_t = [%sexp_of: (float, _, _) Array2.t]
      let quickcheck_generator = Base_quickcheck.Generator.float32_mat
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float32_mat

      module Sexpable = struct
        type t = float32_mat [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "float64_mat" =
  test
    ~cr:CR_soon
    (module struct
      type t = float64_mat

      let sexp_of_t = [%sexp_of: (float, _, _) Array2.t]
      let quickcheck_generator = Base_quickcheck.Generator.float64_mat
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float64_mat

      module Sexpable = struct
        type t = float64_mat [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;

let%expect_test "mat" =
  test
    ~cr:CR_soon
    (module struct
      type t = mat

      let sexp_of_t = [%sexp_of: (float, _, _) Array2.t]
      let quickcheck_generator = Base_quickcheck.Generator.float64_mat
      let quickcheck_shrinker = Base_quickcheck.Shrinker.float64_mat

      module Sexpable = struct
        type t = mat [@@deriving sexp]
      end
    end);
  [%expect {| |}]
;;
