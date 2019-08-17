open! Core_kernel
open! Import

module Cpuset = struct
  include Validated.Make (struct
      type t = Int.Set.t [@@deriving sexp]

      let here = [%here]

      let validate t =
        Validate.first_failure
          (Int.validate_lbound ~min:(Incl 1) (Int.Set.length t))
          (Int.Set.to_list t
           |> List.map ~f:Int.validate_non_negative
           |> Validate.name_list "Thread_pool_cpuset")
      ;;
    end)

  let equal t1 t2 = Int.Set.equal (t1 |> raw) (t2 |> raw)
end

type t =
  | Inherit
  | Cpuset of Cpuset.t
[@@deriving sexp]
