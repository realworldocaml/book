open Core

module Stable = struct
  module V1 = struct
    type t =
      { name : string
      ; version : int
      }
    [@@deriving bin_io, compare, hash, sexp]
  end
end

include Stable.V1
include Comparable.Make (Stable.V1)
include Hashable.Make (Stable.V1)

let summarize ts =
  List.map ts ~f:(fun { name; version } -> name, version)
  |> String.Map.of_alist_fold ~init:Int.Set.empty ~f:Int.Set.add
;;

let%expect_test _ =
  let descriptions =
    [ { name = "foo"; version = 1 }
    ; { name = "foo"; version = 2 }
    ; { name = "bar"; version = 5 }
    ]
  in
  let summary = summarize descriptions in
  print_s [%sexp (summary : Int.Set.t String.Map.t)];
  [%expect {| ((bar (5)) (foo (1 2))) |}]
;;
