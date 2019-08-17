module Bisect_visit___refutation_clause_403___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\007\000\000\000\003\000\000\000\t\000\000\000\t\160\160KA\160X@" in
      let `Staged cb =
        Bisect.Runtime.register_file "refutation_clause_403.ml"
          ~point_count:2 ~point_definitions in
      cb
  end
open Bisect_visit___refutation_clause_403___ml
let test =
  ___bisect_visit___ 1;
  (function | () -> (___bisect_visit___ 0; ()) | () -> .)
