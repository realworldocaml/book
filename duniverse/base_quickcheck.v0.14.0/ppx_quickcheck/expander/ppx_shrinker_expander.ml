open! Import

let any ~loc = [%expr Base_quickcheck.Shrinker.atomic]
let arrow ~loc = [%expr Base_quickcheck.Shrinker.atomic]

let compound_sequence ~loc ~make_compound_expr ~field_pats ~field_exprs ~shrinker_exprs =
  [%expr
    Base.Sequence.round_robin
      [%e
        elist
          ~loc
          (List.map3_exn
             field_pats
             field_exprs
             shrinker_exprs
             ~f:(fun field_pat field_expr shrinker ->
               let loc = { shrinker.pexp_loc with loc_ghost = true } in
               [%expr
                 Base.Sequence.map
                   (Base_quickcheck.Shrinker.shrink [%e shrinker] [%e field_expr])
                   ~f:(fun [%p field_pat] -> [%e make_compound_expr ~loc field_exprs])]))]]
;;

let compound
      (type field)
      ~shrinker_of_core_type
      ~loc
      ~fields
      (module Field : Field_syntax.S with type ast = field)
  =
  let fields = List.map fields ~f:Field.create in
  let field_pats, field_exprs = gensyms "x" (List.map fields ~f:Field.location) in
  let shrinker_exprs =
    List.map fields ~f:(fun field -> shrinker_of_core_type (Field.core_type field))
  in
  [%expr
    Base_quickcheck.Shrinker.create (fun [%p Field.pattern fields ~loc field_pats] ->
      [%e
        compound_sequence
          ~loc
          ~make_compound_expr:(Field.expression fields)
          ~field_pats
          ~field_exprs
          ~shrinker_exprs])]
;;

let variant
      (type clause)
      ~shrinker_of_core_type
      ~loc
      ~variant_type
      ~clauses
      (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  [%expr
    Base_quickcheck.Shrinker.create
      [%e
        pexp_function
          ~loc
          (List.map clauses ~f:(fun clause ->
             let loc = { (Clause.location clause) with loc_ghost = true } in
             let core_type_list = Clause.core_type_list clause in
             let field_pats, field_exprs =
               gensyms
                 "x"
                 (List.map core_type_list ~f:(fun core_type -> core_type.ptyp_loc))
             in
             let shrinker_exprs = List.map core_type_list ~f:shrinker_of_core_type in
             let lhs = Clause.pattern clause ~loc field_pats in
             let rhs =
               compound_sequence
                 ~loc
                 ~make_compound_expr:(Clause.expression clause variant_type)
                 ~field_pats
                 ~field_exprs
                 ~shrinker_exprs
             in
             case ~lhs ~guard:None ~rhs))]]
;;
