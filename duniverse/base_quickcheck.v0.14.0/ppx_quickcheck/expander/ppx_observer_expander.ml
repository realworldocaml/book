open! Import

let any ~loc = [%expr Base_quickcheck.Observer.opaque]

let arrow
      ~observer_of_core_type
      ~generator_of_core_type
      ~loc
      ~arg_label
      ~input_type
      ~output_type
  =
  let input_generator =
    match arg_label with
    | Nolabel | Labelled _ -> generator_of_core_type input_type
    | Optional _ ->
      [%expr Base_quickcheck.Generator.option [%e generator_of_core_type input_type]]
  in
  let output_observer = observer_of_core_type output_type in
  let unlabelled =
    [%expr Base_quickcheck.Observer.fn [%e input_generator] [%e output_observer]]
  in
  match arg_label with
  | Nolabel -> unlabelled
  | Labelled _ | Optional _ ->
    [%expr
      Base_quickcheck.Observer.unmap
        ~f:[%e fn_map_label ~loc ~from:arg_label ~to_:Nolabel]
        [%e unlabelled]]
;;

let compound_hash ~loc ~size_expr ~hash_expr ~hash_pat ~observer_exprs ~field_exprs =
  let alist = List.zip_exn observer_exprs field_exprs in
  List.fold_right alist ~init:hash_expr ~f:(fun (observer_expr, field_expr) body_expr ->
    [%expr
      let [%p hash_pat] =
        Base_quickcheck.Observer.observe
          [%e observer_expr]
          [%e field_expr]
          ~size:[%e size_expr]
          ~hash:[%e hash_expr]
      in
      [%e body_expr]])
;;

let compound
      (type field)
      ~observer_of_core_type
      ~loc
      ~fields
      (module Field : Field_syntax.S with type ast = field)
  =
  let fields = List.map fields ~f:Field.create in
  let field_pats, field_exprs = gensyms "x" (List.map fields ~f:Field.location) in
  let pat = Field.pattern fields ~loc field_pats in
  let observer_exprs =
    List.map fields ~f:(fun field -> observer_of_core_type (Field.core_type field))
  in
  let size_pat, size_expr = gensym "size" loc in
  let hash_pat, hash_expr = gensym "hash" loc in
  [%expr
    Base_quickcheck.Observer.create
      (fun [%p pat] ~size:[%p size_pat] ~hash:[%p hash_pat] ->
         [%e
           compound_hash ~loc ~size_expr ~hash_expr ~hash_pat ~observer_exprs ~field_exprs])]
;;

let variant
      (type clause)
      ~observer_of_core_type
      ~loc
      ~clauses
      (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  let pat, expr = gensym "x" loc in
  let size_pat, size_expr = gensym "size" loc in
  let hash_pat, hash_expr = gensym "hash" loc in
  [%expr
    Base_quickcheck.Observer.create
      (fun [%p pat] ~size:[%p size_pat] ~hash:[%p hash_pat] ->
         [%e
           pexp_match
             ~loc
             expr
             (List.map clauses ~f:(fun clause ->
                let core_type_list = Clause.core_type_list clause in
                let observer_exprs = List.map core_type_list ~f:observer_of_core_type in
                let field_pats, field_exprs =
                  gensyms
                    "x"
                    (List.map core_type_list ~f:(fun core_type -> core_type.ptyp_loc))
                in
                let lhs = Clause.pattern clause ~loc field_pats in
                let body =
                  compound_hash
                    ~loc
                    ~size_expr
                    ~hash_expr
                    ~hash_pat
                    ~observer_exprs
                    ~field_exprs
                in
                let rhs =
                  match Clause.salt clause with
                  | None -> body
                  | Some salt ->
                    pexp_let
                      ~loc
                      Nonrecursive
                      [ value_binding
                          ~loc
                          ~pat:hash_pat
                          ~expr:
                            [%expr
                              Base.hash_fold_int [%e hash_expr] [%e eint ~loc salt]]
                      ]
                      body
                in
                case ~lhs ~guard:None ~rhs))])]
;;
