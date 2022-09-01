open! Import

let arrow
      ~generator_of_core_type
      ~observer_of_core_type
      ~loc
      ~arg_label
      ~input_type
      ~output_type
  =
  let input_observer =
    match arg_label with
    | Nolabel | Labelled _ -> observer_of_core_type input_type
    | Optional _ ->
      [%expr
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.option
          [%e observer_of_core_type input_type]]
  in
  let output_generator = generator_of_core_type output_type in
  let unlabelled =
    [%expr
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
        [%e input_observer]
        [%e output_generator]]
  in
  match arg_label with
  | Nolabel -> unlabelled
  | Labelled _ | Optional _ ->
    [%expr
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
        ~f:[%e fn_map_label ~loc ~from:Nolabel ~to_:arg_label]
        [%e unlabelled]]
;;

let compound_generator ~loc ~make_compound_expr generator_list =
  let loc = { loc with loc_ghost = true } in
  let size_pat, size_expr = gensym "size" loc in
  let random_pat, random_expr = gensym "random" loc in
  [%expr
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.create
      (fun ~size:[%p size_pat] ~random:[%p random_pat] ->
         [%e
           make_compound_expr
             ~loc
             (List.map generator_list ~f:(fun generator ->
                let loc = { generator.pexp_loc with loc_ghost = true } in
                [%expr
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.generate
                    [%e generator]
                    ~size:[%e size_expr]
                    ~random:[%e random_expr]]))])]
;;

let compound
      (type field)
      ~generator_of_core_type
      ~loc
      ~fields
      (module Field : Field_syntax.S with type ast = field)
  =
  let fields = List.map fields ~f:Field.create in
  compound_generator
    ~loc
    ~make_compound_expr:(Field.expression fields)
    (List.map fields ~f:(fun field -> generator_of_core_type (Field.core_type field)))
;;

let does_refer_to name_set =
  object (self)
    inherit [bool] Ast_traverse.fold as super

    method! core_type ty acc =
      match ty.ptyp_desc with
      | Ptyp_constr (name, args) ->
        acc
        || Set.mem name_set (Longident.name name.txt)
        || List.exists args ~f:(fun arg -> self#core_type arg false)
      | _ -> super#core_type ty acc
  end
;;

let clause_is_recursive
      (type clause)
      ~clause
      ~rec_names
      (module Clause : Clause_syntax.S with type t = clause)
  =
  List.exists (Clause.core_type_list clause) ~f:(fun ty ->
    (does_refer_to rec_names)#core_type ty false)
;;

let variant
      (type clause)
      ~generator_of_core_type
      ~loc
      ~variant_type
      ~clauses
      ~rec_names
      (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  let make_generator clause =
    compound_generator
      ~loc:(Clause.location clause)
      ~make_compound_expr:(Clause.expression clause variant_type)
      (List.map (Clause.core_type_list clause) ~f:generator_of_core_type)
  in
  let make_pair clause =
    Option.map (Clause.weight clause) ~f:(fun weight ->
      pexp_tuple
        ~loc:{ (Clause.location clause) with loc_ghost = true }
        [ weight; make_generator clause ])
  in
  (* We filter out clauses with weight None now. If we don't, then we can get code in
     [body] below that relies on bindings that don't get generated. *)
  let clauses =
    List.filter clauses ~f:(fun clause -> Option.is_some (Clause.weight clause))
  in
  match
    List.partition_tf clauses ~f:(fun clause ->
      clause_is_recursive ~clause ~rec_names (module Clause))
  with
  | [], clauses | clauses, [] ->
    let pairs = List.filter_map clauses ~f:make_pair in
    [%expr
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
        [%e elist ~loc pairs]]
  | recursive_clauses, nonrecursive_clauses ->
    let size_pat, size_expr = gensym "size" loc in
    let nonrec_pat, nonrec_expr = gensym "gen" loc in
    let rec_pat, rec_expr = gensym "gen" loc in
    let nonrec_pats, nonrec_exprs =
      gensyms "pair" (List.map nonrecursive_clauses ~f:Clause.location)
    in
    let rec_pats, rec_exprs =
      gensyms "pair" (List.map recursive_clauses ~f:Clause.location)
    in
    let bindings =
      List.filter_opt
        (List.map2_exn nonrec_pats nonrecursive_clauses ~f:(fun pat clause ->
           let loc = { (Clause.location clause) with loc_ghost = true } in
           Option.map (make_pair clause) ~f:(fun expr -> value_binding ~loc ~pat ~expr))
         @ List.map2_exn rec_pats recursive_clauses ~f:(fun pat clause ->
           Option.map (Clause.weight clause) ~f:(fun weight_expr ->
             let loc = { (Clause.location clause) with loc_ghost = true } in
             let gen_expr =
               [%expr
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                   ~f:(fun [%p size_pat] ->
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                       ~size:(Ppx_quickcheck_runtime.Base.Int.pred [%e size_expr])
                       [%e make_generator clause])]
             in
             let expr = pexp_tuple ~loc [ weight_expr; gen_expr ] in
             value_binding ~loc ~pat ~expr)))
    in
    let body =
      [%expr
        let [%p nonrec_pat] =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [%e elist ~loc nonrec_exprs]
        and [%p rec_pat] =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [%e elist ~loc (nonrec_exprs @ rec_exprs)]
        in
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
          ~f:(function
            | 0 -> [%e nonrec_expr]
            | _ -> [%e rec_expr])]
    in
    pexp_let ~loc Nonrecursive bindings body
;;
