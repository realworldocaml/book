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
    | Nolabel | Labelled _ ->
      observer_of_core_type input_type
    | Optional _ ->
      [%expr
        Base_quickcheck.Observer.option
          [%e observer_of_core_type input_type]]
  in
  let output_generator =
    generator_of_core_type output_type
  in
  let unlabelled =
    [%expr
      Base_quickcheck.Generator.fn
        [%e input_observer]
        [%e output_generator]]
  in
  match arg_label with
  | Nolabel -> unlabelled
  | Labelled _
  | Optional _ ->
    [%expr
      Base_quickcheck.Generator.map
        ~f:[%e fn_map_label ~loc ~from:Nolabel ~to_:arg_label]
        [%e unlabelled]]

let compound_generator ~loc ~make_compound_expr generator_list =
  let size_pat, size_expr = gensym "size" loc in
  let random_pat, random_expr = gensym "random" loc in
  [%expr
    Base_quickcheck.Generator.create
      (fun ~size:[%p size_pat] ~random:[%p random_pat] ->
         [%e
           make_compound_expr ~loc
             (List.map generator_list ~f:(fun generator ->
                let loc = generator.pexp_loc in
                [%expr
                  Base_quickcheck.Generator.generate
                    [%e generator]
                    ~size:[%e size_expr]
                    ~random:[%e random_expr]]))])]

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
    (List.map fields ~f:(fun field ->
       generator_of_core_type (Field.core_type field)))

let variant
      (type clause)
      ~generator_of_core_type
      ~loc
      ~variant_type
      ~clauses
      (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  let generators =
    List.map clauses ~f:(fun clause ->
      let loc = Clause.location clause in
      compound_generator
        ~loc
        ~make_compound_expr:(Clause.expression clause variant_type)
        (List.map (Clause.core_type_list clause) ~f:generator_of_core_type))
  in
  [%expr Base_quickcheck.Generator.union [%e elist ~loc generators]]
