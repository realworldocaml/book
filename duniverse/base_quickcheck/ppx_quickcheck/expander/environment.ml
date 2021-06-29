open! Import

type 'a or_raise =
  | Ok of 'a
  | Error of { fail : 'a. loc:location -> 'a }

type t = (string, expression or_raise, String.comparator_witness) Map.t

let empty = Map.empty (module String)

let lookup t ~loc ~tyvar =
  match Map.find t tyvar with
  | Some (Ok expr) -> expr
  | Some (Error { fail }) -> fail ~loc
  | None -> invalid ~loc "unbound type variable: '%s" tyvar
;;

let of_alist ~loc alist =
  match Map.of_alist (module String) alist with
  | `Ok t -> t
  | `Duplicate_key name -> invalid ~loc "duplicate type parameter: '%s" name
;;

let create ~loc ~prefix param_list =
  let pat_list, alist =
    List.map param_list ~f:(fun ((core_type, _) as param) ->
      let loc = core_type.ptyp_loc in
      let name = get_type_param_name param in
      let pat, expr = gensym prefix loc in
      pat, (name.txt, Ok expr))
    |> List.unzip
  in
  let t = of_alist ~loc alist in
  pat_list, t
;;

let variance_error ~loc ~tyvar ~actual ~expect =
  invalid
    ~loc
    "misuse of type variable '%s: would confuse %s with %s in generated code; could be \
     due to a missing or incorrect covariance/contravariance annotation"
    tyvar
    actual
    expect
;;

let create_with_variance ~loc ~covariant ~contravariant param_list =
  let pat_list, by_variance_list =
    List.map param_list ~f:(fun ((core_type, (variance, injectivity)) as param) ->
      let loc = core_type.ptyp_loc in
      let name = get_type_param_name param in
      match (variance, injectivity) with
      | ((NoVariance | Covariant), NoInjectivity) ->
        let pat, expr = gensym covariant loc in
        pat, `Covariant (name.txt, expr)
      | (Contravariant, NoInjectivity) ->
        let pat, expr = gensym contravariant loc in
        pat, `Contravariant (name.txt, expr)
      | (_, Injective) -> Location.raise_errorf ~loc "Injective type parameters aren't supported.")
    |> List.unzip
  in
  let covariant_t =
    List.map by_variance_list ~f:(function
      | `Covariant (tyvar, expr) -> tyvar, Ok expr
      | `Contravariant (tyvar, _) ->
        let fail ~loc =
          variance_error ~loc ~tyvar ~expect:covariant ~actual:contravariant
        in
        tyvar, Error { fail })
    |> of_alist ~loc
  in
  let contravariant_t =
    List.map by_variance_list ~f:(function
      | `Contravariant (tyvar, expr) -> tyvar, Ok expr
      | `Covariant (tyvar, _) ->
        let fail ~loc =
          variance_error ~loc ~tyvar ~expect:contravariant ~actual:covariant
        in
        tyvar, Error { fail })
    |> of_alist ~loc
  in
  pat_list, `Covariant covariant_t, `Contravariant contravariant_t
;;
