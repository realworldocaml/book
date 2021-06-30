open Base
open Ppxlib
open Ast_builder.Default

module List = struct
  include List

  (* All functions copied from core_list.ml (except for [invalid_argf], which is copied
     from core_printf.ml. *)
  let invalid_argf fmt = Printf.ksprintf (fun s () -> invalid_arg s) fmt

  let init n ~f =
    if n < 0 then invalid_argf "List.init %d" n ();
    let rec loop i accum =
      assert (i >= 0);
      if i = 0 then accum
      else loop (i-1) (f (i-1) :: accum)
    in
    loop n []
  ;;

  let split_n t_orig n =
  if n <= 0 then
    ([], t_orig)
  else
    let rec loop n t accum =
      if n = 0 then
        (List.rev accum, t)
      else
        match t with
        | [] -> (t_orig, []) (* in this case, t_orig = List.rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig []

  let take t n = fst (split_n t n)
  let drop t n = snd (split_n t n)

  let rev_mapi l ~f =
    let rec loop i acc = function
      | [] -> acc
      | h :: t -> loop (i + 1) (f i h :: acc) t
    in
    loop 0 [] l

  let mapi l ~f = List.rev (rev_mapi l ~f)
end

let name_of_type_name = function
  | "t" -> "all"
  | type_name -> "all_of_" ^ type_name
let name_of_type_variable str =
  "_" ^ name_of_type_name str

(* Utility functions *)
let enumeration_type_of_td td =
  let init =
    let tp = core_type_of_type_declaration td in
    let loc = tp.ptyp_loc in
    [%type: [%t tp] list]
  in
  List.fold_right td.ptype_params ~init
    ~f:(fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      [%type: [%t tp] list -> [%t acc] ])
;;

let sig_of_tds ~loc:_ ~path:_ (_rec_flag, tds) =
  List.map tds ~f:(fun td ->
    let td = name_type_params_in_td td in
    let enumeration_type = enumeration_type_of_td td in
    let name = name_of_type_name td.ptype_name.txt in
    let loc = td.ptype_loc in
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name)
                       ~type_:enumeration_type ~prim:[]))

let gen_symbol = gen_symbol ~prefix:"enumerate"

let tuple loc exprs =
  assert (List.length exprs >= 2);
  pexp_tuple ~loc exprs
let patt_tuple loc pats =
  assert (List.length pats >= 2);
  ppat_tuple ~loc pats
let apply e el = eapply ~loc:e.pexp_loc e el

let replace_variables_by_underscores =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc ty =
      match super#core_type_desc ty with
      | Ptyp_var _ -> Ptyp_any
      | ty -> ty
  end in
  map#core_type
;;

let list_map loc l ~f =
  let element = gen_symbol () in
  let applied = f (evar ~loc element) in
  [%expr
    let rec map l acc = match l with
      | [] -> Ppx_enumerate_lib.List.rev acc
      | [%p pvar ~loc element] :: l ->
        map l ([%e applied] ::acc)
    in
    map [%e l] []
  ]

(* [cartesian_product_map l's f loc] takes a list of expressions of type list, and
   returns code generating the Cartesian product of those lists, with [f] applied to each
   tuple.
*)
let cartesian_product_map ~exhaust_check l's ~f loc =
  match l's with
  | [] -> Location.raise_errorf ~loc "cartesian_product_map passed list of zero length"
  | [l] -> list_map loc l ~f:(fun x -> f [x])
  | _ ->
    let lid x =  evar ~loc x in
    let patt_lid x = pvar ~loc x in
    let alias_vars = List.map l's ~f:(fun _ -> gen_symbol ()) in
    let init =
      let len = List.length l's in
      let hd_vars = List.map l's ~f:(fun _ -> gen_symbol ()) in
      let args_vars = List.map l's ~f:(fun _ -> gen_symbol ()) in
      let tl_var = gen_symbol () in
      let base_case =
        let patts =
          List.rev ([%pat? []] :: List.init (len - 1) ~f:(fun _ -> [%pat? _]))
        in
        case ~guard:None ~lhs:(patt_tuple loc patts) ~rhs:[%expr Ppx_enumerate_lib.List.rev acc]
      in
      let apply_case =
        let patts = List.mapi hd_vars ~f:(fun i x ->
          [%pat? ([%p pvar ~loc x] :: [%p if i = 0 then
                                            patt_lid tl_var
                                          else
                                            ppat_any ~loc])])
        in
        case ~guard:None ~lhs:(patt_tuple loc patts)
          ~rhs:(apply [%expr loop ([%e f (List.map hd_vars ~f:lid)] :: acc)]
                  (evar ~loc tl_var :: List.map (List.tl_exn args_vars) ~f:lid))
      in
      let decrement_cases =
        List.init (len - 1) ~f:(fun i ->
          let patts = List.init i ~f:(fun _ -> ppat_any ~loc)
                      @ [ [%pat? [] ]; [%pat?  (_ :: [%p pvar ~loc tl_var]) ] ]
                      @ List.init (len - i - 2) ~f:(fun _ -> ppat_any ~loc)
          in
          case ~guard:None ~lhs:(patt_tuple loc patts)
            ~rhs:(apply [%expr loop acc ]
                    (List.map ~f:lid (List.take alias_vars (i + 1))
                     @ evar ~loc tl_var ::
                       (List.map ~f:lid (List.drop args_vars (i + 2))))))
      in
      let decrement_cases =
        if exhaust_check then
          decrement_cases
        else
          decrement_cases @ [
            case ~guard:None ~lhs:(ppat_any ~loc) ~rhs:[%expr assert false ]
          ]
      in
      let match_exp =
        pexp_match ~loc (tuple loc (List.map args_vars ~f:lid))
           (base_case :: apply_case :: decrement_cases)
      in
      let match_exp =
        if exhaust_check then
          match_exp
        else
          let loc = Location.none in
          { match_exp with
            pexp_attributes = [
              attribute
                ~loc
                ~name:(Location.{ txt = "ocaml.warning"; loc })
                ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-11") [] ])
            ]
          }
      in
      [%expr
        let rec loop acc =
          [%e eabstract ~loc (List.map args_vars ~f:patt_lid) match_exp]
        in
        [%e apply [%expr loop []] (List.map ~f:lid alias_vars)]
      ]
    in
    Caml.ListLabels.fold_right2 alias_vars l's ~init ~f:(fun alias_var input_list acc ->
      [%expr
        let [%p pvar ~loc alias_var] = [%e input_list] in
        [%e acc]
      ])

(* Here we do two things: simplify append on static lists, to make the generated code more
   readable and rewrite (List.append (List.append a b) c) as (List.append a (List.append b
   c)), to avoid a quadratic behaviour with long nesting to the left. *)
let rec list_append loc l1 l2 =
  match l2 with
  | [%expr [] ] -> l1
  | _ ->
    match l1 with
    | [%expr [] ] -> l2
    | [%expr [%e? hd] :: [%e? tl] ] -> [%expr [%e hd] :: [%e list_append loc tl l2] ]
    | [%expr Ppx_enumerate_lib.List.append [%e? ll] [%e? lr] ] -> list_append loc ll (list_append loc lr l2)
    | _ ->
      [%expr  Ppx_enumerate_lib.List.append [%e l1] [%e l2] ]

let rec enum ~exhaust_check ~main_type ty =
  let loc = { ty.ptyp_loc with loc_ghost = true } in
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr [false; true] ]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr  [()] ]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [tp]) ->
    [%expr (None :: [%e list_map loc (enum ~exhaust_check ~main_type:tp tp)
                          ~f:(fun e -> [%expr Some [%e e]])]
           )
    ]
  | Ptyp_constr (id, args) ->
    type_constr_conv ~loc id ~f:name_of_type_name
      (List.map args ~f:(fun t -> enum ~exhaust_check t ~main_type:t))
  | Ptyp_tuple tps -> product ~exhaust_check loc tps (fun exprs -> tuple loc exprs)
  | Ptyp_variant (row_fields, Closed, None) ->
    List.fold_left row_fields ~init:[%expr []] ~f:(fun acc rf ->
      list_append loc acc (variant_case ~exhaust_check loc rf ~main_type))
  | Ptyp_var id -> evar ~loc (name_of_type_variable id)
  | _ -> Location.raise_errorf ~loc "ppx_enumerate: unsupported type"

and variant_case ~exhaust_check loc row_field ~main_type =
  match row_field.prf_desc with
  | Rtag ({ txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, _, []) ->
    [%expr [ [%e pexp_variant ~loc cnstr None] ] ]
  | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
    list_map loc (enum ~exhaust_check tp ~main_type:tp) ~f:(fun e ->
      pexp_variant ~loc cnstr (Some e))
  | Rinherit ty ->
    let e = enum ~exhaust_check ~main_type ty in
    [%expr ([%e e] :> [%t replace_variables_by_underscores main_type] list) ]

and constructor_case ~exhaust_check loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> [%expr [ [%e econstruct cd None ] ] ]
  | Pcstr_tuple tps ->
    product ~exhaust_check loc tps (fun x ->
      econstruct cd (Some (pexp_tuple ~loc x)))
  | Pcstr_record lds ->
    enum_of_lab_decs ~exhaust_check ~loc lds ~k:(fun x -> econstruct cd (Some x))

and enum_of_lab_decs ~exhaust_check ~loc lds ~k =
  let field_names, types =
    List.unzip (
      List.map lds ~f:(fun ld -> (ld.pld_name, ld.pld_type)))
  in
  product ~exhaust_check loc types (function l ->
    let fields =
      List.map2_exn field_names l ~f:(fun field_name x ->
        (Located.map lident field_name, x))
    in
    k (pexp_record ~loc fields None)
  )

and product ~exhaust_check loc tps f =
    let all = List.map tps ~f:(fun tp -> enum ~exhaust_check ~main_type:tp tp) in
    cartesian_product_map ~exhaust_check all loc ~f

let quantify loc tps typ =
  match tps with
  | [] -> typ
  | _  ->
    ptyp_poly ~loc (List.map tps ~f:(fun x -> (get_type_param_name x))) typ

let enum_of_td ~exhaust_check td =
  let td = name_type_params_in_td td in
  let loc = td.ptype_loc in
  let all =
    let main_type =
      ptyp_constr ~loc (Located.map lident td.ptype_name)
        (List.map td.ptype_params ~f:(fun _ -> ptyp_any ~loc))
    in
    match td.ptype_kind with
    | Ptype_variant cds ->
      (* Process [cd] elements in same order as camlp4 to avoid code-gen diffs caused by
         different order of [gen_symbol] calls *)
      List.fold_left cds ~init:[%expr []] ~f:(fun acc cd ->
        list_append loc acc (constructor_case ~exhaust_check loc cd))
    | Ptype_record lds -> enum_of_lab_decs ~exhaust_check ~loc lds ~k:(fun x -> x)
    | Ptype_open ->
      Location.raise_errorf ~loc "ppx_enumerate: open types not supported"
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> [%expr [] ]
      | Some tp -> enum ~exhaust_check tp ~main_type
  in
  let name = name_of_type_name td.ptype_name.txt in
  let args = List.map td.ptype_params ~f:(fun ((tp, _) as x) ->
    let name = name_of_type_variable (get_type_param_name x).txt in
    let loc = tp.ptyp_loc in
    pvar ~loc name
  )
  in
  let enumeration_type =
    let typ = enumeration_type_of_td td in
    quantify loc td.ptype_params typ
  in
  let body = eabstract ~loc args all in
  let zero_args = (List.length args = 0) in
  if zero_args (* constrain body rather than pattern *)
  then [%str let [%p pvar ~loc name] = ([%e body] : [%t enumeration_type]) ]
  else [%str let [%p pvar ~loc name] : [%t enumeration_type] = [%e body] ]


let enumerate =
  let str_args = Deriving.Args.(empty +> flag "no_exhaustiveness_check") in
  Deriving.add "enumerate"
    ~str_type_decl:(Deriving.Generator.make str_args
                      (fun ~loc ~path:_ (_rec, tds) no_exhaustiveness_check ->
                         match tds with
                         | [td] ->
                           enum_of_td ~exhaust_check:(not no_exhaustiveness_check) td
                         | _ -> Location.raise_errorf ~loc
                                  "only one type at a time is support by ppx_enumerate"))
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty sig_of_tds)

let () =
  Deriving.add "all"
    ~extension:(fun ~loc:_ ~path:_ ty -> enum ~exhaust_check:true ty ~main_type:ty)
  |> Deriving.ignore
