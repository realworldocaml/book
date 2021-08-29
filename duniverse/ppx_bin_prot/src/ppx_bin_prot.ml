(** Ppx_bin_prot: Preprocessing Module for a Type Safe Binary Protocol *)

open Base
open Ppxlib
open Ast_builder.Default

let ( @@ ) a b = a b

(* +-----------------------------------------------------------------+
   | Signature generators                                            |
   +-----------------------------------------------------------------+ *)

module Sig = struct
  let mk_sig_generator combinators =
    let mk_sig ~ctxt:_ (_rf, tds) =
      List.concat_map tds ~f:(fun td ->
        let td = name_type_params_in_td td in
        List.map combinators ~f:(fun mk -> mk td))
    in
    Deriving.Generator.V2.make Deriving.Args.empty mk_sig

  let mk_typ ?(wrap_result=fun ~loc:_ x -> x) type_constr td =
    let loc = td.ptype_loc in
    let id = Longident.parse type_constr in
    let wrap_type ~loc t =
      ptyp_constr ~loc (Located.mk ~loc id) [t]
    in
    let result_type =
      wrap_type ~loc:td.ptype_name.loc
        (wrap_result ~loc (core_type_of_type_declaration td))
    in
    List.fold_right
      td.ptype_params
      ~init:result_type
      ~f:(fun (tp, _variance) acc ->
        let loc = tp.ptyp_loc in
        ptyp_arrow ~loc Nolabel (wrap_type ~loc tp) acc)
  ;;

  let mk name_format type_constr ?wrap_result td =
    let loc = td.ptype_loc in
    let name = Loc.map ~f:(Printf.sprintf name_format) td.ptype_name in
    let typ = mk_typ ?wrap_result type_constr td in
    psig_value ~loc (value_description ~loc ~name ~type_:typ ~prim:[])

  let bin_write =
    mk_sig_generator
      [ mk "bin_size_%s"   "Bin_prot.Size.sizer"
      ; mk "bin_write_%s"  "Bin_prot.Write.writer"
      ; mk "bin_writer_%s" "Bin_prot.Type_class.writer"
      ]

  let bin_read =
    mk_sig_generator
      [ mk "bin_read_%s"     "Bin_prot.Read.reader"
      ; mk                   "__bin_read_%s__" "Bin_prot.Read.reader"
          ~wrap_result:(fun ~loc t -> [%type: int -> [%t t]])
      ; mk "bin_reader_%s"   "Bin_prot.Type_class.reader"
      ]

  let bin_type_class =
    mk_sig_generator
      [ mk "bin_%s" "Bin_prot.Type_class.t" ]

  let named =
    let mk_named_sig ~ctxt (rf, tds) =
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      match
        mk_named_sig ~loc ~sg_name:"Bin_prot.Binable.S"
              ~handle_polymorphic_variant:true tds
      with
      | Some incl -> [psig_include ~loc incl]
      | None ->
        List.concat_map
          [ Bin_shape_expand.sig_gen; bin_write; bin_read; bin_type_class ]
          ~f:(fun gen -> Deriving.Generator.apply ~name:"unused" gen ~ctxt (rf, tds) [])
    in
    Deriving.Generator.V2.make Deriving.Args.empty mk_named_sig
end

(* +-----------------------------------------------------------------+
   | Utility functions                                               |
   +-----------------------------------------------------------------+ *)

let atoms_in_row_fields row_fields =
  List.exists row_fields ~f:(fun row_field ->
    match row_field.prf_desc with
    | Rtag (_, is_constant, _) -> is_constant
    | Rinherit _ -> false)
;;

let atoms_in_variant cds =
  List.filter cds ~f:(fun cds ->
    match cds.pcd_args with
    | Pcstr_tuple [] -> true
    | Pcstr_tuple _ -> false
    | Pcstr_record _ -> false)

let let_ins loc bindings expr =
  List.fold_right bindings ~init:expr ~f:(fun binding expr ->
    pexp_let ~loc Nonrecursive [binding] expr)

let alias_or_fun expr fct =
  let is_id =
    match expr.pexp_desc with
    | Pexp_ident _ -> true
    | _ -> false
  in
  if is_id then expr else fct
;;

let td_is_nil td =
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_abstract, None -> true
  | _ -> false

type var = string Located.t
let vars_of_params ~prefix td =
  List.map td.ptype_params ~f:(fun tp ->
    let name = get_type_param_name tp in
    { name with txt = prefix ^ name.txt })
let map_vars vars ~f = List.map vars ~f:(fun (v : var) -> f ~loc:v.loc v.txt)
let patts_of_vars = map_vars ~f:pvar
let exprs_of_vars = map_vars ~f:evar

let project_vars expr vars ~field_name =
  let field : Longident.t = Ldot (Ldot (Lident "Bin_prot", "Type_class"), field_name) in
  let args =
    map_vars vars ~f:(fun ~loc txt ->
      pexp_field ~loc (evar ~loc txt) (Located.mk ~loc field))
  in
  let loc = expr.pexp_loc in
  eapply ~loc expr args

module Full_type_name : sig
  type t
  val make : path:string -> type_declaration -> t
  val absent : t
  val get : t -> string option
  val get_exn : loc:Location.t -> t -> string
end = struct
  type t = string option

  let make ~path td =
    Some (Printf.sprintf "%s.%s" path td.ptype_name.txt)

  let absent = None

  let get t = t

  let get_exn ~loc t =
    match t with
    | Some n -> n
    | None ->
      Location.raise_errorf ~loc
        "Bug in ppx_bin_prot: full type name needed but not in a type declaration.\n\
         Callstack:\n\
         %s"
        (Caml.Printexc.get_callstack 256 |> Caml.Printexc.raw_backtrace_to_string)
end


let generate_poly_type ?wrap_result ~loc td constructor =
  ptyp_poly ~loc
    (List.map td.ptype_params ~f:get_type_param_name)
    (Sig.mk_typ ?wrap_result constructor td)

(* Determines whether or not the generated code associated with
   a type declaration should include explicit type signatures.

   In particular, we'd rather not add an explicit type signature when
   polymorphic variants are involved.
   As discussed in https://github.com/janestreet/ppx_bin_prot/pull/7

   However, if we have mutually recursive type declarations involving polymorphic type
   constructors where we add a type declaration to one of them, we need it on all of them,
   otherwise we'll generate ill-typed code. *)
let would_rather_omit_type_signatures =
  let module M = struct exception E end in
  let has_variant =
    object
      inherit Ast_traverse.iter as super
      method! core_type ct =
        match ct.ptyp_desc with
        | Ptyp_variant _ -> Exn.raise_without_backtrace M.E
        | _ -> super#core_type ct
    end
  in
  fun td ->
    match td.ptype_kind with
    | Ptype_variant _ | Ptype_record _
    | Ptype_open -> false
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> false
      | Some body -> try has_variant#core_type body; false with M.E -> true

(* +-----------------------------------------------------------------+
   | Generator for size computation of OCaml-values for bin_prot     |
   +-----------------------------------------------------------------+ *)

module Generate_bin_size = struct
  let mk_abst_call ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "bin_size_" ^ s) args

  (* Conversion of types *)
  let rec bin_size_type full_type_name _loc ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match ty.ptyp_desc with
    | Ptyp_constr (id, args) ->
      `Fun (bin_size_appl_fun full_type_name loc id args)
    | Ptyp_tuple l -> bin_size_tuple full_type_name loc l
    | Ptyp_var parm -> `Fun (evar ~loc @@ "_size_of_" ^ parm)
    | Ptyp_arrow _ ->
      Location.raise_errorf ~loc
        "bin_size_type: cannot convert functions to the binary protocol"
    | Ptyp_variant (row_fields, _, _) -> bin_size_variant full_type_name loc row_fields
    | Ptyp_poly (parms, ty) -> bin_size_poly full_type_name loc parms ty
    | _ ->
      Location.raise_errorf ~loc "bin_size_type: unknown type construct"

  (* Conversion of polymorphic types *)
  and bin_size_appl_fun full_type_name loc id args =
    let loc = { loc with loc_ghost = true } in
    let sizers =
      List.map args ~f:(fun ty ->
        match bin_size_type full_type_name ty.ptyp_loc ty with
        | `Fun e -> e
        | `Match cases ->
          pexp_function ~loc:{ ty.ptyp_loc with loc_ghost = true } cases)
    in
    match mk_abst_call ~loc id sizers with
    | [%expr Bin_prot.Size.bin_size_array Bin_prot.Size.bin_size_float ] ->
      [%expr Bin_prot.Size.bin_size_float_array ]
    | e -> e

  (* Conversion of tuples and records *)

  and bin_size_args
    : 'a 'b. Full_type_name.t
    -> Location.t
    -> ('a -> core_type)
    -> (Location.t -> string -> 'a -> 'b)
    -> 'a list
    -> 'b list * expression
    = fun full_type_name loc get_tp mk_patt tps ->
    let rec loop i = function
      | el :: rest ->
        let tp = get_tp el in
        let v_name = "v" ^ Int.to_string i in
        let v_expr =
          let e_name = evar ~loc v_name in
          let expr =
            match bin_size_type full_type_name loc tp with
            | `Fun fun_expr -> eapply ~loc fun_expr [e_name]
            | `Match cases  -> pexp_match ~loc e_name cases
          in
          [%expr Bin_prot.Common.(+) size [%e expr] ]
        in
        let patt = mk_patt loc v_name el in
        if List.is_empty rest then [patt], v_expr
        else
          let patts, in_expr = loop (i + 1) rest in
            patt :: patts, [%expr  let size = [%e v_expr] in [%e in_expr] ]
      | [] -> assert false  (* impossible *)
    in
    loop 1 tps

  and bin_size_tup_rec
    : 'a 'b. Full_type_name.t
    -> Location.t
    -> ('b list -> pattern)
    -> ('a -> core_type)
    -> (Location.t -> string -> 'a -> 'b)
    -> 'a list
    -> _
    = fun full_type_name loc cnv_patts get_tp mk_patt tp ->
    let patts, expr = bin_size_args full_type_name loc get_tp mk_patt tp in
    `Match [ case ~lhs:(cnv_patts patts) ~guard:None
               ~rhs:[%expr let size = 0 in [%e expr] ] ]

  (* Conversion of tuples *)
  and bin_size_tuple full_type_name loc l =
    let cnv_patts patts = ppat_tuple ~loc patts in
    let get_tp tp = tp in
    let mk_patt loc v_name _ = pvar ~loc v_name in
    bin_size_tup_rec full_type_name loc cnv_patts get_tp mk_patt l

  (* Conversion of records *)
  and bin_size_record full_type_name loc tp =
    let cnv_patts lbls = ppat_record ~loc lbls Closed in
    let get_tp ld = ld.pld_type in
    let mk_patt loc v_name ld =
      (Located.map lident ld.pld_name, pvar ~loc v_name)
    in
    bin_size_tup_rec full_type_name loc cnv_patts get_tp mk_patt tp

  (* Conversion of variant types *)
  and bin_size_variant full_type_name loc row_fields =
    let nonatom_matchings =
      List.fold_left row_fields ~init:[] ~f:(fun acc rf ->
        match rf.prf_desc with
        | Rtag (_, true, _) -> acc
        | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
          let size_args =
            match bin_size_type full_type_name tp.ptyp_loc tp with
            | `Fun fun_expr -> eapply ~loc fun_expr [ [%expr args ] ]
            | `Match cases  -> pexp_match ~loc [%expr args] cases
          in
          case
            ~lhs:(ppat_variant cnstr ~loc (Some [%pat? args]))
            ~guard:None
            ~rhs:[%expr
              let size_args = [%e size_args] in
              Bin_prot.Common.(+) size_args 4
            ]
          :: acc
        | Rtag (_, false, []) -> acc (* Impossible, let the OCaml compiler fail *)
        | Rinherit ty ->
          let loc = { ty.ptyp_loc with loc_ghost = true } in
          match ty.ptyp_desc with
          | Ptyp_constr (id, args) ->
            let call = bin_size_appl_fun full_type_name loc id args in
            case
              ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc "v"))
              ~guard:None
              ~rhs:(eapply ~loc call [ [%expr v] ])
            :: acc
          | _ ->
            Location.raise_errorf ~loc "bin_size_variant: unknown type"
      )
    in
    let matchings =
      if atoms_in_row_fields row_fields then
        case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr 4] :: nonatom_matchings
      else
        nonatom_matchings
    in
    `Match (List.rev matchings)

  (* Polymorphic record fields *)
  and bin_size_poly full_type_name loc parms tp =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding ~loc ~pat:(pvar ~loc @@ "_size_of_" ^ parm.txt)
          ~expr:[%expr fun _v ->
               raise (Bin_prot.Common.Poly_rec_write [%e estring ~loc full_type_name])
          ]
      in
      List.map parms ~f:mk_binding
    in
    match bin_size_type full_type_name loc tp with
    | `Fun fun_expr -> `Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | `Match matchings ->
        `Match
          [ case ~lhs:(pvar ~loc "arg") ~guard:None
              ~rhs:(pexp_let ~loc Nonrecursive bindings
                      (pexp_match ~loc (evar ~loc "arg") matchings))
          ]

  (* Conversion of sum types *)

  let bin_size_sum full_type_name loc alts =
    let n_alts = List.length alts in
    let size_tag =
      if n_alts <= 256 then
        [%expr  1 ]
      else if n_alts <= 65536 then
        [%expr  2 ]
      else
        Location.raise_errorf ~loc
          "bin_size_sum: too many alternatives (%d > 65536)" n_alts
    in
    let nonatom_matchings =
      List.fold_left alts ~init:[] ~f:(fun acc cd ->
        (match cd.pcd_res with
         | None -> ()
         | Some ty ->
           Location.raise_errorf ~loc:ty.ptyp_loc
             "bin_size_sum: GADTs are not supported by bin_prot");
        match cd.pcd_args with
        | Pcstr_tuple [] -> acc
        | Pcstr_tuple args ->
          let get_tp tp = tp in
          let mk_patt loc v_name _ = pvar ~loc v_name in
          let patts, size_args =
            bin_size_args full_type_name loc get_tp mk_patt args
          in
          let args =
            match patts with
            | [patt] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          case ~lhs:(pconstruct cd (Some args)) ~guard:None
            ~rhs:[%expr
              let size = [%e size_tag] in
              [%e size_args]
            ]
          :: acc
        | Pcstr_record fields ->
          let cnv_patts lbls = ppat_record ~loc lbls Closed in
          let get_tp ld = ld.pld_type in
          let mk_patt loc v_name ld =
            Located.map lident ld.pld_name, pvar ~loc v_name
          in
          let patts, size_args =
            bin_size_args full_type_name loc get_tp mk_patt fields
          in
          case
            ~lhs:(pconstruct cd (Some (cnv_patts patts)))
            ~guard:None
            ~rhs:[%expr
              let size = [%e size_tag] in
              [%e size_args]
            ]
          :: acc
      )
    in
    let atom_matching init atoms =
      List.fold_left atoms
        ~init:(pconstruct init None)
        ~f:(fun acc atom -> ppat_or ~loc acc (pconstruct atom None))
    in
    let matchings =
      match atoms_in_variant alts with
      | [] ->
        nonatom_matchings
      | init :: atoms ->
        case ~lhs:(atom_matching init atoms) ~guard:None ~rhs:size_tag :: nonatom_matchings
    in
    `Match (List.rev matchings)

  (* Empty types *)
  let bin_size_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Fun [%expr fun _v ->
         raise (Bin_prot.Common.Empty_type [%e estring ~loc full_type_name]) ]

  let make_fun ~loc ?(don't_expand=false) fun_or_match =
    match fun_or_match with
    | `Fun fun_expr when don't_expand -> fun_expr
    | `Fun fun_expr -> alias_or_fun fun_expr [%expr fun v -> [%e eapply ~loc fun_expr [[%expr v]]]]
    | `Match matchings -> pexp_function ~loc matchings

  let sizer_body_of_td ~path td =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let res =
      match td.ptype_kind with
      | Ptype_variant alts -> bin_size_sum    full_type_name loc alts
      | Ptype_record  flds -> bin_size_record full_type_name loc flds
      | Ptype_open ->
        Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
      | Ptype_abstract ->
        match td.ptype_manifest with
        | None    -> bin_size_nil  full_type_name loc
        | Some ty -> bin_size_type full_type_name loc ty
    in
    make_fun ~loc ~don't_expand:(td_is_nil td) res

  (* Generate code from type definitions *)
  let bin_size_td ~can_omit_type_annot ~loc ~path td =
    let body = sizer_body_of_td ~path td in
    let tparam_patts = vars_of_params td ~prefix:"_size_of_" |> patts_of_vars in
    let pat = pvar ~loc @@ "bin_size_" ^ td.ptype_name.txt in
    let pat_with_type =
      if can_omit_type_annot then pat
      else
        ppat_constraint ~loc pat
          (generate_poly_type ~loc td "Bin_prot.Size.sizer")
    in
    value_binding ~loc
      ~pat:pat_with_type
      ~expr:(eabstract ~loc tparam_patts body)

  let bin_size ~loc ~path (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag = really_recursive rec_flag tds in
    let can_omit_type_annot = List.for_all ~f:would_rather_omit_type_signatures tds in
    let bindings = List.map tds ~f:(bin_size_td ~can_omit_type_annot ~loc ~path) in
    pstr_value ~loc rec_flag bindings
end

(* +-----------------------------------------------------------------+
   | Generator for converters of OCaml-values to the binary protocol |
   +-----------------------------------------------------------------+ *)

module Generate_bin_write = struct
  let mk_abst_call ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "bin_write_" ^ s) args

  (* Conversion of types *)
  let rec bin_write_type full_type_name _loc ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match ty.ptyp_desc with
    | Ptyp_constr (id, args) ->
      `Fun (bin_write_appl_fun full_type_name loc id args)
    | Ptyp_tuple l -> bin_write_tuple full_type_name loc l
    | Ptyp_var parm -> `Fun (evar ~loc @@ "_write_" ^ parm)
    | Ptyp_arrow _ ->
      Location.raise_errorf ~loc
        "bin_write_type: cannot convert functions to the binary protocol"
    | Ptyp_variant (row_fields, _, _) -> bin_write_variant full_type_name loc row_fields
    | Ptyp_poly (parms, ty) -> bin_write_poly full_type_name loc parms ty
    | _ ->
      Location.raise_errorf ~loc "bin_write_type: unknown type construct"

  (* Conversion of polymorphic types *)
  and bin_write_appl_fun full_type_name loc id args =
    let loc = { loc with loc_ghost = true } in
    let writers =
      List.map args ~f:(fun ty ->
        match bin_write_type full_type_name ty.ptyp_loc ty with
        | `Fun e -> e
        | `Match cases ->
          [%expr fun buf ~pos -> [%e pexp_function ~loc:ty.ptyp_loc cases ] ]
      )
    in
    let e =
      match mk_abst_call ~loc id writers with
      | [%expr Bin_prot.Write.bin_write_array Bin_prot.Write.bin_write_float ] ->
        [%expr Bin_prot.Write.bin_write_float_array ]
      | e -> e
    in
    e

  (* Conversion of tuples and records *)

  and bin_write_args
    : 'a 'b. Full_type_name.t
    -> Location.t
    -> ('a -> core_type)
    -> (Location.t -> string -> 'a -> 'b)
    -> 'a list
    -> 'b list * expression
    = fun full_type_name loc get_tp mk_patt tp ->
    let rec loop i = function
      | el :: rest ->
          let tp = get_tp el in
          let v_name = "v" ^ Int.to_string i in
          let v_expr =
            let e_name = evar ~loc v_name in
            match bin_write_type full_type_name loc tp with
            | `Fun fun_expr -> [%expr [%e fun_expr] buf ~pos [%e e_name] ]
            | `Match cases  -> pexp_match ~loc e_name cases
          in
          let patt = mk_patt loc v_name el in
          if List.is_empty rest then [patt], v_expr
          else
            let patts, in_expr = loop (i + 1) rest in
            patt :: patts, [%expr  let pos = [%e v_expr] in [%e in_expr] ]
      | [] -> assert false  (* impossible *)
    in
    loop 1 tp

  and bin_write_tup_rec
    : 'a 'b. Full_type_name.t
    -> Location.t
    -> ('b list -> pattern)
    -> ('a -> core_type)
    -> (Location.t -> string -> 'a -> 'b)
    -> 'a list
    -> _
    = fun full_type_name loc cnv_patts get_tp mk_patt tp ->
    let patts, expr = bin_write_args full_type_name loc get_tp mk_patt tp in
    `Match [ case ~lhs:(cnv_patts patts) ~guard:None ~rhs:expr ]

  (* Conversion of tuples *)
  and bin_write_tuple full_type_name loc l =
    let cnv_patts patts = ppat_tuple ~loc patts in
    let get_tp tp = tp in
    let mk_patt loc v_name _ = pvar ~loc v_name in
    bin_write_tup_rec full_type_name loc cnv_patts get_tp mk_patt l

  (* Conversion of records *)
  and bin_write_record full_type_name loc tp =
    let cnv_patts lbls = ppat_record ~loc lbls Closed in
    let get_tp ld = ld.pld_type in
    let mk_patt loc v_name ld =
      (Located.map lident ld.pld_name, pvar ~loc v_name)
    in
    bin_write_tup_rec full_type_name loc cnv_patts get_tp mk_patt tp

  (* Conversion of variant types *)
  and bin_write_variant full_type_name loc row_fields =
    let matchings =
      List.map row_fields ~f:(fun row_field ->
        match row_field.prf_desc with
        | Rtag ({txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, false, []) ->
          case ~lhs:(ppat_variant  ~loc cnstr None) ~guard:None
            ~rhs:[%expr
              Bin_prot.Write.bin_write_variant_int buf ~pos
                [%e eint ~loc (Ocaml_common.Btype.hash_variant cnstr) ]
            ]
        | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
          let write_args =
            match bin_write_type full_type_name tp.ptyp_loc tp with
            | `Fun fun_expr -> [%expr [%e fun_expr] buf ~pos args ]
            | `Match cases  -> pexp_match ~loc [%expr args] cases
          in
          case
            ~lhs:(ppat_variant ~loc cnstr (Some [%pat? args]))
            ~guard:None
            ~rhs:[%expr
              let pos =
                Bin_prot.Write.bin_write_variant_int buf ~pos
                  [%e eint ~loc (Ocaml_common.Btype.hash_variant cnstr)]
              in
              [%e write_args]
            ]
        | Rinherit ty ->
          let loc = { ty.ptyp_loc with loc_ghost = true } in
          match ty.ptyp_desc with
          | Ptyp_constr (id, args) ->
            let call = bin_write_appl_fun full_type_name loc id args in
            case
              ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc "v"))
              ~guard:None
              ~rhs:[%expr [%e call] buf ~pos v]
          | _ ->
            Location.raise_errorf ~loc "bin_write_variant: unknown type"
      )
    in
    `Match matchings

  (* Polymorphic record fields *)
  and bin_write_poly full_type_name loc parms tp =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding ~loc ~pat:(pvar ~loc @@ "_write_" ^ parm.txt)
          ~expr:[%expr fun _buf ~pos:_ _v ->
               raise (Bin_prot.Common.Poly_rec_write [%e estring ~loc full_type_name])
          ]
      in
      List.map parms ~f:mk_binding
    in
    match bin_write_type full_type_name loc tp with
    | `Fun fun_expr -> `Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | `Match matchings ->
        `Match
          [ case ~lhs:(pvar ~loc "arg") ~guard:None
              ~rhs:(pexp_let ~loc Nonrecursive bindings
                      (pexp_match ~loc (evar ~loc "arg") matchings))
          ]

  (* Conversion of sum types *)

  let bin_write_sum full_type_name loc alts =
    let n_alts = List.length alts in
    let write_tag =
      if n_alts <= 256 then
        [%expr Bin_prot.Write.bin_write_int_8bit buf ~pos ]
      else if n_alts <= 65536 then
        [%expr Bin_prot.Write.bin_write_int_16bit buf ~pos ]
      else
        Location.raise_errorf ~loc
          "bin_write_sum: too many alternatives (%d > 65536)" n_alts
    in
    let matchings =
      List.mapi alts ~f:(fun i cd ->
        (match cd.pcd_res with
         | None -> ()
         | Some ty ->
           Location.raise_errorf ~loc:ty.ptyp_loc
             "bin_write_sum: GADTs are not supported by bin_prot");
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let loc = cd.pcd_loc in
          case
            ~lhs:(pconstruct cd None)
            ~guard:None
            ~rhs:(eapply ~loc write_tag [eint ~loc i])
        | Pcstr_tuple args ->
          let get_tp tp = tp in
          let mk_patt loc v_name _ = pvar ~loc v_name in
          let patts, write_args =
            bin_write_args full_type_name loc get_tp mk_patt args
          in
          let args =
            match patts with
            | [patt] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          case ~lhs:(pconstruct cd (Some args)) ~guard:None
            ~rhs:[%expr
              let pos = [%e eapply ~loc write_tag [eint ~loc i]] in
              [%e write_args]
            ]
        | Pcstr_record fields ->
          let cnv_patts lbls = ppat_record ~loc lbls Closed in
          let get_tp ld = ld.pld_type in
          let mk_patt loc v_name ld =
            Located.map lident ld.pld_name, pvar ~loc v_name
          in
          let patts, expr =
            bin_write_args full_type_name loc get_tp mk_patt fields in
          case
            ~lhs:(pconstruct cd (Some (cnv_patts patts)))
            ~guard:None
            ~rhs:[%expr
              let pos = [%e eapply ~loc write_tag [eint ~loc i]] in
              [%e expr]
            ]
      )
    in
    `Match matchings

  (* Empty types *)
  let bin_write_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Fun [%expr fun _buf ~pos:_ _v ->
         raise (Bin_prot.Common.Empty_type [%e estring ~loc full_type_name]) ]

  let make_fun ~loc ?(don't_expand=false) fun_or_match =
    match fun_or_match with
    | `Fun fun_expr when don't_expand -> fun_expr
    | `Fun fun_expr -> alias_or_fun fun_expr
                         [%expr fun buf ~pos v -> [%e fun_expr] buf ~pos v ]
    | `Match matchings -> [%expr fun buf ~pos -> [%e pexp_function ~loc matchings ] ]

  let writer_type_class_record ~loc ~size ~write =
    [%expr
      { Bin_prot.Type_class.
        size  = [%e size]
      ; write = [%e write]
      }
    ]

  let writer_body_of_td ~path td =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let res =
      match td.ptype_kind with
      | Ptype_variant alts -> bin_write_sum    full_type_name loc alts
      | Ptype_record  flds -> bin_write_record full_type_name loc flds
      | Ptype_open ->
        Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
      | Ptype_abstract ->
        match td.ptype_manifest with
        | None    -> bin_write_nil  full_type_name loc
        | Some ty -> bin_write_type full_type_name loc ty
    in
    make_fun ~loc ~don't_expand:(td_is_nil td) res

  let project_vars expr vars ~field_name =
    let call = project_vars expr vars ~field_name in
    let loc = call.pexp_loc in
    alias_or_fun call [%expr fun v -> [%e eapply ~loc call [ [%expr v] ]]]

  (* Generate code from type definitions *)
  let bin_write_td ~can_omit_type_annot ~loc ~path td =
    let body = writer_body_of_td ~path td in
    let size_name  = "bin_size_"  ^ td.ptype_name.txt in
    let write_name = "bin_write_" ^ td.ptype_name.txt in

    let write_binding =
      let tparam_patts = vars_of_params td ~prefix:"_write_" |> patts_of_vars in
      let pat = pvar ~loc write_name in
      let pat_with_type =
        if can_omit_type_annot then pat
        else
          ppat_constraint ~loc pat
            (generate_poly_type ~loc td "Bin_prot.Write.writer")
      in
      value_binding ~loc ~pat:pat_with_type
        ~expr:(eabstract ~loc tparam_patts body)
    in
    let writer_binding =
      let vars = vars_of_params td ~prefix:"bin_writer_" in
      let writer_record =
        writer_type_class_record ~loc
          ~size:  (project_vars (evar ~loc size_name ) vars ~field_name:"size" )
          ~write: (project_vars (evar ~loc write_name) vars ~field_name:"write")
      in
      value_binding ~loc
        ~pat:(pvar ~loc @@ "bin_writer_" ^ td.ptype_name.txt)
        ~expr:(eabstract ~loc (patts_of_vars vars) writer_record)
    in
    (write_binding, writer_binding)

  let bin_write ~loc ~path (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag = really_recursive rec_flag tds in
    let can_omit_type_annot = List.for_all tds ~f:would_rather_omit_type_signatures in
    let write_bindings, writer_bindings =
      List.map tds ~f:(bin_write_td ~can_omit_type_annot ~loc ~path)
      |> List.unzip
    in
    Generate_bin_size.bin_size ~loc ~path (rec_flag, tds)
    :: [ pstr_value ~loc rec_flag write_bindings
       ; pstr_value ~loc Nonrecursive writer_bindings
       ]
  ;;

  let gen = Deriving.Generator.make Deriving.Args.empty bin_write

  let extension ~loc ~path:_ ty =
    let loc = { loc with loc_ghost = true } in
    let full_type_name = Full_type_name.absent in
    let size =
      Generate_bin_size.bin_size_type full_type_name loc ty
      |> Generate_bin_size.make_fun ~loc
    in
    let write = bin_write_type full_type_name loc ty |> make_fun ~loc in
    writer_type_class_record ~loc ~size ~write
  ;;
end

(* +-----------------------------------------------------------------+
   | Generator for converters of binary protocol to OCaml-values     |
   +-----------------------------------------------------------------+ *)

module Generate_bin_read = struct

  let full_type_name_or_anonymous full_type_name =
    match Full_type_name.get full_type_name with
    | None -> "<anonymous type>"
    | Some s -> s

  let mk_abst_call loc ?(internal = false) id args =
    type_constr_conv ~loc id args
      ~f:(fun s -> let s = "bin_read_" ^ s in
                   if internal then "__" ^ s ^ "__" else s)

  (* Conversion of type paths *)
  let bin_read_path_fun loc id args =
    mk_abst_call { loc with loc_ghost = true } id args

  let get_closed_expr loc = function
    | `Open   expr -> [%expr  fun buf ~pos_ref -> [%e expr] ]
    | `Closed expr -> expr

  let get_open_expr loc = function
    | `Open   expr -> expr
    | `Closed expr -> [%expr  [%e expr] buf ~pos_ref ]

  (* Conversion of arguments *)
  let rec handle_arg_tp loc full_type_name arg_tp =
    let args, bindings =
      let arg_map ai tp =
        let f =
          get_open_expr loc (bin_read_type full_type_name loc tp)
        in
        let arg_name = "arg_" ^ Int.to_string (ai + 1) in
        (evar ~loc arg_name, value_binding ~loc ~pat:(pvar ~loc arg_name) ~expr:f)
      in
      List.mapi arg_tp ~f:arg_map |> List.unzip
    in
    let args_expr =
      match args with
      | [expr] -> expr
      | _ -> pexp_tuple ~loc args
    in
    bindings, args_expr

  (* Conversion of types *)
  and bin_read_type_internal full_type_name ~full_type _loc ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match ty.ptyp_desc with
    | Ptyp_constr (id, args) ->
      let args_expr =
        List.map args ~f:(fun tp ->
          get_closed_expr _loc (bin_read_type full_type_name _loc tp))
      in
      let expr =
        match bin_read_path_fun id.loc id args_expr with
        | [%expr Bin_prot.Read.bin_read_array Bin_prot.Read.bin_read_float ] ->
          [%expr  Bin_prot.Read.bin_read_float_array ]
        | expr -> expr
      in
      `Closed expr
    | Ptyp_tuple tp -> bin_read_tuple full_type_name loc tp
    | Ptyp_var parm -> `Closed (evar ~loc ("_of__" ^ parm))
    | Ptyp_arrow _ ->
      Location.raise_errorf ~loc "bin_read_arrow: cannot convert functions"
    | Ptyp_variant (row_fields, _, _) ->
      bin_read_variant full_type_name loc ?full_type row_fields
    | Ptyp_poly (parms, poly_tp) ->
      bin_read_poly full_type_name loc parms poly_tp
    | _ ->
      Location.raise_errorf ~loc
        "bin_read_type: unknown type construct"

  and bin_read_type full_type_name loc ty =
    bin_read_type_internal full_type_name ~full_type:None loc ty

  and bin_read_type_toplevel full_type_name ~full_type loc ty =
    bin_read_type_internal full_type_name ~full_type:(Some full_type) loc ty

  (* Conversion of tuples *)
  and bin_read_tuple full_type_name loc tps =
    let bindings, exprs =
      let map i tp =
        let v_name = "v" ^ Int.to_string (i + 1) in
        let expr =
          get_open_expr loc (bin_read_type full_type_name loc tp)
        in
        (value_binding ~loc ~pat:(pvar ~loc v_name) ~expr, evar ~loc v_name)
      in
      List.mapi tps ~f:map |> List.unzip
    in
    `Open (let_ins loc bindings (pexp_tuple ~loc exprs))

  (* Variant conversions *)

  (* Generate internal call *)
  and mk_internal_call full_type_name loc ty =
    let loc = { loc with loc_ghost = true } in
    match ty.ptyp_desc with
    | Ptyp_constr (id, args) | Ptyp_class (id, args) ->
      let arg_exprs =
        List.map args ~f:(fun tp ->
          get_closed_expr loc (bin_read_type full_type_name loc tp))
      in
      mk_abst_call loc ~internal:true id arg_exprs
    | _ ->
      Location.raise_errorf ~loc:ty.ptyp_loc "bin_read: unknown type"

  (* Generate matching code for variants *)
  and bin_read_variant full_type_name loc ?full_type row_fields =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let code =
      let mk_check_vint mcs =
        pexp_match ~loc (evar ~loc "vint") mcs
      in
      let mk_try_next_expr call next_expr =
        [%expr
          try
            [%e call]
          with Bin_prot.Common.No_variant_match ->
            [%e next_expr]
        ]
      in
      let raise_nvm = [%expr raise Bin_prot.Common.No_variant_match ] in
      let rec loop_many next = function
        | h :: t -> loop_one next t h
        | [] ->
          match next with
          | `Matches mcs -> mk_check_vint mcs
          | `Expr expr -> expr
          | `None -> raise_nvm
      and loop_one next t = fun row_field ->
        match row_field.prf_desc with
        | Rtag ({ txt = cnstr; _ }, is_constant, tps) ->
          let rhs =
            match is_constant, tps with
            | false, arg_tp :: _ ->
              let bnds, args_expr =
                handle_arg_tp loc full_type_name
                  [ arg_tp ]
              in
              let_ins loc bnds (pexp_variant ~loc cnstr (Some args_expr))
            | _ ->
              pexp_variant ~loc cnstr None
          in
          let this_mc =
            case ~lhs:(pint ~loc (Ocaml_common.Btype.hash_variant cnstr))
              ~guard:None ~rhs
          in
          add_mc next this_mc t
        | Rinherit ty ->
          let call =
            [%expr
              (
                [%e mk_internal_call full_type_name ty.ptyp_loc ty] buf ~pos_ref vint
                :> [%t full_type]
              )
            ]
          in
          let expr =
            match next with
            | `Matches mcs -> mk_try_next_expr call (mk_check_vint mcs)
            | `Expr expr -> mk_try_next_expr call expr
            | `None -> call
          in
          loop_many (`Expr expr) t
      and add_mc next this_mc t =
        let next_mcs =
          match next with
          | `Matches mcs -> mcs
          | `Expr expr -> [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:expr ]
          | `None -> [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:raise_nvm ]
        in
        loop_many (`Matches (this_mc :: next_mcs)) t
      in
      loop_many `None (List.rev row_fields)
    in
    if is_contained then
      let full_type_name = full_type_name_or_anonymous full_type_name in
      `Open
        [%expr
          let vint =
            Bin_prot.Read.bin_read_variant_int buf ~pos_ref
          in
          try
            [%e code]
          with Bin_prot.Common.No_variant_match ->
            Bin_prot.Common.raise_variant_wrong_type [%e estring ~loc full_type_name]
              !pos_ref
        ]
    else `Open code

  (* Polymorphic record field conversion *)
  and bin_read_poly full_type_name loc parms tp =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding ~loc
          ~pat:(pvar ~loc @@ "_of__" ^ parm.txt)
          ~expr:[%expr
            fun _buf ~pos_ref ->
              Bin_prot.Common.raise_read_error
                (Bin_prot.Common.ReadError.Poly_rec_bound
                   [%e estring ~loc full_type_name]) !pos_ref
          ]
      in
      List.map parms ~f:mk_binding
    in
    let f = get_open_expr loc (bin_read_type full_type_name loc tp) in
    `Open (pexp_let ~loc Nonrecursive bindings f)

  (* Record conversions *)
  let bin_read_label_declaration_list full_type_name loc fields wrap =
    let bindings, rec_bindings =
      let map field =
        let loc = field.pld_loc in
        let v_name = "v_" ^ field.pld_name.txt in
        let f = get_open_expr loc (bin_read_type full_type_name loc field.pld_type) in
        (
          value_binding ~loc ~pat:(pvar ~loc:field.pld_name.loc v_name) ~expr:f,
          (Located.map lident field.pld_name, evar ~loc:field.pld_name.loc v_name)
        )
      in
      List.map fields ~f:map |> List.unzip
    in
    let_ins loc bindings (wrap (pexp_record ~loc rec_bindings None))

  (* Sum type conversions *)
  let bin_read_sum full_type_name loc alts =
    let map mi cd =
      (match cd.pcd_res with
       | None -> ()
       | Some _ ->
         Location.raise_errorf ~loc:cd.pcd_loc
           "bin_read_sum: GADTs are not supported by bin_prot");
      match cd.pcd_args with
      | Pcstr_tuple [] ->
        let loc = cd.pcd_loc in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs:(econstruct cd None)
      | Pcstr_tuple args ->
        let bindings, args_expr = handle_arg_tp loc full_type_name args in
        let rhs = let_ins loc bindings (econstruct cd (Some args_expr)) in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs
      | Pcstr_record fields ->
        let rhs =
          bin_read_label_declaration_list full_type_name loc fields
            (fun e -> econstruct cd (Some e))
        in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs
    in
    let mcs = List.mapi alts ~f:map in
    let n_alts = List.length alts in
    let read_fun =
      if n_alts <= 256 then
        [%expr  Bin_prot.Read.bin_read_int_8bit ]
      else if n_alts <= 65536 then
        [%expr  Bin_prot.Read.bin_read_int_16bit ]
      else
        Location.raise_errorf ~loc
          "bin_size_sum: too many alternatives (%d > 65536)" n_alts
    in
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Open
      (pexp_match ~loc [%expr [%e read_fun] buf ~pos_ref ]
         (mcs @
          [ case ~lhs:(ppat_any ~loc) ~guard:None
              ~rhs:[%expr
                Bin_prot.Common.raise_read_error
                  (Bin_prot.Common.ReadError.Sum_tag [%e estring ~loc full_type_name])
                  !pos_ref
              ]
          ]))

  (* Record conversions *)
  let bin_read_record full_type_name loc fields =
    let rhs = bin_read_label_declaration_list full_type_name loc fields (fun x -> x) in
    `Open rhs

  (* Empty types *)
  let bin_read_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Closed
      [%expr  fun _buf ~pos_ref ->
           Bin_prot.Common.raise_read_error
             (Bin_prot.Common.ReadError.Empty_type [%e estring ~loc full_type_name])
             !pos_ref
      ]


  (* Generate code from type definitions *)

  let reader_body_of_td td full_type_name =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_variant cds -> bin_read_sum    full_type_name loc cds
    | Ptype_record  lds -> bin_read_record full_type_name loc lds
    | Ptype_open ->
      Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> bin_read_nil full_type_name loc
      | Some ty ->
        bin_read_type_toplevel full_type_name loc ty
          ~full_type:(core_type_of_type_declaration td)

  (* When the type is a polymorphic variant the main bin_read_NAME function reads an
     integer and calls the __bin_read_NAME__ function wrapped into a try-with. *)
  let main_body_for_polymorphic_variant ~loc ~vtag_read_name ~full_type_name ~args =
    let full_type_name = full_type_name_or_anonymous full_type_name in
    let vtag_read_expr = evar ~loc vtag_read_name in
    [%expr
      fun buf ~pos_ref ->
        let vint =
          Bin_prot.Read.bin_read_variant_int buf ~pos_ref
        in
        try
          [%e eapply ~loc vtag_read_expr (exprs_of_vars args)] buf ~pos_ref vint
        with Bin_prot.Common.No_variant_match ->
          let err =
            Bin_prot.Common.ReadError.Variant [%e estring ~loc full_type_name]
          in
          Bin_prot.Common.raise_read_error err !pos_ref
    ]

  module Td_class = struct
    type polymorphic_variant = { all_atoms : bool }

    type t =
      | Polymorphic_variant of polymorphic_variant
      | Alias_but_not_polymorphic_variant
      | Other

    let of_core_type ty =
      match ty.ptyp_desc with
      | Ptyp_variant (row_fields, _, _) ->
        let all_atoms =
          List.for_all row_fields ~f:(fun row_field ->
            match row_field.prf_desc with
            | Rtag (_, is_constant, _) -> is_constant
            | Rinherit _ -> false)
        in
        Polymorphic_variant { all_atoms }
      | _ -> Alias_but_not_polymorphic_variant

    let of_td td =
      match td.ptype_kind, td.ptype_manifest with
      | Ptype_abstract, Some ty -> of_core_type ty
      | _ -> Other
  end

  let variant_wrong_type ~loc full_type_name =
    let full_type_name = full_type_name_or_anonymous full_type_name in
    [%expr
      fun _buf ~pos_ref _vint ->
        Bin_prot.Common.raise_variant_wrong_type
          [%e estring ~loc full_type_name] !pos_ref
    ]

  let vtag_reader ~loc ~(td_class : Td_class.t) ~full_type_name ~oc_body =
    match td_class with
    | Alias_but_not_polymorphic_variant -> begin
        match oc_body with
        | `Closed call ->
          let rec rewrite_call cnv e =
            let loc = e.pexp_loc in
            match e.pexp_desc with
            | Pexp_apply (f,[arg]) ->
              rewrite_call (fun new_f -> cnv (pexp_apply ~loc new_f [arg])) f
            | Pexp_ident { txt = Ldot (Ldot (Lident "Bin_prot", "Read"), _); _ } ->
              variant_wrong_type ~loc full_type_name
            | Pexp_ident { txt = Lident name; _ }
              when String.is_prefix name ~prefix:"_o" ->
              let full_type_name = Full_type_name.get_exn ~loc full_type_name in
              [%expr
                fun _buf ~pos_ref _vint ->
                  Bin_prot.Common.raise_read_error
                    (Bin_prot.Common.ReadError.Silly_type
                       [%e estring ~loc full_type_name])
                    !pos_ref
              ]
            | Pexp_ident id ->
              let expr = unapplied_type_constr_conv ~loc id ~f:(fun s -> "__" ^ s ^ "__") in
              let cnv_expr = cnv expr in
              alias_or_fun cnv_expr [%expr
                fun buf ~pos_ref vint ->
                  [%e cnv_expr] buf ~pos_ref vint
              ]
            | _ ->
              let s = Pprintast.string_of_expression e in
              Location.raise_errorf ~loc "ppx_bin_prot: impossible case: %s" s
          in
          rewrite_call (fun x -> x) (curry_applications call)
        | _ -> variant_wrong_type ~loc full_type_name
      end
    | Polymorphic_variant { all_atoms } -> begin
        match oc_body with
        | `Open body when all_atoms ->
          [%expr fun _buf ~pos_ref:_ vint -> [%e body] ]
        | `Open body -> [%expr fun buf ~pos_ref vint -> [%e body] ]
        | _ -> assert false (* impossible *)
      end
    | Other ->
      variant_wrong_type ~loc full_type_name

  let read_and_vtag_read_bindings ~loc
      ~read_name ~read_binding_type
      ~vtag_read_name ~vtag_read_binding_type
      ~full_type_name ~(td_class : Td_class.t) ~args ~oc_body =
    let read_binding =
      let body =
        match td_class with
        | Polymorphic_variant _ ->
          main_body_for_polymorphic_variant ~loc ~vtag_read_name ~full_type_name
            ~args
        | Alias_but_not_polymorphic_variant | Other ->
          match oc_body with
          | `Closed expr ->
            alias_or_fun expr [%expr fun buf ~pos_ref -> [%e expr] buf ~pos_ref ]
          | `Open body -> [%expr fun buf ~pos_ref -> [%e body] ]
      in
      let pat = pvar ~loc read_name in
      let pat_with_type =
        match read_binding_type with
        | None -> pat
        | Some ty -> ppat_constraint ~loc pat ty
      in
      value_binding ~loc
        ~pat:pat_with_type
        ~expr:(eabstract ~loc (patts_of_vars args) body)
    in
    let vtag_read_binding =
      let pat = pvar ~loc vtag_read_name in
      let pat_with_type =
        match vtag_read_binding_type with
        | None -> pat
        | Some ty -> ppat_constraint ~loc pat ty
      in
      value_binding ~loc
        ~pat:pat_with_type
        ~expr:(eabstract ~loc (patts_of_vars args)
                 (vtag_reader ~loc ~td_class ~full_type_name ~oc_body))
    in
    (read_binding, vtag_read_binding)

  let reader_type_class_record ~loc ~read ~vtag_read =
    [%expr
      { Bin_prot.Type_class.
        read      = [%e read]
      ; vtag_read = [%e vtag_read]
      }
    ]

  let bin_read_td ~can_omit_type_annot ~loc:_ ~path td =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let oc_body = reader_body_of_td td full_type_name in
    let read_name      =   "bin_read_" ^ td.ptype_name.txt        in
    let vtag_read_name = "__bin_read_" ^ td.ptype_name.txt ^ "__" in
    let vtag_read_binding_type, read_binding_type =
      if can_omit_type_annot then
        None, None
      else
        Some (generate_poly_type ~loc td "Bin_prot.Read.reader"
                ~wrap_result:(fun ~loc t -> [%type: int -> [%t t]])),
        Some (generate_poly_type ~loc td "Bin_prot.Read.reader")
    in
    let read_binding, vtag_read_binding =
      let args = vars_of_params td ~prefix:"_of__" in
      read_and_vtag_read_bindings ~loc ~read_name ~read_binding_type
        ~vtag_read_name ~vtag_read_binding_type
        ~full_type_name ~td_class:(Td_class.of_td td) ~args ~oc_body
    in
    let vars = vars_of_params td ~prefix:"bin_reader_" in
    let read =
      let call = project_vars (evar ~loc read_name) vars ~field_name:"read" in
      alias_or_fun call [%expr fun buf ~pos_ref -> [%e call] buf ~pos_ref ]
    in
    let vtag_read =
      let call = project_vars (evar ~loc vtag_read_name) vars ~field_name:"read" in
      alias_or_fun call [%expr fun buf ~pos_ref vtag -> [%e call] buf ~pos_ref vtag ]
    in
    let reader = reader_type_class_record ~loc ~read ~vtag_read in
    let reader_binding =
      value_binding ~loc
        ~pat:(pvar ~loc @@ "bin_reader_" ^ td.ptype_name.txt)
        ~expr:(eabstract ~loc (patts_of_vars vars) reader)
    in
    (vtag_read_binding, (read_binding, reader_binding))

  (* Generate code from type definitions *)
  let bin_read ~loc ~path (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag = really_recursive rec_flag tds in
    (match rec_flag, tds with
     | Nonrecursive, _ :: _ :: _ ->
       (* there can be captures in the generated code if we allow this *)
       Location.raise_errorf ~loc
         "bin_prot doesn't support multiple nonrecursive definitions."
     | _ -> ());
    let can_omit_type_annot = List.for_all tds ~f:would_rather_omit_type_signatures in
    let vtag_read_bindings, read_and_reader_bindings =
      List.map tds ~f:(bin_read_td ~can_omit_type_annot ~loc ~path)
      |> List.unzip
    in
    let read_bindings, reader_bindings = List.unzip read_and_reader_bindings in
    let defs =
      match rec_flag with
      | Recursive ->
        [ pstr_value ~loc Recursive (vtag_read_bindings @ read_bindings) ]
      | Nonrecursive ->
        let cnv binding = pstr_value ~loc Nonrecursive [binding] in
        List.map vtag_read_bindings ~f:cnv @
        List.map      read_bindings ~f:cnv
    in
    defs @ [ pstr_value ~loc Nonrecursive reader_bindings ]

  let gen = Deriving.Generator.make Deriving.Args.empty bin_read

  let extension ~loc ~path:_ ty =
    let loc = { loc with loc_ghost = true } in
    let full_type_name = Full_type_name.absent in
    let read_name      = "read"      in
    let vtag_read_name = "vtag_read" in
    let read_binding, vtag_read_binding =
      let oc_body = bin_read_type_toplevel full_type_name loc ty ~full_type:ty in
      read_and_vtag_read_bindings ~loc ~read_name ~read_binding_type:None
        ~vtag_read_name ~vtag_read_binding_type:None
        ~full_type_name ~td_class:(Td_class.of_core_type ty) ~args:[] ~oc_body
    in
    pexp_let ~loc Nonrecursive [vtag_read_binding]
      (pexp_let ~loc Nonrecursive [read_binding]
         (reader_type_class_record ~loc
            ~read:     (evar ~loc read_name)
            ~vtag_read:(evar ~loc vtag_read_name)))
  ;;
end

(* Generator for binary protocol type classes *)
module Generate_tp_class = struct
  let tp_record ~loc ~writer ~reader ~shape =
    [%expr
      { Bin_prot.Type_class.
        writer = [%e writer]
      ; reader = [%e reader]
      ; shape = [%e shape]
      }
    ]

  let bin_tp_class_td td =
    let loc = td.ptype_loc in
    let tparam_cnvs =
      List.map td.ptype_params ~f:(fun tp ->
        let name = get_type_param_name tp in
        "bin_" ^ name.txt)
    in
    let mk_pat id = pvar ~loc id in
    let tparam_patts = List.map tparam_cnvs ~f:mk_pat in
    let writer =
      let tparam_exprs =
        List.map td.ptype_params ~f:(fun tp ->
          let name = get_type_param_name tp in
          [%expr
            [%e evar ~loc:name.loc @@ "bin_" ^ name.txt]
            .Bin_prot.Type_class.writer
          ])
      in
      eapply ~loc (evar ~loc @@ "bin_writer_" ^ td.ptype_name.txt) tparam_exprs
    in
    let reader =
      let tparam_exprs =
        List.map td.ptype_params ~f:(fun tp ->
          let name = get_type_param_name tp in
          [%expr
            [%e evar ~loc:name.loc @@ "bin_" ^ name.txt]
            .Bin_prot.Type_class.reader
          ])
      in
      eapply ~loc (evar ~loc @@ "bin_reader_" ^ td.ptype_name.txt) tparam_exprs
    in
    let shape =
      let tparam_exprs =
        List.map td.ptype_params ~f:(fun tp ->
          let name = get_type_param_name tp in
          [%expr
            [%e evar ~loc:name.loc @@ "bin_" ^ name.txt]
            .Bin_prot.Type_class.shape
          ])
      in
      eapply ~loc (evar ~loc @@ "bin_shape_" ^ td.ptype_name.txt) tparam_exprs
    in
    let body = tp_record ~loc ~writer ~reader ~shape in
    value_binding ~loc ~pat:(pvar ~loc @@ "bin_" ^ td.ptype_name.txt)
      ~expr:(eabstract ~loc tparam_patts body)

  (* Generate code from type definitions *)
  let bin_tp_class ~loc ~path:_ (_rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let bindings = List.map tds ~f:bin_tp_class_td in
    [ pstr_value ~loc Nonrecursive bindings ]

  (* Add code generator to the set of known generators *)
  let gen = Deriving.Generator.make Deriving.Args.empty bin_tp_class

  let extension ~loc ~path ty =
    let loc = { loc with loc_ghost = true } in
    tp_record ~loc
      ~writer:(Generate_bin_write.extension ~loc ~path ty)
      ~reader:(Generate_bin_read .extension ~loc ~path ty)
      ~shape:(Bin_shape_expand.shape_extension ~loc ty)
  ;;
end

let bin_shape =
  Deriving.add
    "bin_shape"
    ~str_type_decl:Bin_shape_expand.str_gen
    ~sig_type_decl:Bin_shape_expand.sig_gen
    ~extension:(fun ~loc ~path:_ -> Bin_shape_expand.shape_extension ~loc)

let () =
  Deriving.add
    "bin_digest"
    ~extension:(fun ~loc ~path:_ -> Bin_shape_expand.digest_extension ~loc)
  |> Deriving.ignore

let bin_write =
  Deriving.add
    "bin_write"
    ~str_type_decl:Generate_bin_write.gen
    ~sig_type_decl:Sig.bin_write

let () =
  Deriving.add
    "bin_writer"
    ~extension:Generate_bin_write.extension
  |> Deriving.ignore

let bin_read =
  Deriving.add
    "bin_read"
    ~str_type_decl:Generate_bin_read.gen
    ~sig_type_decl:Sig.bin_read

let () =
  Deriving.add
    "bin_reader"
    ~extension:Generate_bin_read.extension
  |> Deriving.ignore

let bin_type_class =
  Deriving.add
    "bin_type_class"
    ~str_type_decl:Generate_tp_class.gen
    ~sig_type_decl:Sig.bin_type_class
    ~extension:Generate_tp_class.extension

let bin_io_named_sig =
  Deriving.add "bin_io.named_sig.prevent using this in source files"
    ~sig_type_decl:Sig.named

let bin_io =
  let set = [bin_shape; bin_write; bin_read; bin_type_class] in
  Deriving.add_alias "bin_io" set
    ~sig_type_decl:[bin_io_named_sig]
    ~str_type_decl:(List.rev set)
