open! Import
open Ast_builder.Default
module Buffer = Caml.Buffer
module Format = Caml.Format

let lident x = Longident.Lident x

let core_type_of_type_declaration td =
  let loc = td.ptype_name.loc in
  ptyp_constr ~loc
    (Located.map lident td.ptype_name)
    (List.map td.ptype_params ~f:fst)

let strip_gen_symbol_suffix =
  let chop n ~or_more string pos f =
    let target = !pos - n in
    while !pos > 0 && (or_more || !pos > target) && f string.[!pos - 1] do
      pos := !pos - 1
    done;
    !pos <= target
  in
  fun string ->
    let pos = ref (String.length string) in
    if
      chop 1 ~or_more:false string pos (Char.equal '_')
      && chop 3 ~or_more:true string pos (function
           | '0' .. '9' -> true
           | _ -> false)
      && chop 2 ~or_more:false string pos (Char.equal '_')
    then String.prefix string !pos
    else string

let gen_symbol =
  let cnt = ref 0 in
  fun ?(prefix = "_x") () ->
    cnt := !cnt + 1;
    let prefix = strip_gen_symbol_suffix prefix in
    Printf.sprintf "%s__%03i_" prefix !cnt

let name_type_params_in_td (td : type_declaration) : type_declaration =
  let prefix_string i =
    (* a, b, ..., y, z, aa, bb, ... *)
    String.make ((i / 26) + 1) (Char.chr (Char.code 'a' + (i mod 26)))
  in
  let name_param i (tp, variance) =
    let ptyp_desc =
      match tp.ptyp_desc with
      | Ptyp_any -> Ptyp_var (gen_symbol ~prefix:(prefix_string i) ())
      | Ptyp_var _ as v -> v
      | _ -> Location.raise_errorf ~loc:tp.ptyp_loc "not a type parameter"
    in
    ({ tp with ptyp_desc }, variance)
  in
  { td with ptype_params = List.mapi td.ptype_params ~f:name_param }

let combinator_type_of_type_declaration td ~f =
  let td = name_type_params_in_td td in
  let result_type =
    f ~loc:td.ptype_name.loc (core_type_of_type_declaration td)
  in
  List.fold_right td.ptype_params ~init:result_type
    ~f:(fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      ptyp_arrow ~loc Nolabel (f ~loc tp) acc)

let string_of_core_type ct =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Pprintast.core_type ppf ct;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let get_type_param_name (ty, _) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_var name -> Located.mk ~loc name
  | _ -> Location.raise_errorf ~loc "not a type parameter"

exception Type_is_recursive

class type_is_recursive rec_flag tds =
  object (self)
    inherit Ast_traverse.iter as super
    val type_names : string list = List.map tds ~f:(fun td -> td.ptype_name.txt)
    method return_true () = raise_notrace Type_is_recursive

    method! core_type ctype =
      match ctype.ptyp_desc with
      | Ptyp_arrow _ -> ()
      | Ptyp_constr ({ txt = Longident.Lident id; _ }, _)
        when List.mem ~set:type_names id ->
          self#return_true ()
      | _ -> super#core_type ctype

    method! constructor_declaration cd =
      (* Don't recurse through cd.pcd_res *)
      match cd.pcd_args with
      | Pcstr_tuple args -> List.iter args ~f:self#core_type
      | Pcstr_record fields -> List.iter fields ~f:self#label_declaration

    method! attributes _ = (* Don't recurse through attributes *)
                           ()

    method go () =
      match rec_flag with
      | Nonrecursive -> Nonrecursive
      | Recursive -> (
          match List.iter tds ~f:self#type_declaration with
          | exception Type_is_recursive -> Recursive
          | () -> Nonrecursive)
  end

let really_recursive rec_flag tds = (new type_is_recursive rec_flag tds)#go ()
let rec last x l = match l with [] -> x | x :: l -> last x l

let loc_of_name_and_payload name payload =
  match payload with
  | PStr [] -> name.loc
  | PStr (x :: l) -> { x.pstr_loc with loc_end = (last x l).pstr_loc.loc_end }
  | PSig [] -> name.loc
  | PSig (x :: l) -> { x.psig_loc with loc_end = (last x l).psig_loc.loc_end }
  | PTyp t -> t.ptyp_loc
  | PPat (x, None) -> x.ppat_loc
  | PPat (x, Some e) -> { x.ppat_loc with loc_end = e.pexp_loc.loc_end }

let loc_of_payload { attr_name; attr_payload; attr_loc = _ } =
  loc_of_name_and_payload attr_name attr_payload

let loc_of_attribute { attr_name; attr_payload; attr_loc = _ } =
  (* TODO: fix this in the compiler, and move the logic to omp when converting
     from older asts. *)
  (* "ocaml.doc" attributes are generated with [Location.none], which is not helpful for
     error messages. *)
  if Poly.( = ) attr_name.loc Location.none then
    loc_of_name_and_payload attr_name attr_payload
  else
    {
      attr_name.loc with
      loc_end = (loc_of_name_and_payload attr_name attr_payload).loc_end;
    }

let loc_of_extension (name, payload) =
  if Poly.( = ) name.loc Location.none then loc_of_name_and_payload name payload
  else
    { name.loc with loc_end = (loc_of_name_and_payload name payload).loc_end }

let curry_applications expr =
  let open Ast_builder_generated.M in
  match expr.pexp_desc with
  | Pexp_apply (f, orig_forward_args) ->
      let loc = expr.pexp_loc in
      let rec loop = function
        | [] -> f
        | last_arg :: rev_front_args ->
            pexp_apply ~loc (loop rev_front_args) [ last_arg ]
      in
      loop (List.rev orig_forward_args)
  | _ -> expr

let rec assert_no_attributes = function
  | [] -> ()
  | { attr_name = name; attr_loc = _; attr_payload = _ } :: rest
    when Name.ignore_checks name.Location.txt ->
      assert_no_attributes rest
  | attr :: _ ->
      let loc = loc_of_attribute attr in
      Location.raise_errorf ~loc "Attributes not allowed here"

let assert_no_attributes_in =
  object
    inherit Ast_traverse.iter
    method! attribute a = assert_no_attributes [ a ]
  end

let attribute_of_warning loc s =
  {
    attr_name = { loc; txt = "ocaml.ppwarning" };
    attr_payload = PStr [ pstr_eval ~loc (estring ~loc s) [] ];
    attr_loc = loc;
  }

let is_polymorphic_variant =
  let rec check = function
    | { ptyp_desc = Ptyp_variant _; _ } -> `Definitely
    | { ptyp_desc = Ptyp_alias (typ, _); _ } -> check typ
    | { ptyp_desc = Ptyp_constr _; _ } -> `Maybe
    | _ -> `Surely_not
    (* Type vars go here even though they could be polymorphic
       variants, however we don't handle it if they get substituted
       by a polymorphic variant that is then included. *)
  in
  fun td ~sig_ ->
    match td.ptype_kind with
    | Ptype_variant _ | Ptype_record _ | Ptype_open -> `Surely_not
    | Ptype_abstract -> (
        match td.ptype_manifest with
        | None -> if sig_ then `Maybe else `Surely_not
        | Some typ -> check typ)

let mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant = function
  | [ td ]
    when String.equal td.ptype_name.txt "t" && List.is_empty td.ptype_cstrs ->
      if
        (not handle_polymorphic_variant)
        && Poly.( = ) (is_polymorphic_variant td ~sig_:true) `Definitely
      then None
      else
        let arity = List.length td.ptype_params in
        if arity >= 4 then None
        else
          let mty =
            if arity = 0 then sg_name else Printf.sprintf "%s%d" sg_name arity
          in
          let td = name_type_params_in_td td in
          let for_subst =
            Ast_helper.Type.mk ~loc td.ptype_name ~params:td.ptype_params
              ~manifest:
                (ptyp_constr ~loc
                   (Located.map_lident td.ptype_name)
                   (List.map ~f:fst td.ptype_params))
          in
          Some
            (include_infos ~loc
               (pmty_with ~loc
                  (pmty_ident ~loc (Located.lident mty ~loc))
                  [ Pwith_typesubst (Located.lident ~loc "t", for_subst) ]))
  | _ -> None
