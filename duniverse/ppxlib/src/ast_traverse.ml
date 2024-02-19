open! Import
open Common.With_errors
include Ast_traverse0

class virtual ['ctx, 'res] lift_map_with_context =
  object
    inherit ['ctx, 'res] Ppxlib_traverse_builtins.lift_map_with_context
    inherit ['ctx, 'res] Ast.lift_map_with_context
  end

let module_name = function None -> "_" | Some name -> name
let enter name path = if String.is_empty path then name else path ^ "." ^ name
let enter_opt name_opt path = enter (module_name name_opt) path

class map_with_path =
  object
    inherit [string] map_with_context as super

    (* WAS:
       method! structure_item_desc path x =
       match x with
       | Pstr_module mb -> super#structure_item_desc (enter mb.pmb_name.txt path) x
       | _ -> super#structure_item_desc path x

       Overriding [module_binding] seems to be OK because it does not catch
       local module bindings because at the moment the parsetree doesn't make
       use of [module_binding] for local modules, but that might change in the
       future, so this might be something to keep in mind.

       The following:

           module A = struct .. end
           module A = struct .. end

       is disallowed, but

           let _ = .. let module A = struct .. end in ..
           module A = struct .. end
           let _ = .. let module A = struct .. end in ..

       isn't, and the "path" constructed here would be able to differentiate
       between them. *)
    method! module_binding path mb =
      super#module_binding (enter_opt mb.pmb_name.txt path) mb

    method! module_declaration path md =
      super#module_declaration (enter_opt md.pmd_name.txt path) md

    method! module_type_declaration path mtd =
      super#module_type_declaration (enter mtd.pmtd_name.txt path) mtd
  end

let var_names_of =
  object
    inherit [string list] fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with Ppat_var { txt; _ } -> txt :: acc | _ -> acc
  end

let ec_enter_module_opt ~loc name_opt ctxt =
  Expansion_context.Base.enter_module ~loc (module_name name_opt) ctxt

let enter_value =
  Attribute.declare "ppxlib.enter_value" Expression
    Ast_pattern.(single_expr_payload (pexp_ident (lident __')))
    Fn.id

let enter_module =
  Attribute.declare "ppxlib.enter_module" Module_expr
    Ast_pattern.(single_expr_payload (pexp_construct (lident __') none))
    Fn.id

let do_not_enter_value_binding =
  Attribute.declare "ppxlib.do_not_enter_value" Value_binding
    Ast_pattern.(pstr nil)
    ()

let do_not_enter_value_description =
  Attribute.declare "ppxlib.do_not_enter_value" Value_description
    Ast_pattern.(pstr nil)
    ()

let do_not_enter_module_binding =
  Attribute.declare "ppxlib.do_not_enter_module" Module_binding
    Ast_pattern.(pstr nil)
    ()

let do_not_enter_module_declaration =
  Attribute.declare "ppxlib.do_not_enter_module" Module_declaration
    Ast_pattern.(pstr nil)
    ()

let do_not_enter_module_type_declaration =
  Attribute.declare "ppxlib.do_not_enter_module" Module_type_declaration
    Ast_pattern.(pstr nil)
    ()

let do_not_enter_let_module =
  Attribute.declare "ppxlib.do_not_enter_module" Expression
    Ast_pattern.(pstr nil)
    ()

class map_with_expansion_context_and_errors =
  let return _ctx x = (x, []) in
  object (self)
    inherit
      [Expansion_context.Base.t, Location.Error.t list] lift_map_with_context as super

    method int = return
    method string = return
    method bool = return
    method char = return
    method float = return
    method int32 = return
    method int64 = return
    method nativeint = return
    method unit = return

    method array
        : 'a.
          (Expansion_context.Base.t -> 'a -> 'a * Location.Error.t list) ->
          Expansion_context.Base.t ->
          'a array ->
          'a array * Location.Error.t list =
      fun f ctx a ->
        let list, errors = self#list f ctx (Array.to_list a) in
        (Array.of_list list, errors)

    method other : 'a. Expansion_context.Base.t -> 'a -> Location.Error.t list =
      fun _ _ -> []

    method record _ctx fields = List.concat_map fields ~f:snd
    method constr _ctx _tag args = List.concat args
    method tuple _ctx l = List.concat l

    method! expression ctxt
        ({ pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } as expr) =
      let with_value =
        Attribute.get_res enter_value expr |> of_result ~default:None
        >>| function
        | None -> ctxt
        | Some { loc; txt } -> Expansion_context.Base.enter_value ~loc txt ctxt
      in
      with_value >>= fun ctxt ->
      let ctxt = Expansion_context.Base.enter_expr ctxt in
      let pexp_desc, desc_errors =
        match pexp_desc with
        | Pexp_letmodule (name, module_expr, body) ->
            let name, name_errors =
              self#loc (self#option self#string) ctxt name
            in
            let module_expr, module_expr_errors =
              let with_let_module =
                Attribute.get_res do_not_enter_let_module expr
                |> of_result ~default:None
                >>| function
                | Some () -> ctxt
                | None ->
                    ec_enter_module_opt ~loc:module_expr.pmod_loc name.txt ctxt
              in
              with_let_module >>= fun ctxt -> self#module_expr ctxt module_expr
            in
            let body, body_errors = self#expression ctxt body in
            let errors =
              self#constr ctxt "Pexp_letmodule"
                [ name_errors; module_expr_errors; body_errors ]
            in
            (Pexp_letmodule (name, module_expr, body), errors)
        | _ -> self#expression_desc ctxt pexp_desc
      in
      let pexp_loc, loc_errors = self#location ctxt pexp_loc in
      let pexp_loc_stack, loc_stack_errors =
        self#list self#location ctxt pexp_loc_stack
      in
      let pexp_attributes, attributes_errors =
        self#attributes ctxt pexp_attributes
      in
      ( { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes },
        self#record ctxt
          [
            ("pexp_desc", desc_errors);
            ("pexp_loc", loc_errors);
            ("pexp_loc_stack", loc_stack_errors);
            ("attributes", attributes_errors);
          ] )

    method! module_expr ctxt me =
      let with_module_expr =
        Attribute.get_res enter_module me |> of_result ~default:None
        >>| function
        | None -> ctxt
        | Some { loc; txt } -> Expansion_context.Base.enter_module ~loc txt ctxt
      in
      with_module_expr >>= fun ctxt -> super#module_expr ctxt me

    method! module_binding ctxt mb =
      let with_module_binding =
        Attribute.get_res do_not_enter_module_binding mb
        |> of_result ~default:None
        >>| function
        | Some () -> ctxt
        | None -> ec_enter_module_opt ~loc:mb.pmb_loc mb.pmb_name.txt ctxt
      in
      with_module_binding >>= fun ctxt -> super#module_binding ctxt mb

    method! module_declaration ctxt md =
      let with_module_declaration =
        Attribute.get_res do_not_enter_module_declaration md
        |> of_result ~default:None
        >>| function
        | Some () -> ctxt
        | None -> ec_enter_module_opt ~loc:md.pmd_loc md.pmd_name.txt ctxt
      in
      with_module_declaration >>= fun ctxt -> super#module_declaration ctxt md

    method! module_type_declaration ctxt mtd =
      let with_module_type_declaration =
        Attribute.get_res do_not_enter_module_type_declaration mtd
        |> of_result ~default:None
        >>| function
        | Some () -> ctxt
        | None ->
            Expansion_context.Base.enter_module ~loc:mtd.pmtd_loc
              mtd.pmtd_name.txt ctxt
      in
      with_module_type_declaration >>= fun ctxt ->
      super#module_type_declaration ctxt mtd

    method! value_description ctxt vd =
      let with_value_description =
        Attribute.get_res do_not_enter_value_description vd
        |> of_result ~default:None
        >>| function
        | Some () -> ctxt
        | None ->
            Expansion_context.Base.enter_value ~loc:vd.pval_loc vd.pval_name.txt
              ctxt
      in
      with_value_description >>= fun ctxt -> super#value_description ctxt vd

    method! value_binding ctxt
        ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } as vb) =
      Attribute.get_res do_not_enter_value_binding vb |> of_result ~default:None
      >>= function
      | Some () -> super#value_binding ctxt vb
      | None ->
          let in_binding_ctxt =
            match var_names_of#pattern pvb_pat [] with
            | [] | _ :: _ :: _ -> ctxt
            | [ var_name ] ->
                Expansion_context.Base.enter_value ~loc:pvb_loc var_name ctxt
          in
          let pvb_pat, pat_errors = self#pattern ctxt pvb_pat in
          let pvb_expr, expr_errors =
            self#expression in_binding_ctxt pvb_expr
          in
          let pvb_attributes, attributes_errors =
            self#attributes in_binding_ctxt pvb_attributes
          in
          let pvb_loc, loc_errors = self#location ctxt pvb_loc in
          let errors =
            self#record ctxt
              [
                ("pvb_pat", pat_errors);
                ("pvb_expr", expr_errors);
                ("pvb_attributes", attributes_errors);
                ("pvb_loc", loc_errors);
              ]
          in
          ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc }, errors)
  end

class sexp_of =
  object
    inherit [Sexp.t] Ast.lift
    method int = sexp_of_int
    method string = sexp_of_string
    method bool = sexp_of_bool
    method char = sexp_of_char
    method float = sexp_of_float
    method int32 = sexp_of_int32
    method int64 = sexp_of_int64
    method nativeint = sexp_of_nativeint
    method unit = sexp_of_unit
    method option = sexp_of_option
    method list = sexp_of_list
    method array : 'a. ('a -> Sexp.t) -> 'a array -> Sexp.t = sexp_of_array
    method other : 'a. 'a -> Sexp.t = fun _ -> Sexp.Atom "_"

    method record fields =
      List
        (List.map fields ~f:(fun (label, sexp) ->
             Sexp.List [ Atom label; sexp ]))

    method constr tag args =
      match args with [] -> Atom tag | _ -> List (Atom tag :: args)

    method tuple l = List l
  end

let sexp_of = new sexp_of
