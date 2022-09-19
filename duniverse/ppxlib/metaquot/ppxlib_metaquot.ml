open Ppxlib
open Ast_builder.Default
module E = Extension
module A = Ast_pattern

module Make (M : sig
  type result

  val annotate : result -> core_type -> result
  val cast : extension -> result
  val location : location -> result
  val location_stack : (location -> result) option
  val attributes : (location -> result) option

  class std_lifters : location -> [result] Ppxlib_traverse_builtins.std_lifters
end) =
struct
  let lift loc =
    object (self)
      inherit [M.result] Ast_traverse.lift as super
      inherit! M.std_lifters loc

      method typed ast name =
        let loc = { loc with loc_ghost = true } in
        M.annotate ast
          (ptyp_constr ~loc
             { loc; txt = Ldot (Ldot (Lident "Ppxlib_ast", "Ast"), name) }
             [])

      method! attribute x =
        Attribute.mark_as_handled_manually x;
        super#attribute x

      method! location _ = M.location loc

      method! attributes x =
        match M.attributes with
        | None -> super#attributes x
        | Some f ->
            assert_no_attributes x;
            f loc

      method! location_stack x =
        match M.location_stack with
        | None -> super#location_stack x
        | Some f -> f loc

      method! expression e =
        match e.pexp_desc with
        | Pexp_extension (({ txt = "e"; _ }, _) as ext) ->
            self#typed (M.cast ext) "expression"
        | _ -> super#expression e

      method! pattern p =
        match p.ppat_desc with
        | Ppat_extension (({ txt = "p"; _ }, _) as ext) ->
            self#typed (M.cast ext) "pattern"
        | _ -> super#pattern p

      method! core_type t =
        match t.ptyp_desc with
        | Ptyp_extension (({ txt = "t"; _ }, _) as ext) ->
            self#typed (M.cast ext) "core_type"
        | _ -> super#core_type t

      method! module_expr m =
        match m.pmod_desc with
        | Pmod_extension (({ txt = "m"; _ }, _) as ext) ->
            self#typed (M.cast ext) "module_expr"
        | _ -> super#module_expr m

      method! module_type m =
        match m.pmty_desc with
        | Pmty_extension (({ txt = "m"; _ }, _) as ext) ->
            self#typed (M.cast ext) "module_type"
        | _ -> super#module_type m

      method! structure_item i =
        match i.pstr_desc with
        | Pstr_extension ((({ txt = "i"; _ }, _) as ext), attrs) ->
            assert_no_attributes attrs;
            self#typed (M.cast ext) "structure_item"
        | _ -> super#structure_item i

      method! signature_item i =
        match i.psig_desc with
        | Psig_extension ((({ txt = "i"; _ }, _) as ext), attrs) ->
            assert_no_attributes attrs;
            self#typed (M.cast ext) "signature_item"
        | _ -> super#signature_item i
    end
end

module Expr = Make (struct
  type result = expression

  let location loc = evar ~loc:{ loc with loc_ghost = true } "loc"
  let location_stack = None
  let attributes = None

  class std_lifters = Ppxlib_metaquot_lifters.expression_lifters

  let annotate e core_type = pexp_constraint ~loc:core_type.ptyp_loc e core_type

  let cast ext =
    match snd ext with
    | PStr [ { pstr_desc = Pstr_eval (e, attrs); _ } ] ->
        assert_no_attributes attrs;
        e
    | _ ->
        Location.raise_errorf ~loc:(loc_of_extension ext) "expression expected"
end)

module Patt = Make (struct
  type result = pattern

  let location loc = ppat_any ~loc:{ loc with loc_ghost = true }

  let location_stack =
    Some (fun loc -> ppat_any ~loc:{ loc with loc_ghost = true })

  let attributes = Some (fun loc -> ppat_any ~loc:{ loc with loc_ghost = true })

  class std_lifters = Ppxlib_metaquot_lifters.pattern_lifters

  let annotate p core_type = ppat_constraint ~loc:core_type.ptyp_loc p core_type

  let cast ext =
    match snd ext with
    | PPat (p, None) -> p
    | PPat (_, Some e) ->
        Location.raise_errorf ~loc:e.pexp_loc "guard not expected here"
    | _ -> Location.raise_errorf ~loc:(loc_of_extension ext) "pattern expected"
end)

let () =
  let extensions ctx lifter =
    [
      E.declare "metaquot.expr" ctx
        A.(single_expr_payload __)
        (fun ~loc ~path:_ e ->
          let lift = lifter loc in
          lift#typed (lift#expression e) "expression");
      E.declare "metaquot.pat" ctx
        A.(ppat __ none)
        (fun ~loc ~path:_ p ->
          let lift = lifter loc in
          lift#typed (lift#pattern p) "pattern");
      E.declare "metaquot.str" ctx
        A.(pstr __)
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#structure s) "structure");
      E.declare "metaquot.stri" ctx
        A.(pstr (__ ^:: nil))
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#structure_item s) "structure_item");
      E.declare "metaquot.sig" ctx
        A.(psig __)
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#signature s) "signature");
      E.declare "metaquot.sigi" ctx
        A.(psig (__ ^:: nil))
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#signature_item s) "signature_item");
      E.declare "metaquot.type" ctx
        A.(ptyp __)
        (fun ~loc ~path:_ t ->
          let lift = lifter loc in
          lift#typed (lift#core_type t) "core_type");
    ]
  in
  let extensions =
    extensions Expression Expr.lift @ extensions Pattern Patt.lift
  in
  Driver.register_transformation "metaquot" ~extensions
