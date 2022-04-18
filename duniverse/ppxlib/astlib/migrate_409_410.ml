module From = Ast_409
module To = Ast_410

let map_option f x = match x with None -> None | Some x -> Some (f x)

let rec copy_toplevel_phrase :
    Ast_409.Parsetree.toplevel_phrase -> Ast_410.Parsetree.toplevel_phrase =
  function
  | Ast_409.Parsetree.Ptop_def x0 ->
      Ast_410.Parsetree.Ptop_def (copy_structure x0)
  | Ast_409.Parsetree.Ptop_dir x0 ->
      Ast_410.Parsetree.Ptop_dir (copy_toplevel_directive x0)

and copy_toplevel_directive :
    Ast_409.Parsetree.toplevel_directive -> Ast_410.Parsetree.toplevel_directive
    =
 fun {
       Ast_409.Parsetree.pdir_name;
       Ast_409.Parsetree.pdir_arg;
       Ast_409.Parsetree.pdir_loc;
     } ->
  {
    Ast_410.Parsetree.pdir_name = copy_loc (fun x -> x) pdir_name;
    Ast_410.Parsetree.pdir_arg = map_option copy_directive_argument pdir_arg;
    Ast_410.Parsetree.pdir_loc = copy_location pdir_loc;
  }

and copy_directive_argument :
    Ast_409.Parsetree.directive_argument -> Ast_410.Parsetree.directive_argument
    =
 fun { Ast_409.Parsetree.pdira_desc; Ast_409.Parsetree.pdira_loc } ->
  {
    Ast_410.Parsetree.pdira_desc = copy_directive_argument_desc pdira_desc;
    Ast_410.Parsetree.pdira_loc = copy_location pdira_loc;
  }

and copy_directive_argument_desc :
    Ast_409.Parsetree.directive_argument_desc ->
    Ast_410.Parsetree.directive_argument_desc = function
  | Ast_409.Parsetree.Pdir_string x0 -> Ast_410.Parsetree.Pdir_string x0
  | Ast_409.Parsetree.Pdir_int (x0, x1) ->
      Ast_410.Parsetree.Pdir_int (x0, map_option (fun x -> x) x1)
  | Ast_409.Parsetree.Pdir_ident x0 ->
      Ast_410.Parsetree.Pdir_ident (copy_Longident_t x0)
  | Ast_409.Parsetree.Pdir_bool x0 -> Ast_410.Parsetree.Pdir_bool x0

and copy_expression :
    Ast_409.Parsetree.expression -> Ast_410.Parsetree.expression =
 fun {
       Ast_409.Parsetree.pexp_desc;
       Ast_409.Parsetree.pexp_loc;
       Ast_409.Parsetree.pexp_loc_stack;
       Ast_409.Parsetree.pexp_attributes;
     } ->
  {
    Ast_410.Parsetree.pexp_desc = copy_expression_desc pexp_desc;
    Ast_410.Parsetree.pexp_loc = copy_location pexp_loc;
    Ast_410.Parsetree.pexp_loc_stack = List.map copy_location pexp_loc_stack;
    Ast_410.Parsetree.pexp_attributes = copy_attributes pexp_attributes;
  }

and copy_expression_desc :
    Ast_409.Parsetree.expression_desc -> Ast_410.Parsetree.expression_desc =
  function
  | Ast_409.Parsetree.Pexp_ident x0 ->
      Ast_410.Parsetree.Pexp_ident (copy_loc copy_Longident_t x0)
  | Ast_409.Parsetree.Pexp_constant x0 ->
      Ast_410.Parsetree.Pexp_constant (copy_constant x0)
  | Ast_409.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_expression x2)
  | Ast_409.Parsetree.Pexp_function x0 ->
      Ast_410.Parsetree.Pexp_function (copy_cases x0)
  | Ast_409.Parsetree.Pexp_fun (x0, x1, x2, x3) ->
      Ast_410.Parsetree.Pexp_fun
        ( copy_arg_label x0,
          map_option copy_expression x1,
          copy_pattern x2,
          copy_expression x3 )
  | Ast_409.Parsetree.Pexp_apply (x0, x1) ->
      Ast_410.Parsetree.Pexp_apply
        ( copy_expression x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_409.Parsetree.Pexp_match (x0, x1) ->
      Ast_410.Parsetree.Pexp_match (copy_expression x0, copy_cases x1)
  | Ast_409.Parsetree.Pexp_try (x0, x1) ->
      Ast_410.Parsetree.Pexp_try (copy_expression x0, copy_cases x1)
  | Ast_409.Parsetree.Pexp_tuple x0 ->
      Ast_410.Parsetree.Pexp_tuple (List.map copy_expression x0)
  | Ast_409.Parsetree.Pexp_construct (x0, x1) ->
      Ast_410.Parsetree.Pexp_construct
        (copy_loc copy_Longident_t x0, map_option copy_expression x1)
  | Ast_409.Parsetree.Pexp_variant (x0, x1) ->
      Ast_410.Parsetree.Pexp_variant
        (copy_label x0, map_option copy_expression x1)
  | Ast_409.Parsetree.Pexp_record (x0, x1) ->
      Ast_410.Parsetree.Pexp_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_expression x1))
            x0,
          map_option copy_expression x1 )
  | Ast_409.Parsetree.Pexp_field (x0, x1) ->
      Ast_410.Parsetree.Pexp_field
        (copy_expression x0, copy_loc copy_Longident_t x1)
  | Ast_409.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_setfield
        (copy_expression x0, copy_loc copy_Longident_t x1, copy_expression x2)
  | Ast_409.Parsetree.Pexp_array x0 ->
      Ast_410.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_409.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_ifthenelse
        (copy_expression x0, copy_expression x1, map_option copy_expression x2)
  | Ast_409.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_410.Parsetree.Pexp_sequence (copy_expression x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_while (x0, x1) ->
      Ast_410.Parsetree.Pexp_while (copy_expression x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_410.Parsetree.Pexp_for
        ( copy_pattern x0,
          copy_expression x1,
          copy_expression x2,
          copy_direction_flag x3,
          copy_expression x4 )
  | Ast_409.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_410.Parsetree.Pexp_constraint (copy_expression x0, copy_core_type x1)
  | Ast_409.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_coerce
        (copy_expression x0, map_option copy_core_type x1, copy_core_type x2)
  | Ast_409.Parsetree.Pexp_send (x0, x1) ->
      Ast_410.Parsetree.Pexp_send (copy_expression x0, copy_loc copy_label x1)
  | Ast_409.Parsetree.Pexp_new x0 ->
      Ast_410.Parsetree.Pexp_new (copy_loc copy_Longident_t x0)
  | Ast_409.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_410.Parsetree.Pexp_setinstvar
        (copy_loc copy_label x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_override x0 ->
      Ast_410.Parsetree.Pexp_override
        (List.map
           (fun x ->
             let x0, x1 = x in
             (copy_loc copy_label x0, copy_expression x1))
           x0)
  | Ast_409.Parsetree.Pexp_letmodule (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_letmodule
        (copy_loc (fun x -> Some x) x0, copy_module_expr x1, copy_expression x2)
  | Ast_409.Parsetree.Pexp_letexception (x0, x1) ->
      Ast_410.Parsetree.Pexp_letexception
        (copy_extension_constructor x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_assert x0 ->
      Ast_410.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_409.Parsetree.Pexp_lazy x0 ->
      Ast_410.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_409.Parsetree.Pexp_poly (x0, x1) ->
      Ast_410.Parsetree.Pexp_poly
        (copy_expression x0, map_option copy_core_type x1)
  | Ast_409.Parsetree.Pexp_object x0 ->
      Ast_410.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_409.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_410.Parsetree.Pexp_newtype
        (copy_loc (fun x -> x) x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_pack x0 ->
      Ast_410.Parsetree.Pexp_pack (copy_module_expr x0)
  | Ast_409.Parsetree.Pexp_open (x0, x1) ->
      Ast_410.Parsetree.Pexp_open (copy_open_declaration x0, copy_expression x1)
  | Ast_409.Parsetree.Pexp_letop x0 ->
      Ast_410.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_409.Parsetree.Pexp_extension x0 ->
      Ast_410.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_409.Parsetree.Pexp_unreachable -> Ast_410.Parsetree.Pexp_unreachable

and copy_letop : Ast_409.Parsetree.letop -> Ast_410.Parsetree.letop =
 fun { Ast_409.Parsetree.let_; Ast_409.Parsetree.ands; Ast_409.Parsetree.body } ->
  {
    Ast_410.Parsetree.let_ = copy_binding_op let_;
    Ast_410.Parsetree.ands = List.map copy_binding_op ands;
    Ast_410.Parsetree.body = copy_expression body;
  }

and copy_binding_op :
    Ast_409.Parsetree.binding_op -> Ast_410.Parsetree.binding_op =
 fun {
       Ast_409.Parsetree.pbop_op;
       Ast_409.Parsetree.pbop_pat;
       Ast_409.Parsetree.pbop_exp;
       Ast_409.Parsetree.pbop_loc;
     } ->
  {
    Ast_410.Parsetree.pbop_op = copy_loc (fun x -> x) pbop_op;
    Ast_410.Parsetree.pbop_pat = copy_pattern pbop_pat;
    Ast_410.Parsetree.pbop_exp = copy_expression pbop_exp;
    Ast_410.Parsetree.pbop_loc = copy_location pbop_loc;
  }

and copy_direction_flag :
    Ast_409.Asttypes.direction_flag -> Ast_410.Asttypes.direction_flag =
  function
  | Ast_409.Asttypes.Upto -> Ast_410.Asttypes.Upto
  | Ast_409.Asttypes.Downto -> Ast_410.Asttypes.Downto

and copy_cases : Ast_409.Parsetree.cases -> Ast_410.Parsetree.case list =
 fun x -> List.map copy_case x

and copy_case : Ast_409.Parsetree.case -> Ast_410.Parsetree.case =
 fun {
       Ast_409.Parsetree.pc_lhs;
       Ast_409.Parsetree.pc_guard;
       Ast_409.Parsetree.pc_rhs;
     } ->
  {
    Ast_410.Parsetree.pc_lhs = copy_pattern pc_lhs;
    Ast_410.Parsetree.pc_guard = map_option copy_expression pc_guard;
    Ast_410.Parsetree.pc_rhs = copy_expression pc_rhs;
  }

and copy_value_binding :
    Ast_409.Parsetree.value_binding -> Ast_410.Parsetree.value_binding =
 fun {
       Ast_409.Parsetree.pvb_pat;
       Ast_409.Parsetree.pvb_expr;
       Ast_409.Parsetree.pvb_attributes;
       Ast_409.Parsetree.pvb_loc;
     } ->
  {
    Ast_410.Parsetree.pvb_pat = copy_pattern pvb_pat;
    Ast_410.Parsetree.pvb_expr = copy_expression pvb_expr;
    Ast_410.Parsetree.pvb_attributes = copy_attributes pvb_attributes;
    Ast_410.Parsetree.pvb_loc = copy_location pvb_loc;
  }

and copy_pattern : Ast_409.Parsetree.pattern -> Ast_410.Parsetree.pattern =
 fun {
       Ast_409.Parsetree.ppat_desc;
       Ast_409.Parsetree.ppat_loc;
       Ast_409.Parsetree.ppat_loc_stack;
       Ast_409.Parsetree.ppat_attributes;
     } ->
  {
    Ast_410.Parsetree.ppat_desc = copy_pattern_desc ppat_desc;
    Ast_410.Parsetree.ppat_loc = copy_location ppat_loc;
    Ast_410.Parsetree.ppat_loc_stack = List.map copy_location ppat_loc_stack;
    Ast_410.Parsetree.ppat_attributes = copy_attributes ppat_attributes;
  }

and copy_pattern_desc :
    Ast_409.Parsetree.pattern_desc -> Ast_410.Parsetree.pattern_desc = function
  | Ast_409.Parsetree.Ppat_any -> Ast_410.Parsetree.Ppat_any
  | Ast_409.Parsetree.Ppat_var x0 ->
      Ast_410.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_409.Parsetree.Ppat_alias (x0, x1) ->
      Ast_410.Parsetree.Ppat_alias (copy_pattern x0, copy_loc (fun x -> x) x1)
  | Ast_409.Parsetree.Ppat_constant x0 ->
      Ast_410.Parsetree.Ppat_constant (copy_constant x0)
  | Ast_409.Parsetree.Ppat_interval (x0, x1) ->
      Ast_410.Parsetree.Ppat_interval (copy_constant x0, copy_constant x1)
  | Ast_409.Parsetree.Ppat_tuple x0 ->
      Ast_410.Parsetree.Ppat_tuple (List.map copy_pattern x0)
  | Ast_409.Parsetree.Ppat_construct (x0, x1) ->
      Ast_410.Parsetree.Ppat_construct
        (copy_loc copy_Longident_t x0, map_option copy_pattern x1)
  | Ast_409.Parsetree.Ppat_variant (x0, x1) ->
      Ast_410.Parsetree.Ppat_variant (copy_label x0, map_option copy_pattern x1)
  | Ast_409.Parsetree.Ppat_record (x0, x1) ->
      Ast_410.Parsetree.Ppat_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_pattern x1))
            x0,
          copy_closed_flag x1 )
  | Ast_409.Parsetree.Ppat_array x0 ->
      Ast_410.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_409.Parsetree.Ppat_or (x0, x1) ->
      Ast_410.Parsetree.Ppat_or (copy_pattern x0, copy_pattern x1)
  | Ast_409.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_410.Parsetree.Ppat_constraint (copy_pattern x0, copy_core_type x1)
  | Ast_409.Parsetree.Ppat_type x0 ->
      Ast_410.Parsetree.Ppat_type (copy_loc copy_Longident_t x0)
  | Ast_409.Parsetree.Ppat_lazy x0 ->
      Ast_410.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_409.Parsetree.Ppat_unpack x0 ->
      Ast_410.Parsetree.Ppat_unpack (copy_loc (fun x -> Some x) x0)
  | Ast_409.Parsetree.Ppat_exception x0 ->
      Ast_410.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_409.Parsetree.Ppat_extension x0 ->
      Ast_410.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_409.Parsetree.Ppat_open (x0, x1) ->
      Ast_410.Parsetree.Ppat_open (copy_loc copy_Longident_t x0, copy_pattern x1)

and copy_core_type : Ast_409.Parsetree.core_type -> Ast_410.Parsetree.core_type
    =
 fun {
       Ast_409.Parsetree.ptyp_desc;
       Ast_409.Parsetree.ptyp_loc;
       Ast_409.Parsetree.ptyp_loc_stack;
       Ast_409.Parsetree.ptyp_attributes;
     } ->
  {
    Ast_410.Parsetree.ptyp_desc = copy_core_type_desc ptyp_desc;
    Ast_410.Parsetree.ptyp_loc = copy_location ptyp_loc;
    Ast_410.Parsetree.ptyp_loc_stack = List.map copy_location ptyp_loc_stack;
    Ast_410.Parsetree.ptyp_attributes = copy_attributes ptyp_attributes;
  }

and copy_core_type_desc :
    Ast_409.Parsetree.core_type_desc -> Ast_410.Parsetree.core_type_desc =
  function
  | Ast_409.Parsetree.Ptyp_any -> Ast_410.Parsetree.Ptyp_any
  | Ast_409.Parsetree.Ptyp_var x0 -> Ast_410.Parsetree.Ptyp_var x0
  | Ast_409.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_410.Parsetree.Ptyp_arrow
        (copy_arg_label x0, copy_core_type x1, copy_core_type x2)
  | Ast_409.Parsetree.Ptyp_tuple x0 ->
      Ast_410.Parsetree.Ptyp_tuple (List.map copy_core_type x0)
  | Ast_409.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_410.Parsetree.Ptyp_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_409.Parsetree.Ptyp_object (x0, x1) ->
      Ast_410.Parsetree.Ptyp_object
        (List.map copy_object_field x0, copy_closed_flag x1)
  | Ast_409.Parsetree.Ptyp_class (x0, x1) ->
      Ast_410.Parsetree.Ptyp_class
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_409.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_410.Parsetree.Ptyp_alias (copy_core_type x0, x1)
  | Ast_409.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_410.Parsetree.Ptyp_variant
        ( List.map copy_row_field x0,
          copy_closed_flag x1,
          map_option (fun x -> List.map copy_label x) x2 )
  | Ast_409.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_410.Parsetree.Ptyp_poly
        (List.map (fun x -> copy_loc (fun x -> x) x) x0, copy_core_type x1)
  | Ast_409.Parsetree.Ptyp_package x0 ->
      Ast_410.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_409.Parsetree.Ptyp_extension x0 ->
      Ast_410.Parsetree.Ptyp_extension (copy_extension x0)

and copy_package_type :
    Ast_409.Parsetree.package_type -> Ast_410.Parsetree.package_type =
 fun x ->
  let x0, x1 = x in
  ( copy_loc copy_Longident_t x0,
    List.map
      (fun x ->
        let x0, x1 = x in
        (copy_loc copy_Longident_t x0, copy_core_type x1))
      x1 )

and copy_row_field : Ast_409.Parsetree.row_field -> Ast_410.Parsetree.row_field
    =
 fun {
       Ast_409.Parsetree.prf_desc;
       Ast_409.Parsetree.prf_loc;
       Ast_409.Parsetree.prf_attributes;
     } ->
  {
    Ast_410.Parsetree.prf_desc = copy_row_field_desc prf_desc;
    Ast_410.Parsetree.prf_loc = copy_location prf_loc;
    Ast_410.Parsetree.prf_attributes = copy_attributes prf_attributes;
  }

and copy_row_field_desc :
    Ast_409.Parsetree.row_field_desc -> Ast_410.Parsetree.row_field_desc =
  function
  | Ast_409.Parsetree.Rtag (x0, x1, x2) ->
      Ast_410.Parsetree.Rtag
        (copy_loc copy_label x0, x1, List.map copy_core_type x2)
  | Ast_409.Parsetree.Rinherit x0 ->
      Ast_410.Parsetree.Rinherit (copy_core_type x0)

and copy_object_field :
    Ast_409.Parsetree.object_field -> Ast_410.Parsetree.object_field =
 fun {
       Ast_409.Parsetree.pof_desc;
       Ast_409.Parsetree.pof_loc;
       Ast_409.Parsetree.pof_attributes;
     } ->
  {
    Ast_410.Parsetree.pof_desc = copy_object_field_desc pof_desc;
    Ast_410.Parsetree.pof_loc = copy_location pof_loc;
    Ast_410.Parsetree.pof_attributes = copy_attributes pof_attributes;
  }

and copy_attributes :
    Ast_409.Parsetree.attributes -> Ast_410.Parsetree.attributes =
 fun x -> List.map copy_attribute x

and copy_attribute : Ast_409.Parsetree.attribute -> Ast_410.Parsetree.attribute
    =
 fun {
       Ast_409.Parsetree.attr_name;
       Ast_409.Parsetree.attr_payload;
       Ast_409.Parsetree.attr_loc;
     } ->
  {
    Ast_410.Parsetree.attr_name = copy_loc (fun x -> x) attr_name;
    Ast_410.Parsetree.attr_payload = copy_payload attr_payload;
    Ast_410.Parsetree.attr_loc = copy_location attr_loc;
  }

and copy_payload : Ast_409.Parsetree.payload -> Ast_410.Parsetree.payload =
  function
  | Ast_409.Parsetree.PStr x0 -> Ast_410.Parsetree.PStr (copy_structure x0)
  | Ast_409.Parsetree.PSig x0 -> Ast_410.Parsetree.PSig (copy_signature x0)
  | Ast_409.Parsetree.PTyp x0 -> Ast_410.Parsetree.PTyp (copy_core_type x0)
  | Ast_409.Parsetree.PPat (x0, x1) ->
      Ast_410.Parsetree.PPat (copy_pattern x0, map_option copy_expression x1)

and copy_structure : Ast_409.Parsetree.structure -> Ast_410.Parsetree.structure
    =
 fun x -> List.map copy_structure_item x

and copy_structure_item :
    Ast_409.Parsetree.structure_item -> Ast_410.Parsetree.structure_item =
 fun { Ast_409.Parsetree.pstr_desc; Ast_409.Parsetree.pstr_loc } ->
  {
    Ast_410.Parsetree.pstr_desc = copy_structure_item_desc pstr_desc;
    Ast_410.Parsetree.pstr_loc = copy_location pstr_loc;
  }

and copy_structure_item_desc :
    Ast_409.Parsetree.structure_item_desc ->
    Ast_410.Parsetree.structure_item_desc = function
  | Ast_409.Parsetree.Pstr_eval (x0, x1) ->
      Ast_410.Parsetree.Pstr_eval (copy_expression x0, copy_attributes x1)
  | Ast_409.Parsetree.Pstr_value (x0, x1) ->
      Ast_410.Parsetree.Pstr_value
        (copy_rec_flag x0, List.map copy_value_binding x1)
  | Ast_409.Parsetree.Pstr_primitive x0 ->
      Ast_410.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_409.Parsetree.Pstr_type (x0, x1) ->
      Ast_410.Parsetree.Pstr_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_409.Parsetree.Pstr_typext x0 ->
      Ast_410.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_409.Parsetree.Pstr_exception x0 ->
      Ast_410.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_409.Parsetree.Pstr_module x0 ->
      Ast_410.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_409.Parsetree.Pstr_recmodule x0 ->
      Ast_410.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_409.Parsetree.Pstr_modtype x0 ->
      Ast_410.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_409.Parsetree.Pstr_open x0 ->
      Ast_410.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_409.Parsetree.Pstr_class x0 ->
      Ast_410.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_409.Parsetree.Pstr_class_type x0 ->
      Ast_410.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_409.Parsetree.Pstr_include x0 ->
      Ast_410.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_409.Parsetree.Pstr_attribute x0 ->
      Ast_410.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_409.Parsetree.Pstr_extension (x0, x1) ->
      Ast_410.Parsetree.Pstr_extension (copy_extension x0, copy_attributes x1)

and copy_include_declaration :
    Ast_409.Parsetree.include_declaration ->
    Ast_410.Parsetree.include_declaration =
 fun x -> copy_include_infos copy_module_expr x

and copy_class_declaration :
    Ast_409.Parsetree.class_declaration -> Ast_410.Parsetree.class_declaration =
 fun x -> copy_class_infos copy_class_expr x

and copy_class_expr :
    Ast_409.Parsetree.class_expr -> Ast_410.Parsetree.class_expr =
 fun {
       Ast_409.Parsetree.pcl_desc;
       Ast_409.Parsetree.pcl_loc;
       Ast_409.Parsetree.pcl_attributes;
     } ->
  {
    Ast_410.Parsetree.pcl_desc = copy_class_expr_desc pcl_desc;
    Ast_410.Parsetree.pcl_loc = copy_location pcl_loc;
    Ast_410.Parsetree.pcl_attributes = copy_attributes pcl_attributes;
  }

and copy_class_expr_desc :
    Ast_409.Parsetree.class_expr_desc -> Ast_410.Parsetree.class_expr_desc =
  function
  | Ast_409.Parsetree.Pcl_constr (x0, x1) ->
      Ast_410.Parsetree.Pcl_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_409.Parsetree.Pcl_structure x0 ->
      Ast_410.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_409.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_410.Parsetree.Pcl_fun
        ( copy_arg_label x0,
          map_option copy_expression x1,
          copy_pattern x2,
          copy_class_expr x3 )
  | Ast_409.Parsetree.Pcl_apply (x0, x1) ->
      Ast_410.Parsetree.Pcl_apply
        ( copy_class_expr x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_409.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_410.Parsetree.Pcl_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_class_expr x2)
  | Ast_409.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_410.Parsetree.Pcl_constraint (copy_class_expr x0, copy_class_type x1)
  | Ast_409.Parsetree.Pcl_extension x0 ->
      Ast_410.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_409.Parsetree.Pcl_open (x0, x1) ->
      Ast_410.Parsetree.Pcl_open (copy_open_description x0, copy_class_expr x1)

and copy_class_structure :
    Ast_409.Parsetree.class_structure -> Ast_410.Parsetree.class_structure =
 fun { Ast_409.Parsetree.pcstr_self; Ast_409.Parsetree.pcstr_fields } ->
  {
    Ast_410.Parsetree.pcstr_self = copy_pattern pcstr_self;
    Ast_410.Parsetree.pcstr_fields = List.map copy_class_field pcstr_fields;
  }

and copy_class_field :
    Ast_409.Parsetree.class_field -> Ast_410.Parsetree.class_field =
 fun {
       Ast_409.Parsetree.pcf_desc;
       Ast_409.Parsetree.pcf_loc;
       Ast_409.Parsetree.pcf_attributes;
     } ->
  {
    Ast_410.Parsetree.pcf_desc = copy_class_field_desc pcf_desc;
    Ast_410.Parsetree.pcf_loc = copy_location pcf_loc;
    Ast_410.Parsetree.pcf_attributes = copy_attributes pcf_attributes;
  }

and copy_class_field_desc :
    Ast_409.Parsetree.class_field_desc -> Ast_410.Parsetree.class_field_desc =
  function
  | Ast_409.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_410.Parsetree.Pcf_inherit
        ( copy_override_flag x0,
          copy_class_expr x1,
          map_option (fun x -> copy_loc (fun x -> x) x) x2 )
  | Ast_409.Parsetree.Pcf_val x0 ->
      Ast_410.Parsetree.Pcf_val
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_mutable_flag x1, copy_class_field_kind x2))
  | Ast_409.Parsetree.Pcf_method x0 ->
      Ast_410.Parsetree.Pcf_method
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_private_flag x1, copy_class_field_kind x2))
  | Ast_409.Parsetree.Pcf_constraint x0 ->
      Ast_410.Parsetree.Pcf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_409.Parsetree.Pcf_initializer x0 ->
      Ast_410.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_409.Parsetree.Pcf_attribute x0 ->
      Ast_410.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_409.Parsetree.Pcf_extension x0 ->
      Ast_410.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
    Ast_409.Parsetree.class_field_kind -> Ast_410.Parsetree.class_field_kind =
  function
  | Ast_409.Parsetree.Cfk_virtual x0 ->
      Ast_410.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_409.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_410.Parsetree.Cfk_concrete (copy_override_flag x0, copy_expression x1)

and copy_open_declaration :
    Ast_409.Parsetree.open_declaration -> Ast_410.Parsetree.open_declaration =
 fun x -> copy_open_infos copy_module_expr x

and copy_module_binding :
    Ast_409.Parsetree.module_binding -> Ast_410.Parsetree.module_binding =
 fun {
       Ast_409.Parsetree.pmb_name;
       Ast_409.Parsetree.pmb_expr;
       Ast_409.Parsetree.pmb_attributes;
       Ast_409.Parsetree.pmb_loc;
     } ->
  {
    Ast_410.Parsetree.pmb_name = copy_loc (fun x -> Some x) pmb_name;
    Ast_410.Parsetree.pmb_expr = copy_module_expr pmb_expr;
    Ast_410.Parsetree.pmb_attributes = copy_attributes pmb_attributes;
    Ast_410.Parsetree.pmb_loc = copy_location pmb_loc;
  }

and copy_module_expr :
    Ast_409.Parsetree.module_expr -> Ast_410.Parsetree.module_expr =
 fun {
       Ast_409.Parsetree.pmod_desc;
       Ast_409.Parsetree.pmod_loc;
       Ast_409.Parsetree.pmod_attributes;
     } ->
  {
    Ast_410.Parsetree.pmod_desc = copy_module_expr_desc pmod_desc;
    Ast_410.Parsetree.pmod_loc = copy_location pmod_loc;
    Ast_410.Parsetree.pmod_attributes = copy_attributes pmod_attributes;
  }

and copy_module_expr_desc :
    Ast_409.Parsetree.module_expr_desc -> Ast_410.Parsetree.module_expr_desc =
  function
  | Ast_409.Parsetree.Pmod_ident x0 ->
      Ast_410.Parsetree.Pmod_ident (copy_loc copy_Longident_t x0)
  | Ast_409.Parsetree.Pmod_structure x0 ->
      Ast_410.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_409.Parsetree.Pmod_functor (x0, x1, x2) ->
      Ast_410.Parsetree.Pmod_functor
        ( (match (x0.txt, x1) with
          | "*", None -> Unit
          | "_", Some mt ->
              Named (copy_loc (fun _ -> None) x0, copy_module_type mt)
          | _, Some mt ->
              Named (copy_loc (fun x -> Some x) x0, copy_module_type mt)
          | _ -> assert false),
          copy_module_expr x2 )
  | Ast_409.Parsetree.Pmod_apply (x0, x1) ->
      Ast_410.Parsetree.Pmod_apply (copy_module_expr x0, copy_module_expr x1)
  | Ast_409.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_410.Parsetree.Pmod_constraint
        (copy_module_expr x0, copy_module_type x1)
  | Ast_409.Parsetree.Pmod_unpack x0 ->
      Ast_410.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_409.Parsetree.Pmod_extension x0 ->
      Ast_410.Parsetree.Pmod_extension (copy_extension x0)

and copy_module_type :
    Ast_409.Parsetree.module_type -> Ast_410.Parsetree.module_type =
 fun {
       Ast_409.Parsetree.pmty_desc;
       Ast_409.Parsetree.pmty_loc;
       Ast_409.Parsetree.pmty_attributes;
     } ->
  {
    Ast_410.Parsetree.pmty_desc = copy_module_type_desc pmty_desc;
    Ast_410.Parsetree.pmty_loc = copy_location pmty_loc;
    Ast_410.Parsetree.pmty_attributes = copy_attributes pmty_attributes;
  }

and copy_module_type_desc :
    Ast_409.Parsetree.module_type_desc -> Ast_410.Parsetree.module_type_desc =
  function
  | Ast_409.Parsetree.Pmty_ident x0 ->
      Ast_410.Parsetree.Pmty_ident (copy_loc copy_Longident_t x0)
  | Ast_409.Parsetree.Pmty_signature x0 ->
      Ast_410.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_409.Parsetree.Pmty_functor (x0, x1, x2) ->
      Ast_410.Parsetree.Pmty_functor
        ( (match (x0.txt, x1) with
          | "*", None -> Unit
          | "_", Some mt ->
              Named (copy_loc (fun _ -> None) x0, copy_module_type mt)
          | _, Some mt ->
              Named (copy_loc (fun x -> Some x) x0, copy_module_type mt)
          | _ -> assert false),
          copy_module_type x2 )
  | Ast_409.Parsetree.Pmty_with (x0, x1) ->
      Ast_410.Parsetree.Pmty_with
        (copy_module_type x0, List.map copy_with_constraint x1)
  | Ast_409.Parsetree.Pmty_typeof x0 ->
      Ast_410.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_409.Parsetree.Pmty_extension x0 ->
      Ast_410.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_409.Parsetree.Pmty_alias x0 ->
      Ast_410.Parsetree.Pmty_alias (copy_loc copy_Longident_t x0)

and copy_with_constraint :
    Ast_409.Parsetree.with_constraint -> Ast_410.Parsetree.with_constraint =
  function
  | Ast_409.Parsetree.Pwith_type (x0, x1) ->
      Ast_410.Parsetree.Pwith_type
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_409.Parsetree.Pwith_module (x0, x1) ->
      Ast_410.Parsetree.Pwith_module
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)
  | Ast_409.Parsetree.Pwith_typesubst (x0, x1) ->
      Ast_410.Parsetree.Pwith_typesubst
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_409.Parsetree.Pwith_modsubst (x0, x1) ->
      Ast_410.Parsetree.Pwith_modsubst
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)

and copy_signature : Ast_409.Parsetree.signature -> Ast_410.Parsetree.signature
    =
 fun x -> List.map copy_signature_item x

and copy_signature_item :
    Ast_409.Parsetree.signature_item -> Ast_410.Parsetree.signature_item =
 fun { Ast_409.Parsetree.psig_desc; Ast_409.Parsetree.psig_loc } ->
  {
    Ast_410.Parsetree.psig_desc = copy_signature_item_desc psig_desc;
    Ast_410.Parsetree.psig_loc = copy_location psig_loc;
  }

and copy_signature_item_desc :
    Ast_409.Parsetree.signature_item_desc ->
    Ast_410.Parsetree.signature_item_desc = function
  | Ast_409.Parsetree.Psig_value x0 ->
      Ast_410.Parsetree.Psig_value (copy_value_description x0)
  | Ast_409.Parsetree.Psig_type (x0, x1) ->
      Ast_410.Parsetree.Psig_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_409.Parsetree.Psig_typesubst x0 ->
      Ast_410.Parsetree.Psig_typesubst (List.map copy_type_declaration x0)
  | Ast_409.Parsetree.Psig_typext x0 ->
      Ast_410.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_409.Parsetree.Psig_exception x0 ->
      Ast_410.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_409.Parsetree.Psig_module x0 ->
      Ast_410.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_409.Parsetree.Psig_modsubst x0 ->
      Ast_410.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_409.Parsetree.Psig_recmodule x0 ->
      Ast_410.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_409.Parsetree.Psig_modtype x0 ->
      Ast_410.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_409.Parsetree.Psig_open x0 ->
      Ast_410.Parsetree.Psig_open (copy_open_description x0)
  | Ast_409.Parsetree.Psig_include x0 ->
      Ast_410.Parsetree.Psig_include (copy_include_description x0)
  | Ast_409.Parsetree.Psig_class x0 ->
      Ast_410.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_409.Parsetree.Psig_class_type x0 ->
      Ast_410.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_409.Parsetree.Psig_attribute x0 ->
      Ast_410.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_409.Parsetree.Psig_extension (x0, x1) ->
      Ast_410.Parsetree.Psig_extension (copy_extension x0, copy_attributes x1)

and copy_class_type_declaration :
    Ast_409.Parsetree.class_type_declaration ->
    Ast_410.Parsetree.class_type_declaration =
 fun x -> copy_class_infos copy_class_type x

and copy_class_description :
    Ast_409.Parsetree.class_description -> Ast_410.Parsetree.class_description =
 fun x -> copy_class_infos copy_class_type x

and copy_class_type :
    Ast_409.Parsetree.class_type -> Ast_410.Parsetree.class_type =
 fun {
       Ast_409.Parsetree.pcty_desc;
       Ast_409.Parsetree.pcty_loc;
       Ast_409.Parsetree.pcty_attributes;
     } ->
  {
    Ast_410.Parsetree.pcty_desc = copy_class_type_desc pcty_desc;
    Ast_410.Parsetree.pcty_loc = copy_location pcty_loc;
    Ast_410.Parsetree.pcty_attributes = copy_attributes pcty_attributes;
  }

and copy_class_type_desc :
    Ast_409.Parsetree.class_type_desc -> Ast_410.Parsetree.class_type_desc =
  function
  | Ast_409.Parsetree.Pcty_constr (x0, x1) ->
      Ast_410.Parsetree.Pcty_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_409.Parsetree.Pcty_signature x0 ->
      Ast_410.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_409.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_410.Parsetree.Pcty_arrow
        (copy_arg_label x0, copy_core_type x1, copy_class_type x2)
  | Ast_409.Parsetree.Pcty_extension x0 ->
      Ast_410.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_409.Parsetree.Pcty_open (x0, x1) ->
      Ast_410.Parsetree.Pcty_open (copy_open_description x0, copy_class_type x1)

and copy_class_signature :
    Ast_409.Parsetree.class_signature -> Ast_410.Parsetree.class_signature =
 fun { Ast_409.Parsetree.pcsig_self; Ast_409.Parsetree.pcsig_fields } ->
  {
    Ast_410.Parsetree.pcsig_self = copy_core_type pcsig_self;
    Ast_410.Parsetree.pcsig_fields = List.map copy_class_type_field pcsig_fields;
  }

and copy_class_type_field :
    Ast_409.Parsetree.class_type_field -> Ast_410.Parsetree.class_type_field =
 fun {
       Ast_409.Parsetree.pctf_desc;
       Ast_409.Parsetree.pctf_loc;
       Ast_409.Parsetree.pctf_attributes;
     } ->
  {
    Ast_410.Parsetree.pctf_desc = copy_class_type_field_desc pctf_desc;
    Ast_410.Parsetree.pctf_loc = copy_location pctf_loc;
    Ast_410.Parsetree.pctf_attributes = copy_attributes pctf_attributes;
  }

and copy_class_type_field_desc :
    Ast_409.Parsetree.class_type_field_desc ->
    Ast_410.Parsetree.class_type_field_desc = function
  | Ast_409.Parsetree.Pctf_inherit x0 ->
      Ast_410.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_409.Parsetree.Pctf_val x0 ->
      Ast_410.Parsetree.Pctf_val
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_mutable_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_409.Parsetree.Pctf_method x0 ->
      Ast_410.Parsetree.Pctf_method
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_private_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_409.Parsetree.Pctf_constraint x0 ->
      Ast_410.Parsetree.Pctf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_409.Parsetree.Pctf_attribute x0 ->
      Ast_410.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_409.Parsetree.Pctf_extension x0 ->
      Ast_410.Parsetree.Pctf_extension (copy_extension x0)

and copy_extension : Ast_409.Parsetree.extension -> Ast_410.Parsetree.extension
    =
 fun x ->
  let x0, x1 = x in
  (copy_loc (fun x -> x) x0, copy_payload x1)

and copy_class_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_409.Parsetree.class_infos ->
      'g0 Ast_410.Parsetree.class_infos =
 fun f0
     {
       Ast_409.Parsetree.pci_virt;
       Ast_409.Parsetree.pci_params;
       Ast_409.Parsetree.pci_name;
       Ast_409.Parsetree.pci_expr;
       Ast_409.Parsetree.pci_loc;
       Ast_409.Parsetree.pci_attributes;
     } ->
  {
    Ast_410.Parsetree.pci_virt = copy_virtual_flag pci_virt;
    Ast_410.Parsetree.pci_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          (copy_core_type x0, copy_variance x1))
        pci_params;
    Ast_410.Parsetree.pci_name = copy_loc (fun x -> x) pci_name;
    Ast_410.Parsetree.pci_expr = f0 pci_expr;
    Ast_410.Parsetree.pci_loc = copy_location pci_loc;
    Ast_410.Parsetree.pci_attributes = copy_attributes pci_attributes;
  }

and copy_virtual_flag :
    Ast_409.Asttypes.virtual_flag -> Ast_410.Asttypes.virtual_flag = function
  | Ast_409.Asttypes.Virtual -> Ast_410.Asttypes.Virtual
  | Ast_409.Asttypes.Concrete -> Ast_410.Asttypes.Concrete

and copy_include_description :
    Ast_409.Parsetree.include_description ->
    Ast_410.Parsetree.include_description =
 fun x -> copy_include_infos copy_module_type x

and copy_include_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_409.Parsetree.include_infos ->
      'g0 Ast_410.Parsetree.include_infos =
 fun f0
     {
       Ast_409.Parsetree.pincl_mod;
       Ast_409.Parsetree.pincl_loc;
       Ast_409.Parsetree.pincl_attributes;
     } ->
  {
    Ast_410.Parsetree.pincl_mod = f0 pincl_mod;
    Ast_410.Parsetree.pincl_loc = copy_location pincl_loc;
    Ast_410.Parsetree.pincl_attributes = copy_attributes pincl_attributes;
  }

and copy_open_description :
    Ast_409.Parsetree.open_description -> Ast_410.Parsetree.open_description =
 fun x -> copy_open_infos (fun x -> copy_loc copy_Longident_t x) x

and copy_open_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_409.Parsetree.open_infos ->
      'g0 Ast_410.Parsetree.open_infos =
 fun f0
     {
       Ast_409.Parsetree.popen_expr;
       Ast_409.Parsetree.popen_override;
       Ast_409.Parsetree.popen_loc;
       Ast_409.Parsetree.popen_attributes;
     } ->
  {
    Ast_410.Parsetree.popen_expr = f0 popen_expr;
    Ast_410.Parsetree.popen_override = copy_override_flag popen_override;
    Ast_410.Parsetree.popen_loc = copy_location popen_loc;
    Ast_410.Parsetree.popen_attributes = copy_attributes popen_attributes;
  }

and copy_override_flag :
    Ast_409.Asttypes.override_flag -> Ast_410.Asttypes.override_flag = function
  | Ast_409.Asttypes.Override -> Ast_410.Asttypes.Override
  | Ast_409.Asttypes.Fresh -> Ast_410.Asttypes.Fresh

and copy_module_type_declaration :
    Ast_409.Parsetree.module_type_declaration ->
    Ast_410.Parsetree.module_type_declaration =
 fun {
       Ast_409.Parsetree.pmtd_name;
       Ast_409.Parsetree.pmtd_type;
       Ast_409.Parsetree.pmtd_attributes;
       Ast_409.Parsetree.pmtd_loc;
     } ->
  {
    Ast_410.Parsetree.pmtd_name = copy_loc (fun x -> x) pmtd_name;
    Ast_410.Parsetree.pmtd_type = map_option copy_module_type pmtd_type;
    Ast_410.Parsetree.pmtd_attributes = copy_attributes pmtd_attributes;
    Ast_410.Parsetree.pmtd_loc = copy_location pmtd_loc;
  }

and copy_module_substitution :
    Ast_409.Parsetree.module_substitution ->
    Ast_410.Parsetree.module_substitution =
 fun {
       Ast_409.Parsetree.pms_name;
       Ast_409.Parsetree.pms_manifest;
       Ast_409.Parsetree.pms_attributes;
       Ast_409.Parsetree.pms_loc;
     } ->
  {
    Ast_410.Parsetree.pms_name = copy_loc (fun x -> x) pms_name;
    Ast_410.Parsetree.pms_manifest = copy_loc copy_Longident_t pms_manifest;
    Ast_410.Parsetree.pms_attributes = copy_attributes pms_attributes;
    Ast_410.Parsetree.pms_loc = copy_location pms_loc;
  }

and copy_module_declaration :
    Ast_409.Parsetree.module_declaration -> Ast_410.Parsetree.module_declaration
    =
 fun {
       Ast_409.Parsetree.pmd_name;
       Ast_409.Parsetree.pmd_type;
       Ast_409.Parsetree.pmd_attributes;
       Ast_409.Parsetree.pmd_loc;
     } ->
  {
    Ast_410.Parsetree.pmd_name = copy_loc (fun x -> Some x) pmd_name;
    Ast_410.Parsetree.pmd_type = copy_module_type pmd_type;
    Ast_410.Parsetree.pmd_attributes = copy_attributes pmd_attributes;
    Ast_410.Parsetree.pmd_loc = copy_location pmd_loc;
  }

and copy_type_exception :
    Ast_409.Parsetree.type_exception -> Ast_410.Parsetree.type_exception =
 fun {
       Ast_409.Parsetree.ptyexn_constructor;
       Ast_409.Parsetree.ptyexn_loc;
       Ast_409.Parsetree.ptyexn_attributes;
     } ->
  {
    Ast_410.Parsetree.ptyexn_constructor =
      copy_extension_constructor ptyexn_constructor;
    Ast_410.Parsetree.ptyexn_loc = copy_location ptyexn_loc;
    Ast_410.Parsetree.ptyexn_attributes = copy_attributes ptyexn_attributes;
  }

and copy_type_extension :
    Ast_409.Parsetree.type_extension -> Ast_410.Parsetree.type_extension =
 fun {
       Ast_409.Parsetree.ptyext_path;
       Ast_409.Parsetree.ptyext_params;
       Ast_409.Parsetree.ptyext_constructors;
       Ast_409.Parsetree.ptyext_private;
       Ast_409.Parsetree.ptyext_loc;
       Ast_409.Parsetree.ptyext_attributes;
     } ->
  {
    Ast_410.Parsetree.ptyext_path = copy_loc copy_Longident_t ptyext_path;
    Ast_410.Parsetree.ptyext_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          (copy_core_type x0, copy_variance x1))
        ptyext_params;
    Ast_410.Parsetree.ptyext_constructors =
      List.map copy_extension_constructor ptyext_constructors;
    Ast_410.Parsetree.ptyext_private = copy_private_flag ptyext_private;
    Ast_410.Parsetree.ptyext_loc = copy_location ptyext_loc;
    Ast_410.Parsetree.ptyext_attributes = copy_attributes ptyext_attributes;
  }

and copy_extension_constructor :
    Ast_409.Parsetree.extension_constructor ->
    Ast_410.Parsetree.extension_constructor =
 fun {
       Ast_409.Parsetree.pext_name;
       Ast_409.Parsetree.pext_kind;
       Ast_409.Parsetree.pext_loc;
       Ast_409.Parsetree.pext_attributes;
     } ->
  {
    Ast_410.Parsetree.pext_name = copy_loc (fun x -> x) pext_name;
    Ast_410.Parsetree.pext_kind = copy_extension_constructor_kind pext_kind;
    Ast_410.Parsetree.pext_loc = copy_location pext_loc;
    Ast_410.Parsetree.pext_attributes = copy_attributes pext_attributes;
  }

and copy_extension_constructor_kind :
    Ast_409.Parsetree.extension_constructor_kind ->
    Ast_410.Parsetree.extension_constructor_kind = function
  | Ast_409.Parsetree.Pext_decl (x0, x1) ->
      Ast_410.Parsetree.Pext_decl
        (copy_constructor_arguments x0, map_option copy_core_type x1)
  | Ast_409.Parsetree.Pext_rebind x0 ->
      Ast_410.Parsetree.Pext_rebind (copy_loc copy_Longident_t x0)

and copy_type_declaration :
    Ast_409.Parsetree.type_declaration -> Ast_410.Parsetree.type_declaration =
 fun {
       Ast_409.Parsetree.ptype_name;
       Ast_409.Parsetree.ptype_params;
       Ast_409.Parsetree.ptype_cstrs;
       Ast_409.Parsetree.ptype_kind;
       Ast_409.Parsetree.ptype_private;
       Ast_409.Parsetree.ptype_manifest;
       Ast_409.Parsetree.ptype_attributes;
       Ast_409.Parsetree.ptype_loc;
     } ->
  {
    Ast_410.Parsetree.ptype_name = copy_loc (fun x -> x) ptype_name;
    Ast_410.Parsetree.ptype_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          (copy_core_type x0, copy_variance x1))
        ptype_params;
    Ast_410.Parsetree.ptype_cstrs =
      List.map
        (fun x ->
          let x0, x1, x2 = x in
          (copy_core_type x0, copy_core_type x1, copy_location x2))
        ptype_cstrs;
    Ast_410.Parsetree.ptype_kind = copy_type_kind ptype_kind;
    Ast_410.Parsetree.ptype_private = copy_private_flag ptype_private;
    Ast_410.Parsetree.ptype_manifest = map_option copy_core_type ptype_manifest;
    Ast_410.Parsetree.ptype_attributes = copy_attributes ptype_attributes;
    Ast_410.Parsetree.ptype_loc = copy_location ptype_loc;
  }

and copy_private_flag :
    Ast_409.Asttypes.private_flag -> Ast_410.Asttypes.private_flag = function
  | Ast_409.Asttypes.Private -> Ast_410.Asttypes.Private
  | Ast_409.Asttypes.Public -> Ast_410.Asttypes.Public

and copy_type_kind : Ast_409.Parsetree.type_kind -> Ast_410.Parsetree.type_kind
    = function
  | Ast_409.Parsetree.Ptype_abstract -> Ast_410.Parsetree.Ptype_abstract
  | Ast_409.Parsetree.Ptype_variant x0 ->
      Ast_410.Parsetree.Ptype_variant (List.map copy_constructor_declaration x0)
  | Ast_409.Parsetree.Ptype_record x0 ->
      Ast_410.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_409.Parsetree.Ptype_open -> Ast_410.Parsetree.Ptype_open

and copy_constructor_declaration :
    Ast_409.Parsetree.constructor_declaration ->
    Ast_410.Parsetree.constructor_declaration =
 fun {
       Ast_409.Parsetree.pcd_name;
       Ast_409.Parsetree.pcd_args;
       Ast_409.Parsetree.pcd_res;
       Ast_409.Parsetree.pcd_loc;
       Ast_409.Parsetree.pcd_attributes;
     } ->
  {
    Ast_410.Parsetree.pcd_name = copy_loc (fun x -> x) pcd_name;
    Ast_410.Parsetree.pcd_args = copy_constructor_arguments pcd_args;
    Ast_410.Parsetree.pcd_res = map_option copy_core_type pcd_res;
    Ast_410.Parsetree.pcd_loc = copy_location pcd_loc;
    Ast_410.Parsetree.pcd_attributes = copy_attributes pcd_attributes;
  }

and copy_constructor_arguments :
    Ast_409.Parsetree.constructor_arguments ->
    Ast_410.Parsetree.constructor_arguments = function
  | Ast_409.Parsetree.Pcstr_tuple x0 ->
      Ast_410.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_409.Parsetree.Pcstr_record x0 ->
      Ast_410.Parsetree.Pcstr_record (List.map copy_label_declaration x0)

and copy_label_declaration :
    Ast_409.Parsetree.label_declaration -> Ast_410.Parsetree.label_declaration =
 fun {
       Ast_409.Parsetree.pld_name;
       Ast_409.Parsetree.pld_mutable;
       Ast_409.Parsetree.pld_type;
       Ast_409.Parsetree.pld_loc;
       Ast_409.Parsetree.pld_attributes;
     } ->
  {
    Ast_410.Parsetree.pld_name = copy_loc (fun x -> x) pld_name;
    Ast_410.Parsetree.pld_mutable = copy_mutable_flag pld_mutable;
    Ast_410.Parsetree.pld_type = copy_core_type pld_type;
    Ast_410.Parsetree.pld_loc = copy_location pld_loc;
    Ast_410.Parsetree.pld_attributes = copy_attributes pld_attributes;
  }

and copy_mutable_flag :
    Ast_409.Asttypes.mutable_flag -> Ast_410.Asttypes.mutable_flag = function
  | Ast_409.Asttypes.Immutable -> Ast_410.Asttypes.Immutable
  | Ast_409.Asttypes.Mutable -> Ast_410.Asttypes.Mutable

and copy_variance : Ast_409.Asttypes.variance -> Ast_410.Asttypes.variance =
  function
  | Ast_409.Asttypes.Covariant -> Ast_410.Asttypes.Covariant
  | Ast_409.Asttypes.Contravariant -> Ast_410.Asttypes.Contravariant
  | Ast_409.Asttypes.Invariant -> Ast_410.Asttypes.Invariant

and copy_value_description :
    Ast_409.Parsetree.value_description -> Ast_410.Parsetree.value_description =
 fun {
       Ast_409.Parsetree.pval_name;
       Ast_409.Parsetree.pval_type;
       Ast_409.Parsetree.pval_prim;
       Ast_409.Parsetree.pval_attributes;
       Ast_409.Parsetree.pval_loc;
     } ->
  {
    Ast_410.Parsetree.pval_name = copy_loc (fun x -> x) pval_name;
    Ast_410.Parsetree.pval_type = copy_core_type pval_type;
    Ast_410.Parsetree.pval_prim = List.map (fun x -> x) pval_prim;
    Ast_410.Parsetree.pval_attributes = copy_attributes pval_attributes;
    Ast_410.Parsetree.pval_loc = copy_location pval_loc;
  }

and copy_object_field_desc :
    Ast_409.Parsetree.object_field_desc -> Ast_410.Parsetree.object_field_desc =
  function
  | Ast_409.Parsetree.Otag (x0, x1) ->
      Ast_410.Parsetree.Otag (copy_loc copy_label x0, copy_core_type x1)
  | Ast_409.Parsetree.Oinherit x0 ->
      Ast_410.Parsetree.Oinherit (copy_core_type x0)

and copy_arg_label : Ast_409.Asttypes.arg_label -> Ast_410.Asttypes.arg_label =
  function
  | Ast_409.Asttypes.Nolabel -> Ast_410.Asttypes.Nolabel
  | Ast_409.Asttypes.Labelled x0 -> Ast_410.Asttypes.Labelled x0
  | Ast_409.Asttypes.Optional x0 -> Ast_410.Asttypes.Optional x0

and copy_closed_flag :
    Ast_409.Asttypes.closed_flag -> Ast_410.Asttypes.closed_flag = function
  | Ast_409.Asttypes.Closed -> Ast_410.Asttypes.Closed
  | Ast_409.Asttypes.Open -> Ast_410.Asttypes.Open

and copy_label : Ast_409.Asttypes.label -> Ast_410.Asttypes.label = fun x -> x

and copy_rec_flag : Ast_409.Asttypes.rec_flag -> Ast_410.Asttypes.rec_flag =
  function
  | Ast_409.Asttypes.Nonrecursive -> Ast_410.Asttypes.Nonrecursive
  | Ast_409.Asttypes.Recursive -> Ast_410.Asttypes.Recursive

and copy_constant : Ast_409.Parsetree.constant -> Ast_410.Parsetree.constant =
  function
  | Ast_409.Parsetree.Pconst_integer (x0, x1) ->
      Ast_410.Parsetree.Pconst_integer (x0, map_option (fun x -> x) x1)
  | Ast_409.Parsetree.Pconst_char x0 -> Ast_410.Parsetree.Pconst_char x0
  | Ast_409.Parsetree.Pconst_string (x0, x1) ->
      Ast_410.Parsetree.Pconst_string (x0, map_option (fun x -> x) x1)
  | Ast_409.Parsetree.Pconst_float (x0, x1) ->
      Ast_410.Parsetree.Pconst_float (x0, map_option (fun x -> x) x1)

and copy_Longident_t : Longident.t -> Longident.t = fun x -> x

and copy_loc :
      'f0 'g0.
      ('f0 -> 'g0) -> 'f0 Ast_409.Asttypes.loc -> 'g0 Ast_410.Asttypes.loc =
 fun f0 { Ast_409.Asttypes.txt; Ast_409.Asttypes.loc } ->
  { Ast_410.Asttypes.txt = f0 txt; Ast_410.Asttypes.loc = copy_location loc }

and copy_location : Location.t -> Location.t = fun x -> x

let copy_expr = copy_expression
let copy_pat = copy_pattern
let copy_typ = copy_core_type
