open Ppxlib

let derive_a_string ~ctxt (_rec_flag, _type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let open Ast_builder.Default in
  [
    pstr_value ~loc Nonrecursive
      [
        {
          pvb_pat = ppat_any ~loc;
          pvb_expr = estring ~loc "derived_string";
          pvb_attributes = [];
          pvb_loc = loc;
        };
      ];
  ]

let impl_generator_derive_a_string =
  Deriving.Generator.V2.make_noarg derive_a_string

let deriver_for_a_string =
  Deriving.add "a_string" ~str_type_decl:impl_generator_derive_a_string

let impl_generator_dependent =
  Deriving.Generator.V2.make_noarg ~deps:[ deriver_for_a_string ]
    derive_a_string

let dependent_deriver =
  Deriving.add "a_dependent_string" ~str_type_decl:impl_generator_dependent

let () = Driver.standalone ()
