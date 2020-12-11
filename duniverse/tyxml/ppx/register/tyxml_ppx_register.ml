open Ppxlib

let str_item_expansion name lang =
  Extension.declare_with_path_arg
    name
    Extension.Context.structure_item
    Ast_pattern.(pstr ((pstr_value __ __) ^:: nil))
    (Tyxml_ppx.expand_str_item ~lang)

let expr_expansion name lang =
  Extension.declare_with_path_arg
    name
    Extension.Context.expression
    Ast_pattern.(pstr ((pstr_eval __ __) ^:: nil))
    (Tyxml_ppx.expand_expr ~lang)

let () =
  let extensions = [
    expr_expansion "tyxml.html" Html;
    expr_expansion "tyxml.svg" Svg;
    str_item_expansion "tyxml.html" Html;
    str_item_expansion "tyxml.svg" Svg;
  ]
  in    
  Ppxlib.Driver.register_transformation ~extensions "tyxml"
