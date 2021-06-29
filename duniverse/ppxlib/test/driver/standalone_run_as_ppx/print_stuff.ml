open Ppxlib

let mk_expression ~loc pexp_desc =
  { pexp_desc; pexp_loc_stack = []; pexp_loc = loc; pexp_attributes = [] }

let print_string s ~loc =
  let print_exp =
    mk_expression ~loc (Pexp_ident { txt = Lident "print_endline"; loc })
  in
  let string_exp =
    mk_expression ~loc (Pexp_constant (Pconst_string (s, loc, None)))
  in
  mk_expression ~loc (Pexp_apply (print_exp, [ (Nolabel, string_exp) ]))

let hi_rule =
  let expand ~loc ~path:_ = print_string "hi" ~loc in
  Extension.declare "print_hi" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
  |> Context_free.Rule.extension

let tool_name_rule =
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let tool_name = Expansion_context.Extension.tool_name ctxt in
    print_string tool_name ~loc
  in
  Extension.V3.declare "print_tool_name" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
  |> Context_free.Rule.extension

let fname_rule =
  let expand ~loc ~path:_ = print_string ~loc loc.loc_start.pos_fname in
  Extension.declare "print_fname" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
  |> Context_free.Rule.extension

let () =
  Driver.register_transformation
    ~rules:[ hi_rule; tool_name_rule; fname_rule ]
    "test"

let () = Ppxlib.Driver.standalone ()
