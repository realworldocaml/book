module Base = struct
  type t = { tool_name : string; code_path : Code_path.t; input_name : string }

  let top_level ~tool_name ~file_path ~input_name =
    let code_path = Code_path.top_level ~file_path in
    { tool_name; code_path; input_name }

  let code_path t = t.code_path

  let input_name t = t.input_name

  let tool_name t = t.tool_name

  let enter_expr t = { t with code_path = Code_path.enter_expr t.code_path }

  let enter_module ~loc name t =
    { t with code_path = Code_path.enter_module ~loc name t.code_path }

  let enter_value ~loc name t =
    { t with code_path = Code_path.enter_value ~loc name t.code_path }
end

module Extension = struct
  type t = { extension_point_loc : Location.t; base : Base.t }

  let make ~extension_point_loc ~base () = { extension_point_loc; base }

  let extension_point_loc t = t.extension_point_loc

  let code_path t = t.base.code_path

  let input_name t = t.base.input_name

  let tool_name t = t.base.tool_name

  let with_loc_and_path f ~ctxt =
    f ~loc:ctxt.extension_point_loc
      ~path:(Code_path.to_string_path ctxt.base.code_path)
end

module Deriver = struct
  type t = { derived_item_loc : Location.t; inline : bool; base : Base.t }

  let make ~derived_item_loc ~inline ~base () =
    { derived_item_loc; base; inline }

  let derived_item_loc t = t.derived_item_loc

  let code_path t = t.base.code_path

  let input_name t = t.base.input_name

  let tool_name t = t.base.tool_name

  let inline t = t.inline

  let with_loc_and_path f ~ctxt =
    f ~loc:ctxt.derived_item_loc
      ~path:(Code_path.to_string_path ctxt.base.code_path)
end
