module Base = struct
  type t =
    { omp_config : Migrate_parsetree.Driver.config
    ; code_path : Code_path.t
    }

  let top_level ~omp_config ~file_path =
    let code_path = Code_path.top_level ~file_path in
    {omp_config; code_path}

  let enter_expr t = {t with code_path = Code_path.enter_expr t.code_path}
  let enter_module ~loc name t = {t with code_path = Code_path.enter_module ~loc name t.code_path}
  let enter_value ~loc name t = {t with code_path = Code_path.enter_value ~loc name t.code_path}
end

module Extension = struct
  type t =
    { extension_point_loc : Location.t
    ; base : Base.t
    }

  let make ~extension_point_loc ~base () = {extension_point_loc; base}

  let extension_point_loc t = t.extension_point_loc
  let code_path t = t.base.code_path
  let omp_config t = t.base.omp_config

  let with_loc_and_path f =
    fun ~ctxt ->
      f ~loc:ctxt.extension_point_loc ~path:(Code_path.to_string_path ctxt.base.code_path)
end

module Deriver = struct
  type t =
    { derived_item_loc : Location.t
    ; inline : bool
    ; base : Base.t
    }

  let make ~derived_item_loc ~inline ~base () = {derived_item_loc; base; inline}

  let derived_item_loc t = t.derived_item_loc
  let code_path t = t.base.code_path
  let omp_config t = t.base.omp_config
  let inline t = t.inline

  let with_loc_and_path f =
    fun ~ctxt -> f ~loc:ctxt.derived_item_loc ~path:(Code_path.to_string_path ctxt.base.code_path)
end
