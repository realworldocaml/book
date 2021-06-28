module Html = Tyxml.Html

open Utils

module Reason = Generator.Make (struct
  module Obj =
  struct
    let close_tag_closed = "}"
    let close_tag_extendable = "}"
    let field_separator = ", "
    let open_tag_closed = "{. "
    let open_tag_extendable = "{.. "
  end

  module Type =
  struct
    let annotation_separator = ": "
    let handle_constructor_params name args = name @ args
    let handle_substitution_params name args = name @ args
    let handle_format_params p = "(" ^ p ^ ")"
    let type_def_semicolon = true
    let private_keyword = "pri"

    let parenthesize_constructor = true

    module Variant =
    struct
      let parenthesize_params = true
    end

    module Tuple =
    struct
      let element_separator = ", "
      let always_parenthesize = true
    end

    module Record =
    struct
      let field_separator = ","
    end

    let var_prefix = "'"
    let any = "_"
    let arrow = Html.span [Html.txt "="; Html.entity "gt"]

    module Exception =
    struct
      let semicolon = true
    end

    module GADT =
    struct
      let arrow = Html.txt ":"
    end

    module External =
    struct
      let semicolon = true
      let handle_primitives prims =
        List.fold_left
          (fun acc p ->
            let str =
              match acc with [] -> "\"" ^ p ^ "\"" | _ -> " \"" ^ p ^ "\""
            in
            Html.txt str :: acc )
          [] prims
    end
  end

  module Mod =
  struct
    let open_tag = Html.txt "{"
    let close_tag = Html.txt "}"
    let close_tag_semicolon = true
    let include_semicolon = true
    let functor_keyword = false
  end

  module Class =
  struct
    let open_tag = Html.txt "{"
    let close_tag = Html.txt "}"
  end

  module Value =
  struct
    let variable_keyword = "let"
    let semicolon = true
  end
end)

include Reason.Page
