open Base
open Ppxlib
open Ast_builder.Default

let disabled_warnings = "-26-27-32-33-34-35-36-37-38-39-60-66-67"
let payload ~loc = PStr [ pstr_eval ~loc (estring ~loc disabled_warnings) [] ]

let expand_disabled_unused_warnings_attribute attr =
  assert (String.equal attr.attr_name.txt "disable_unused_warnings");
  let loc = { attr.attr_name.loc with loc_ghost = true } in
  let name = Loc.make ~loc "warning" in
  let payload = payload ~loc in
  attribute ~loc ~name ~payload
;;

class attribute_mapper =
  object
    inherit Ast_traverse.map as super

    method! attribute attr =
      match attr with
      | { attr_name = { txt = "disable_unused_warnings"; _ }
        ; attr_payload = PStr []
        ; attr_loc = _
        } ->
        Attribute.mark_as_handled_manually attr;
        expand_disabled_unused_warnings_attribute attr
      | _ -> super#attribute attr
  end

let expand_disabled_unused_warnings = (new attribute_mapper)#structure

let () =
  (* The ~rules argument isn't expressive enough to use here (it can't handle attributes
     in all positions *)
  Driver.register_transformation
    "disable_unused_warnings"
    ~impl:expand_disabled_unused_warnings
;;
