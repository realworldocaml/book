open Base
open Ppxlib


let payload_never ~loc =
  let (module B) = Ast_builder.make loc in
  let open B in
  PStr [ pstr_eval (pexp_ident (Located.lident "never")) [] ]

let expand_cold_attribute attr =
  assert (String.equal attr.attr_name.txt "cold");
  let loc = { attr.attr_name.loc with loc_ghost = true } in
  let payload = payload_never ~loc in
  [ Loc.make ~loc "ocaml.inline", payload
  ; Loc.make ~loc "ocaml.local", payload
  ; Loc.make ~loc "ocaml.specialise", payload ]
  |> List.map ~f:(fun (name, payload) ->
    Ast_builder.Default.attribute ~loc ~name ~payload)

class attributes_mapper = object
  inherit Ast_traverse.map as super

  method! attributes attrs =
    let attrs =
      List.concat_map attrs ~f:(function
        | { attr_name = { txt = "cold"; _ };
            attr_payload = PStr [];
            attr_loc = _; } as attr ->
          Attribute.mark_as_handled_manually attr;
          expand_cold_attribute attr
        | attr -> [attr]
      )
    in
    super#attributes attrs
end

let expand_cold = (new attributes_mapper)#structure

let () =
  Driver.register_transformation "cold"
    ~impl:expand_cold
