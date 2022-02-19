open Ppxlib

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt str ->
        let loc =
          match str with
          | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
          | hd :: _ -> hd.pstr_loc
        in
        let extension_node =
          Location.Error.(
            make ~loc "An error message in an extension node" ~sub:[]
            |> to_extension)
        in
        [ Ast_builder.Default.pstr_extension ~loc extension_node [] ])
      "raise_exc")

let () = Ppxlib.Driver.standalone ()
