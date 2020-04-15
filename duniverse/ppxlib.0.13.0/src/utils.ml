open Import

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f
;;

module Kind = struct
  type t = Intf | Impl

  let of_filename fn : t option =
    if Caml.Filename.check_suffix fn ".ml" then
      Some Impl
    else if Caml.Filename.check_suffix fn ".mli" then
      Some Intf
    else
      None
  ;;

  let describe = function
    | Impl -> "implementation"
    | Intf -> "interface"
  ;;

  let equal : t -> t -> bool = Poly.equal
end

module Some_intf_or_impl = struct
  type t =
    | Intf of Migrate_parsetree.Driver.some_signature
    | Impl of Migrate_parsetree.Driver.some_structure

  let to_ast_io (ast : t) ~add_ppx_context =
    let open Migrate_parsetree in
    match ast with
    | Intf (Migrate_parsetree.Driver.Sig ((module Ver), sg)) ->
      let sg =
        (Migrate_parsetree.Versions.migrate
           (module Ver)
           (module Versions.OCaml_current)).copy_signature sg
      in
      let sg =
        if add_ppx_context then
          Ocaml_common.Ast_mapper.add_ppx_context_sig ~tool_name:"ppxlib_driver" sg
        else
          sg
      in
      Ast_io.Intf ((module Versions.OCaml_current), sg)
    | Impl (Migrate_parsetree.Driver.Str ((module Ver), st)) ->
      let st =
        (Migrate_parsetree.Versions.migrate
                      (module Ver)
                      (module Versions.OCaml_current)).copy_structure st
      in
      let st =
        if add_ppx_context then
          Ocaml_common.Ast_mapper.add_ppx_context_str ~tool_name:"ppxlib_driver" st
        else
          st
      in
      Ast_io.Impl ((module Versions.OCaml_current), st)
end

module Intf_or_impl = struct
  type t =
    | Intf of signature
    | Impl of structure

  let map t (map : Ast_traverse.map) =
    match t with
    | Impl x -> Impl (map#structure x)
    | Intf x -> Intf (map#signature x)
  ;;

  let map_with_context t (map : _ Ast_traverse.map_with_context) ctx =
    match t with
    | Impl x -> Impl (map#structure ctx x)
    | Intf x -> Intf (map#signature ctx x)
  ;;

  let kind : _ -> Kind.t = function
    | Intf _ -> Intf
    | Impl _ -> Impl

  let of_some_intf_or_impl ast : t =
    let open Some_intf_or_impl in
    match ast with
    | Intf (Migrate_parsetree.Driver.Sig ((module Ver), sg)) ->
      Intf ((Migrate_parsetree.Versions.migrate (module Ver)
               (module Ppxlib_ast.Selected_ast)).copy_signature sg)
    | Impl (Migrate_parsetree.Driver.Str ((module Ver), st)) ->
      Impl ((Migrate_parsetree.Versions.migrate (module Ver)
               (module Ppxlib_ast.Selected_ast)).copy_structure st)

  let of_ast_io ast : t =
    let open Migrate_parsetree in
    match ast with
    | Ast_io.Intf ((module Ver), sg) ->
      let module C = Versions.Convert(Ver)(Ppxlib_ast.Selected_ast) in
      Intf (C.copy_signature sg)
    | Ast_io.Impl ((module Ver), st) ->
      let module C = Versions.Convert(Ver)(Ppxlib_ast.Selected_ast) in
      Impl (C.copy_structure st)
end
(*
let map_impl x ~(f : _ Intf_or_impl.t -> _ Intf_or_impl.t) =
  match f (Impl x) with
  | Impl x -> x
  | Intf _ -> assert false

let map_intf x ~(f : _ Intf_or_impl.t -> _ Intf_or_impl.t) =
  match f (Intf x) with
  | Intf x -> x
  | Impl _ -> assert false
*)
