(* Test for the ppx_import old syntax compat support *)

open Ppxlib

let id =
  Extension.__declare_ppx_import
    "id"
    (fun ~ctxt:_ td ->
       match td.ptype_manifest with
       | Some {ptyp_desc = Ptyp_extension (_, PTyp wrapped_manifest); _} ->
         {td with ptype_manifest = Some wrapped_manifest}
       | _ -> assert false)
[%%expect{|
val id : Extension.t = <abstr>
|}]

Driver.register_transformation
  ~rules:[Context_free.Rule.extension id]
  "id"
[%%expect{|
- : unit = ()
|}]

(* The expander receives the type decl with the extension point removed, it should preserve
   attibutes *)
type t = [%id: int]
[%%expect{|
type t = int
|}]

(* It also should work in signatures by default *)
module type T = sig
  type t = [%id: int]
end
[%%expect{|
module type T = sig type t = int end
|}]

let foo =
  let check_interpreted (_, type_decls) =
    let {ptype_manifest; _} = List.hd type_decls in
    match ptype_manifest with
    | Some {ptyp_desc = Ptyp_extension _; _} ->
      failwith "Extension should be intepreted before attributes"
    | _ -> ()
  in
  Deriving.add "foo"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ type_decl ->
                         check_interpreted type_decl;
                         [%str let foo = 42]))
    ~sig_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ type_decl ->
                         check_interpreted type_decl;
                         [%sig: val foo : int]))
[%%expect{|
val foo : Deriving.t = <abstr>
|}]

(* It should properly compose with [@@deriving] *)
type t = [%id: int]
[@@deriving foo]
[%%expect{|
type t = int
val foo : t = 42
|}]

module type T = sig
  type t = [%id: int]
  [@@deriving foo]
end
[%%expect{|
module type T = sig type t = int val foo : t end
|}]

(* It should be properly interpreted if it's the result of the expansion of a
   previous node as well *)
let gen_id =
  Extension.V3.declare
    "gen_id"
    Extension.Context.structure_item
    Ast_pattern.(pstr nil)
    (fun ~ctxt ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       [%stri type t = [%id: int]])
[%%expect{|
val gen_id : Extension.t = <abstr>
|}]

Driver.register_transformation
  ~rules:[Context_free.Rule.extension gen_id]
  "gen_id"
[%%expect{|
- : unit = ()
|}]

[%%gen_id]
[%%expect{|
type t = int
|}]

(* One can't have ppx_import-like and core_type extensions with the same name *)
let id_for_core_types =
  Extension.V3.declare
    "id"
    Extension.Context.core_type
    Ast_pattern.(ptyp __)
    (fun ~ctxt:_ core_type -> core_type)
[%%expect{|
Exception:
Failure
 "Some ppx-es tried to register conflicting transformations: Extension 'id' on type declarations matches extension 'id'".
|}]
