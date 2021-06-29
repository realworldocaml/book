open Base
open Ppxlib
open Ppx_compare_expander

let add_deriver name (module E : Ppx_compare_expander.S) =
  let str_type_decl =
    Deriving.Generator.V2.make_noarg E.str_type_decl
      ~attributes:E.str_attributes
  in
  let sig_type_decl =
    Deriving.Generator.V2.make_noarg E.sig_type_decl
  in
  Deriving.add name
    ~str_type_decl
    ~sig_type_decl

let compare = add_deriver "compare" (module Compare)
let equal = add_deriver "equal" (module Equal)

let replace_underscores_by_variables =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_any -> Ptyp_var (gen_symbol ~prefix:"a" ())
      | t -> super#core_type_desc t
  end in
  map#core_type

let () =
  List.iter
    [ "compare"        , Compare.type_ , Compare.core_type
    ; "equal"          , Equal.type_   , Equal.core_type
    ; "@compare.equal" , Equal.type_   , Compare.equal_core_type
    ]
    ~f:(fun (name, type_, core_type) ->
      Driver.register_transformation (String.strip name ~drop:(Char.equal '@'))
        ~rules:[ Context_free.Rule.extension
                   (Extension.declare name
                      Core_type Ast_pattern.(ptyp __)
                      (fun ~loc ~path:_ ty ->
                         type_ ~hide:true ~loc
                           (replace_underscores_by_variables ty)))
               ; Context_free.Rule.extension
                   (Extension.declare name
                      Expression Ast_pattern.(ptyp __)
                      (fun ~loc:_ ~path:_ ty ->
                         core_type ty))
               ])
