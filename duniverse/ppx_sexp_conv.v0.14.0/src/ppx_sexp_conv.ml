(* sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open Ppxlib

module Attrs     = Ppx_sexp_conv_expander.Attrs

module Sexp_of = struct
  module E = Ppx_sexp_conv_expander.Sexp_of
  let name = "sexp_of"

  let str_type_decl =
    Deriving.Generator.make_noarg E.str_type_decl
      ~attributes:[ Attribute.T Attrs.default
                  ; Attribute.T Attrs.drop_default
                  ; Attribute.T Attrs.drop_if
                  ]
  ;;

  let str_exception =
    Deriving.Generator.make_noarg E.str_exception
  ;;

  let sig_type_decl =
    Deriving.Generator.make_noarg E.sig_type_decl
  ;;

  let sig_exception =
    Deriving.Generator.make_noarg E.sig_exception
  ;;

  let extension ~loc:_ ~path:_ ctyp = E.core_type ctyp

  let deriver =
    Deriving.add name
      ~str_type_decl
      ~str_exception
      ~sig_type_decl
      ~sig_exception
      ~extension
  ;;

  let () =
    Driver.register_transformation name
      ~rules:[ Context_free.Rule.extension
                 (Extension.declare name
                    Core_type Ast_pattern.(ptyp __)
                    (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
             ]
  ;;

end

module Of_sexp = struct
  module E = Ppx_sexp_conv_expander.Of_sexp
  let name = "of_sexp"

  let str_type_decl =
    Deriving.Generator.make_noarg (E.str_type_decl ~poly:false)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl =
    Deriving.Generator.make_noarg (E.sig_type_decl ~poly:false)
  ;;

  let extension ~loc:_ ~path ctyp = E.core_type ~path ctyp

  let deriver =
    Deriving.add name
      ~str_type_decl
      ~sig_type_decl
      ~extension
  ;;

  let () =
    Driver.register_transformation name
      ~rules:[ Context_free.Rule.extension
                 (Extension.declare name
                    Core_type Ast_pattern.(ptyp __)
                    (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
             ]
  ;;
end

module Of_sexp_poly = struct
  module E = Ppx_sexp_conv_expander.Of_sexp

  let str_type_decl =
    Deriving.Generator.make_noarg (E.str_type_decl ~poly:true)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl =
    Deriving.Generator.make_noarg (E.sig_type_decl ~poly:true)
  ;;

  let deriver =
    Deriving.add "of_sexp_poly"
      ~sig_type_decl
      ~str_type_decl
  ;;
end

module Sexp_grammar = struct
  module E = Ppx_sexp_conv_expander.Sexp_grammar
  let name = "sexp_grammar"

  let str_type_decl =
    Deriving.Generator.make_noarg E.str_type_decl
  ;;

  let sig_type_decl =
    Deriving.Generator.make_noarg E.sig_type_decl
  ;;

  let extension  = E.core_type

  let deriver =
    Deriving.add name
      ~sig_type_decl
      ~str_type_decl
      ~extension
  ;;

  let () =
    Driver.register_transformation name
      ~rules:[ Context_free.Rule.extension
                 (Extension.declare name
                    Core_type Ast_pattern.(ptyp __)
                    (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
             ]
  ;;

end

let sexp_of = Sexp_of.deriver
let of_sexp = Of_sexp.deriver
let of_sexp_poly = Of_sexp_poly.deriver
let sexp_grammar = Sexp_grammar.deriver

module Sexp_in_sig = struct
  module E = Ppx_sexp_conv_expander.Sig_sexp

  let sig_type_decl =
    Deriving.Generator.make_noarg E.sig_type_decl
  ;;

  let deriver =
    Deriving.add
      "ppx_sexp_conv: let this be a string that wouldn't parse if put in the source"
      ~sig_type_decl
end

let sexp =
  Deriving.add_alias "sexp" [sexp_of; of_sexp]
    ~sig_type_decl:[Sexp_in_sig.deriver]
    ~str_exception:[sexp_of]
    ~sig_exception:[sexp_of]

let sexp_poly =
  Deriving.add_alias "sexp_poly" [sexp_of; of_sexp_poly]
