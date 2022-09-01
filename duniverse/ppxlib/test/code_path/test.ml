#require "base";;

open Base
open Ppxlib

let sexp_of_code_path code_path =
  Sexp.message
    "code_path"
    [ "main_module_name", sexp_of_string (Code_path.main_module_name code_path)
    ; "submodule_path", sexp_of_list sexp_of_string (Code_path.submodule_path code_path)
    ; "enclosing_module", sexp_of_string (Code_path.enclosing_module code_path)
    ; "enclosing_value", sexp_of_option sexp_of_string (Code_path.enclosing_value code_path)
    ; "value", sexp_of_option sexp_of_string (Code_path.value code_path)
    ; "fully_qualified_path", sexp_of_string (Code_path.fully_qualified_path code_path)
    ]

let () =
  Driver.register_transformation "test"
    ~extensions:[
      Extension.V3.declare "code_path"
        Expression
        Ast_pattern.(pstr nil)
        (fun ~ctxt ->
           let loc = Expansion_context.Extension.extension_point_loc ctxt in
           let code_path = Expansion_context.Extension.code_path ctxt in
           Ast_builder.Default.estring ~loc
             (Sexp.to_string (sexp_of_code_path code_path)))
    ]
[%%expect{|
val sexp_of_code_path : Code_path.t -> Sexp.t = <fun>
|}]

let s =
  let module A = struct
    module A' = struct
      let a =
        let module B = struct
          module B' = struct
            let b =
              let module C = struct
                module C' = struct
                  let c = [%code_path]
                end
              end
              in C.C'.c
          end
        end
        in B.B'.b
    end
  end
  in A.A'.a
;;
[%%expect{|
val s : string =
  "(code_path(main_module_name Test)(submodule_path())(enclosing_module C')(enclosing_value(c))(value(s))(fully_qualified_path Test.s))"
|}]

let module M = struct
  let m = [%code_path]
  end
  in
  M.m
[%%expect{|
- : string =
"(code_path(main_module_name Test)(submodule_path())(enclosing_module M)(enclosing_value(m))(value())(fully_qualified_path Test))"
|}]

module Outer = struct
  module Inner = struct
    let code_path = [%code_path]
  end
end
let _ = Outer.Inner.code_path
[%%expect{|
module Outer : sig module Inner : sig val code_path : string end end
- : string =
"(code_path(main_module_name Test)(submodule_path(Outer Inner))(enclosing_module Inner)(enclosing_value(code_path))(value(code_path))(fully_qualified_path Test.Outer.Inner.code_path))"
|}]

module Functor() = struct
  let code_path = ref ""
  module _ = struct
    let x =
      let module First_class = struct
        code_path := [%code_path]
      end in
      let module _ = First_class in
      ()
    ;;

    ignore x
  end
end
let _ = let module M = Functor() in !M.code_path
[%%expect{|
module Functor : functor () -> sig val code_path : string ref end
- : string =
"(code_path(main_module_name Test)(submodule_path(Functor _))(enclosing_module First_class)(enclosing_value(x))(value(x))(fully_qualified_path Test.Functor._.x))"
|}]
