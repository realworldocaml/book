#require "base";;

open Ppxlib

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
             (Code_path.fully_qualified_path code_path))
    ]
[%%expect{|
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
val s : string = "Test.s"
|}]

let module M = struct
  let m = [%code_path]
  end
  in
  M.m
[%%expect{|
- : string = "Test"
|}]
