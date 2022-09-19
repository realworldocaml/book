open Stdppx
open Ppxlib

let _ =
  let loc = Location.none in
  let extension =
    Extension.V3.declare_inline
      "include"
      Structure_item
      Ast_pattern.(pstr __)
      (fun ~ctxt:_ x -> x)
  in
  let rules = [ Context_free.Rule.extension extension ] in
  let enclose_impl _ _ = [%str [%%include let a = 1]], [%str [%%include let c = 3]] in
  Driver.V2.register_transformation ~rules ~enclose_impl "example"

[%%expect{|
- : unit = ()
|}]

let b = 2

[%%expect{|
val a : int = 1
val b : int = 2
val c : int = 3
|}]
