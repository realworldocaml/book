open! Base
open Ppxlib

let pattern =
  let open Ast_pattern in
  map (single_expr_payload __) ~f:(fun f x -> f (Some x))
  ||| map (pstr nil) ~f:(fun f -> f None)
;;

let message ~name ~omit_nil =
  Extension.declare
    name
    Extension.Context.expression
    pattern
    (Ppx_sexp_message_expander.expand_opt ~omit_nil)
;;

let () =
  Driver.register_transformation
    "sexp_message"
    ~extensions:
      [ message ~name:"message" ~omit_nil:false
      ; message ~name:"@message.omit_nil" ~omit_nil:true
      ]
;;
