open! Ppxlib

(* There's some rule I don't understand with extension names where dot prefixes can be
   ignored. There's also an error when you use an extension reserved by the compiler (like
   [%probe] with Jane Street's compiler), and prefixing gets around it. *)

(* [%probe "name" expr] in its default disabled state is equivalent to 
   [if false then expr]. We use this rewrite instead of [()] to avoid causing unused
   variable warnings. *)
let probe_extension =
  Extension.declare
    "ppx_ignore_instrumentation.probe"
    Extension.Context.expression
    Ast_pattern.(
      pstr (pstr_eval (pexp_apply (estring __) (no_label __ ^:: nil)) nil ^:: nil))
    (fun ~loc ~path:_ _ ex ->
       let loc = { loc with loc_ghost = true } in
       [%expr if false then [%e ex]])
;;

(* [%probe_is_enabled "name"] in its default disabled state is equivalent to [false] *)
let probe_is_enabled_extension =
  Extension.declare
    "ppx_ignore_instrumentation.probe_is_enabled"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ _expr -> [%expr false])
;;

let () =
  Driver.register_transformation
    "ignore_instrumentation"
    ~extensions:[ probe_extension; probe_is_enabled_extension ]
;;
