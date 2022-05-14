open Ppxlib

let log_source_position = ref false

let () =
  Driver.add_arg
    "-log-source-position"
    (Set log_source_position)
    ~doc:
      " If set, adds a \"pos\" tag with a source code position to every logged message."
;;

let expand ~level ~loc ~path:_ log message_args =
  let loc = { loc with loc_ghost = true } in
  let pos = Ppx_here_expander.lift_position ~loc in
  let maybe_pos = if !log_source_position then [%expr Some [%e pos]] else [%expr None] in
  let sexp =
    Ppx_sexp_message_expander.sexp_of_labelled_exprs ~omit_nil:false ~loc message_args
  in
  (* In order to use ppx_metaquot, we pass in a loc parameter to level. *)
  let level = level loc in
  [%expr
    if Ppx_log_syntax.would_log [%e log] (Some [%e level]) [@merlin.hide]
    then Ppx_log_syntax.sexp ~level:[%e level] ?pos:[%e maybe_pos] [%e log] [%e sexp]
    else Ppx_log_syntax.default]
;;

let pattern =
  let open Ast_pattern in
  (* this grabs the first argument from the apply and
     then passes it into Log.sexp's [log] parameter.
     All the arguments of apply are parsed as a message. *)
  pstr (pstr_eval (pexp_apply __ __) nil ^:: nil)
;;

let ext name f =
  Extension.declare
    name
    Extension.Context.expression
    pattern
    (expand ~level:(fun loc -> f loc))
;;

(* [Global] has a similar structure to the above code, except that
   it doesn't bother with parsing out a [log] parameter. *)
module Global = struct
  let expand ~level ~loc ~path message_args =
    let loc = { loc with loc_ghost = true } in
    let pos = Ppx_here_expander.lift_position ~loc in
    let maybe_pos =
      if !log_source_position then [%expr Some [%e pos]] else [%expr None]
    in
    let sexp = Ppx_sexp_message_expander.expand ~omit_nil:false ~path message_args in
    let level = level loc in
    [%expr
      if Ppx_log_syntax.Global.would_log (Some [%e level])
      then Ppx_log_syntax.Global.sexp ~level:[%e level] ?pos:[%e maybe_pos] [%e sexp]
      else Ppx_log_syntax.Global.default]
  ;;

  let pattern = Ast_pattern.(single_expr_payload __)

  let ext name f =
    Extension.declare
      name
      Extension.Context.expression
      pattern
      (expand ~level:(fun loc -> f loc))
  ;;
end

let () =
  Driver.register_transformation
    "log"
    ~extensions:
      [ ext "log.debug" (fun loc -> [%expr `Debug])
      ; ext "log.info" (fun loc -> [%expr `Info])
      ; ext "log.error" (fun loc -> [%expr `Error])
      ; Global.ext "log.global.debug" (fun loc -> [%expr `Debug])
      ; Global.ext "log.global.info" (fun loc -> [%expr `Info])
      ; Global.ext "log.global.error" (fun loc -> [%expr `Error])
      ]
;;
