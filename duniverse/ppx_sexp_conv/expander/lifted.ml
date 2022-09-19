open! Base
open Ppxlib
open Ast_builder.Default

type 'a t =
  { value_bindings : value_binding list
  ; body : 'a
  }

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return body = { value_bindings = []; body }

    let bind a ~f =
      let b = f a.body in
      { value_bindings = a.value_bindings @ b.value_bindings; body = b.body }
    ;;

    let map = `Define_using_bind
  end)

let create ~loc ~prefix ~ty rhs =
  let name = gen_symbol ~prefix () in
  let lhs = pvar ~loc name in
  let body = evar ~loc name in
  let ty, rhs, body =
    if Helpers.is_value_expression rhs
    then ty, rhs, body
    else (
      (* Thunkify the value to evaluate when referred to. *)
      let ty = [%type: Stdlib.Unit.t -> [%t ty]] in
      let rhs = [%expr fun () -> [%e rhs]] in
      let body = [%expr [%e body] ()] in
      ty, rhs, body)
  in
  { value_bindings = [ value_binding ~loc ~pat:(ppat_constraint ~loc lhs ty) ~expr:rhs ]
  ; body
  }
;;

let let_bind_user_expressions { value_bindings; body } ~loc =
  if List.is_empty value_bindings
  then body
  else pexp_let ~loc Nonrecursive value_bindings body
;;
