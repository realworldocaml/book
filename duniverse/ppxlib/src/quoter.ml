open Import

type t = {
  mutable next_id : int;
  mutable bindings : Parsetree.value_binding list;
}

let create () = { next_id = 0; bindings = [] }

let sanitize t e =
  match t.bindings with
  | [] -> e
  | bindings ->
      let (module Ast) = Ast_builder.make e.pexp_loc in
      Ast.pexp_let Recursive bindings e

let quote t (e : expression) =
  let loc = e.pexp_loc in
  let (module Ast) = Ast_builder.make loc in
  let name = "__" ^ Int.to_string t.next_id in
  let binding_expr, quoted_expr =
    match e with
    (* Optimize identifier quoting by avoiding closure.
       See https://github.com/ocaml-ppx/ppx_deriving/pull/252. *)
    | { pexp_desc = Pexp_ident _; _ } -> (e, Ast.evar name)
    | _ ->
        let binding_expr =
          Ast.pexp_fun Nolabel None
            (let unit = Ast_builder.Default.Located.lident ~loc "()" in
             Ast.ppat_construct unit None)
            e
        in
        let quoted_expr = Ast.eapply (Ast.evar name) [ Ast.eunit ] in
        (binding_expr, quoted_expr)
  in
  let binding =
    let pat = Ast.pvar name in
    Ast.value_binding ~pat ~expr:binding_expr
  in
  t.bindings <- binding :: t.bindings;
  t.next_id <- t.next_id + 1;
  quoted_expr
