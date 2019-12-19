open Import

type t =
  { mutable next_id : int
  ; mutable bindings : Parsetree.value_binding list
  }

let create () =
  { next_id = 0
  ; bindings = []
  }

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
  let binding =
    let pat = Ast.pvar name in
    let expr =
      Ast.pexp_fun Nolabel None
        (let unit = Ast_builder.Default.Located.lident ~loc "()" in
         Ast.ppat_construct unit None)
        e
    in
    Ast.value_binding ~pat ~expr
  in
  t.bindings <- binding :: t.bindings;
  t.next_id <- t.next_id + 1;
  Ast.evar name
