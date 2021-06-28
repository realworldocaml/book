open! Import
include Field_syntax_intf

module Tuple = struct
  type ast = core_type
  type t = ast

  let create = Fn.id
  let location t = t.ptyp_loc
  let core_type t = t
  let pattern _ ~loc pat_list = ppat_tuple ~loc pat_list
  let expression _ ~loc expr_list = pexp_tuple ~loc expr_list
end

module Record = struct
  type ast = label_declaration
  type t = ast

  let create ast =
    match ast.pld_mutable with
    | Immutable -> ast
    | Mutable ->
      (* We intend to support mutable fields and values shortly, but we leave it to a
         separate feature. Integrating mutable values with replayability and shrinking is
         tricky, and we at least have to figure out what caveats to document. *)
      unsupported ~loc:ast.pld_loc "mutable record field"
  ;;

  let location t = t.pld_loc
  let core_type t = t.pld_type

  let pattern list ~loc pat_list =
    let alist =
      List.map2_exn list pat_list ~f:(fun t pat -> lident_loc t.pld_name, pat)
    in
    ppat_record ~loc alist Closed
  ;;

  let expression list ~loc expr_list =
    let alist =
      List.map2_exn list expr_list ~f:(fun t expr -> lident_loc t.pld_name, expr)
    in
    pexp_record ~loc alist None
  ;;
end
