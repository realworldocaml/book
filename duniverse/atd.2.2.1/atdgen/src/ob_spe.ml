
(*
  Optimization of the biniou representation
*)

let get_table_info deref x =
  match deref x with
      `Record y -> y
    | _ ->
        Error.error (Atd.Ast.loc_of_type_expr x) "Not a record type"
