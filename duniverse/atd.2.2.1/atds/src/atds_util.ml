(* Utilities *)

open Atd.Import
open Atds_env

(* Get rid of `wrap' constructors that we don't support on the Java side yet.
   They could be useful for timestamps, though. *)
let rec unwrap atd_ty =
  match atd_ty with
  | Atd.Ast.Wrap (_, x, _) -> unwrap x
  | x -> x

(* Normalise an ATD type by expanding `top-level' type aliases *)
let rec norm_ty env atd_ty =
  let atd_ty = unwrap atd_ty in
  match atd_ty with
  | Atd.Ast.Name (_, (_, name, _), _) ->
      (match name with
       | "bool" | "int" | "float" | "string" | "abstract" -> atd_ty
       | _ ->
           (match List.assoc name env.module_items with
            | Some x -> norm_ty env x
            | None ->
                eprintf "Warning: unknown type %s\n%!" name;
                atd_ty)
      )
  | _ ->
      atd_ty

let type_not_supported x =
  let loc = Atd.Ast.loc_of_type_expr x in
  Atd.Ast.error_at loc "Type not supported by atds."

let warning loc msg =
  let loc_s = Atd.Ast.string_of_loc loc in
  eprintf "\
Warning:
%s
%s
%!"
    loc_s
    msg
