open! Base
open Ppxlib
open Ast_builder.Default

module Pervasive = struct
  (* this list comes from lib/typerep_lib/lib/type_generic.mli *)
  type t =
    | Array
    | Lazy
    | List
    | Option
    | Ref

  let of_string = function
    | "array" -> Some Array
    | "lazy_t" -> Some Lazy
    | "list" -> Some List
    | "option" -> Some Option
    | "ref" -> Some Ref
    | _ -> None
  ;;
end


(* The specification for which values need to be replaced *)
type t =
  | None
  | Replace of string
  | Tuple of t list
  | Constr_pervasive of Pervasive.t * t
  | Constr_t of longident * t list

let rec needs_mapping = function
  | None -> false
  | Replace _ -> true
  | Constr_pervasive (_, t) -> needs_mapping t
  | Constr_t (_, ts) | Tuple ts -> List.exists ts ~f:needs_mapping
;;

let rec apply ~loc ~map t expr =
  match t with
  | None -> expr
  | Constr_pervasive (pervasive, t) ->
    (match pervasive with
     | Option -> [%expr Stdlib.Option.map [%e apply_fn ~loc ~map t] [%e expr]]
     | List -> [%expr Stdlib.List.map [%e apply_fn ~loc ~map t] [%e expr]]
     | Ref -> [%expr ref [%e apply ~loc ~map t [%expr ![%e expr]]]]
     | Array -> [%expr Stdlib.Array.map [%e apply_fn ~loc ~map t] [%e expr]]
     | Lazy -> [%expr lazy [%e apply ~loc ~map t [%expr Stdlib.Lazy.force [%e expr]]]])
  | Constr_t (longident, ts) ->
    let map_fn = pexp_ident ~loc (Located.mk ~loc (Ldot (longident, "map"))) in
    (match ts with
     | [] -> expr
     | [ t ] -> [%expr [%e map_fn] [%e expr] ~f:[%e apply_fn ~loc ~map t]]
     | ts ->
       let exprs =
         (Nolabel, expr)
         :: List.mapi ts ~f:(fun i t ->
           Labelled ("f" ^ Int.to_string (i + 1)), apply_fn ~loc ~map t)
       in
       pexp_apply ~loc map_fn exprs)
  | Replace text -> [%expr [%e Map.find_exn map text] [%e expr]]
  | Tuple ts ->
    let names = List.mapi ts ~f:(fun i _ -> "v" ^ Int.to_string i) in
    pexp_let
      ~loc
      Nonrecursive
      [ value_binding ~loc ~pat:(ppat_tuple ~loc (List.map names ~f:(pvar ~loc))) ~expr ]
      (pexp_tuple
         ~loc
         (List.map2_exn ts names ~f:(fun t name -> apply ~loc ~map t (evar ~loc name))))

and apply_fn ~loc ~map t =
  pexp_fun ~loc Nolabel None (pvar ~loc "x") (apply ~loc ~map t (evar ~loc "x"))
;;

let rec find_targets ~target ~loc core_type =
  let list tys ~f =
    let recs = List.map tys ~f:(find_targets ~target ~loc) in
    if List.exists recs ~f:needs_mapping then f recs else None
  in
  match core_type.ptyp_desc with
  | Ptyp_constr (lid_loc, args) ->
    (match lid_loc.txt with
     | Lapply _ -> Location.raise_errorf ~loc "Unexpected Lapply"
     | Lident text when Set.mem target text -> Replace text
     | Lident text ->
       (match Pervasive.of_string text with
        | None -> None
        | Some kind ->
          (match args with
           | [] | _ :: _ :: _ ->
             (* Pervasive type with unexpected arity, so it's not a pervasive *)
             None
           | [ arg ] ->
             let t = find_targets ~target ~loc arg in
             (match needs_mapping t with
              | false -> None
              | true -> Constr_pervasive (kind, t))))
     | Ldot (l, "t") -> list args ~f:(fun recs -> Constr_t (l, recs))
     | Ldot _ -> None)
  | Ptyp_tuple args -> list args ~f:(fun x -> Tuple x)
  | _ -> None
;;

type replace_result =
  | Unchanged
  | Replaced

let build ~loc ~map ty expr =
  let t = find_targets ~target:(Set.of_list (module String) (Map.keys map)) ~loc ty in
  if needs_mapping t then Replaced, apply ~loc ~map t expr else Unchanged, expr
;;
