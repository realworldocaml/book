open! Base
open! Ppxlib

type t =
  { universal : (Fresh_name.t, string loc) Result.t Map.M(String).t
  ; existential : bool
  }

module Binding_kind = struct
  type t =
    | Universally_bound of Fresh_name.t
    | Existentially_bound
end

let add_universally_bound t name ~prefix =
  { t with
    universal =
      Map.set
        t.universal
        ~key:name.txt
        ~data:(Ok (Fresh_name.create (prefix ^ name.txt) ~loc:name.loc))
  }
;;

let binding_kind t var ~loc =
  match Map.find t.universal var with
  | None ->
    if t.existential
    then Binding_kind.Existentially_bound
    else Location.raise_errorf ~loc "ppx_sexp_conv: unbound type variable '%s" var
  | Some (Ok fresh) -> Binding_kind.Universally_bound fresh
  | Some (Error { loc; txt }) -> Location.raise_errorf ~loc "%s" txt
;;

(* Return a map translating type variables appearing in the return type of a GADT
   constructor to their name in the type parameter list.

   For instance:

   {[
     type ('a, 'b) t = X : 'x * 'y -> ('x, 'y) t
   ]}

   will produce:

   {v
     "x" -> Ok "a"
     "y" -> Ok "b"
   v}

   If a variable appears twice in the return type it will map to [Error _]. If a
   variable cannot be mapped to a parameter of the type declaration, it will map to
   [Error] (for instance [A : 'a -> 'a list t]).

   It returns [original] on user error, to let the typer give the error message *)
let with_constructor_declaration original cd ~type_parameters:tps =
  (* Add all type variables of a type to a map. *)
  let add_typevars =
    object
      inherit [t] Ast_traverse.fold as super

      method! core_type ty t =
        match ty.ptyp_desc with
        | Ptyp_var var ->
          let error =
            { loc = ty.ptyp_loc
            ; txt = "ppx_sexp_conv: variable is not a parameter of the type constructor"
            }
          in
          { t with universal = Map.set t.universal ~key:var ~data:(Error error) }
        | _ -> super#core_type ty t
    end
  in
  let aux t tp_name tp_in_return_type =
    match tp_in_return_type.ptyp_desc with
    | Ptyp_var var ->
      let data =
        let loc = tp_in_return_type.ptyp_loc in
        if Map.mem t.universal var
        then Error { loc; txt = "ppx_sexp_conv: duplicate variable" }
        else (
          match Map.find original.universal tp_name with
          | Some result -> result
          | None -> Error { loc; txt = "ppx_sexp_conv: unbound type parameter" })
      in
      { t with universal = Map.set t.universal ~key:var ~data }
    | _ -> add_typevars#core_type tp_in_return_type t
  in
  match cd.pcd_res with
  | None -> original
  | Some ty ->
    (match ty.ptyp_desc with
     | Ptyp_constr (_, params) ->
       if List.length params <> List.length tps
       then original
       else
         Stdlib.ListLabels.fold_left2
           tps
           params
           ~init:{ existential = true; universal = Map.empty (module String) }
           ~f:aux
     | _ -> original)
;;

let of_type_declaration decl ~prefix =
  { existential = false
  ; universal =
      List.fold
        decl.ptype_params
        ~init:(Map.empty (module String))
        ~f:(fun map param ->
          let name = get_type_param_name param in
          Map.update map name.txt ~f:(function
            | None -> Ok (Fresh_name.create (prefix ^ name.txt) ~loc:name.loc)
            | Some _ ->
              Error { loc = name.loc; txt = "ppx_sexp_conv: duplicate variable" }))
  }
;;

let without_type () = { existential = false; universal = Map.empty (module String) }
