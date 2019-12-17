(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)


(* Generate code to lift values of a certain type.
   This illustrates how to build fragments of Parsetree through
   Ast_helper and more local helper functions. *)

module Main : sig end = struct

  open Location
  open Types
  open Asttypes
  open Ast_helper
  open Ast_convenience

  let selfcall ?(this = "this") m args = app (Exp.send (evar this) (mknoloc m)) args

  (*************************************************************************)


  let env = Env.initial_safe_string

  let clean s =
    let s = Bytes.of_string s in
    for i = 0 to Bytes.length s - 1 do
      if Bytes.get s i = '.' then Bytes.set s i '_'
    done;
    Bytes.to_string s

  let print_fun s = "lift_" ^ clean s

  let printed = Hashtbl.create 16
  let meths = ref []
  let use_existentials = ref false
  let use_arrows = ref false

  let existential_method =
    Cf.(method_ (mknoloc "existential") Public
          (virtual_ Typ.(poly [mknoloc "a"] (arrow Nolabel (var "a") (var "res"))))
       )

  let arrow_method =
    Cf.(method_ (mknoloc "arrow") Public
          (virtual_ Typ.(poly [mknoloc "a"] (arrow Nolabel (var "a") (var "res"))))
       )

  let rec gen ty =
    if Hashtbl.mem printed ty then ()
    else let tylid = Longident.parse ty in
      let td =
        try Env.find_type (Env.lookup_type tylid env) env
        with Not_found ->
          Format.eprintf "** Cannot resolve type %s@." ty;
          exit 2
      in
      let prefix =
        let open Longident in
        match tylid with
        | Ldot (m, _) -> String.concat "." (Longident.flatten m) ^ "."
        | Lident _ -> ""
        | Lapply _ -> assert false
      in
      Hashtbl.add printed ty ();
      let params = List.mapi (fun i _ -> mknoloc (Printf.sprintf "f%i" i)) td.type_params in
      let env = List.map2 (fun s t -> t.id, evar s.txt) params td.type_params in
      let make_result_t tyargs = Typ.(arrow Asttypes.Nolabel (constr (lid ty) tyargs) (var "res")) in
      let make_t tyargs =
        List.fold_right
          (fun arg t ->
             Typ.(arrow Asttypes.Nolabel (arrow Asttypes.Nolabel arg (var "res")) t))
          tyargs (make_result_t tyargs)
      in
      let tyargs = List.map (fun t -> Typ.var t.txt) params in
      let t = Typ.poly params (make_t tyargs) in
      let concrete e =
        let e = List.fold_right (fun x e -> lam x e) (List.map (fun x -> pvar x.txt) params) e in
        let tyargs = List.map (fun t -> Typ.constr (lid t.txt) []) params in
        let e = Exp.constraint_ e (make_t tyargs) in
        let e = List.fold_right (fun x e -> Exp.newtype x e) params e in
        let body = Exp.poly e (Some t) in
        meths := Cf.(method_ (mknoloc (print_fun ty)) Public (concrete Fresh body)) :: !meths
      in
      let field ld =
        let s = Ident.name ld.ld_id in
        (lid (prefix ^ s), pvar s),
        tuple[str s; tyexpr env ld.ld_type (evar s)]
      in
      match td.type_kind, td.type_manifest with
      | Type_record (l, _), _ ->
          let l = List.map field l in
          concrete
            (lam
               (Pat.record (List.map fst l) Closed)
               (selfcall "record" [str ty; list (List.map snd l)]))
      | Type_variant l, _ ->
          let case cd =
            let c = Ident.name cd.cd_id in
            let qc = prefix ^ c in
            match cd.cd_args with
            | Cstr_tuple (tys) ->
                let p, args = gentuple env tys in
                pconstr qc p, selfcall "constr" [str ty; tuple[str c; list args]]
            | Cstr_record (l) ->
                let l = List.map field l in
                let keep_head ((lid, pattern), _) =
                      let txt = Longident.Lident (Longident.last lid.txt) in
                      ({lid with txt}, pattern)
                    in
                    pconstr qc [Pat.record (List.map keep_head l) Closed],
                    selfcall "constr"
                      [str ty;
                       tuple [str c;
                              list [selfcall "record"
                                      [str ""; list (List.map snd l)]]]]
              in
          concrete (func (List.map case l))
      | Type_abstract, Some t ->
          concrete (tyexpr_fun env t)
      | Type_abstract, None ->
          (* Generate an abstract method to lift abstract types *)
          meths := Cf.(method_ (mknoloc (print_fun ty)) Public (virtual_ t)) :: !meths
      | Type_open, _ ->
          failwith "Open types are not yet supported."

  and gentuple env tl =
    let arg i t =
      let x = Printf.sprintf "x%i" i in
      pvar x, tyexpr env t (evar x)
    in
    List.split (List.mapi arg tl)

  and tyexpr env ty x =
    match ty.desc with
    | Tvar _ ->
        (match List.assoc ty.id env with
         | f -> app f [x]
         | exception Not_found ->
             use_existentials := true;
             selfcall "existential" [x])
    | Ttuple tl ->
        let p, e = gentuple env tl in
        let_in [Vb.mk (Pat.tuple p) x] (selfcall "tuple" [list e])
    | Tconstr (path, [t], _) when Path.same path Predef.path_list ->
        selfcall "list" [app (evar "List.map") [tyexpr_fun env t; x]]
    | Tconstr (path, [t], _) when Path.same path Predef.path_array ->
        selfcall "array" [app (evar "Array.map") [tyexpr_fun env t; x]]
    | Tconstr (path, [], _) when Path.same path Predef.path_string ->
        selfcall "string" [x]
    | Tconstr (path, [], _) when Path.same path Predef.path_int ->
        selfcall "int" [x]
    | Tconstr (path, [], _) when Path.same path Predef.path_char ->
        selfcall "char" [x]
    | Tconstr (path, [], _) when Path.same path Predef.path_int32 ->
        selfcall "int32" [x]
    | Tconstr (path, [], _) when Path.same path Predef.path_int64 ->
        selfcall "int64" [x]
    | Tconstr (path, [], _) when Path.same path Predef.path_nativeint ->
        selfcall "nativeint" [x]
    | Tconstr (path, tl, _) ->
        let ty = Path.name path in
        gen ty;
        selfcall (print_fun ty) (List.map (tyexpr_fun env) tl @ [x])
    | Tarrow _ ->
        use_arrows := true;
        selfcall "arrow" [x]
    | _ ->
        Format.eprintf "** Cannot deal with type %a@." Printtyp.type_expr ty;
        exit 2

  and tyexpr_fun env ty =
    lam (pvar "x") (tyexpr env ty (evar "x"))

  let simplify =
    (* (fun x -> <expr> x) ====> <expr> *)
    let open Ast_mapper in
    let super = default_mapper in
    let expr this e =
      let e = super.expr this e in
      let open Longident in
      let open Parsetree in
      match e.pexp_desc with
      | Pexp_fun
          (Asttypes.Nolabel, None,
           {ppat_desc = Ppat_var{txt=id;_};_},
           {pexp_desc =
              Pexp_apply
                (f,
                 [Asttypes.Nolabel
                 ,{pexp_desc= Pexp_ident{txt=Lident id2;_};_}]);_})
        when id = id2 -> f
      | _ -> e
    in
    {super with expr}

  let args =
    let open Arg in
    [
      "-I", String (fun s -> Load_path.add_dir (Misc.expand_directory Config.standard_library s)),
      "<dir> Add <dir> to the list of include directories";
    ]

  let usage =
    Printf.sprintf "%s [options] <type names>\n" Sys.argv.(0)

  let main () =
    Load_path.init [Config.standard_library];
    Arg.parse (Arg.align args) gen usage;
    let meths = !meths in
    let meths =
      if !use_existentials then
        existential_method :: meths
      else
        meths
    in
    let meths =
      if !use_arrows then
        arrow_method :: meths
      else
        meths
    in
    let cl = Cstr.mk (pvar "this") meths in
    let params = [Typ.var "res", Invariant] in
    let cl = Ci.mk ~virt:Virtual ~params (mknoloc "lifter") (Cl.structure cl) in
    let s = [Str.class_ [cl]] in
    Format.printf "%a@." Pprintast.structure (simplify.Ast_mapper.structure simplify s)

  let () =
    try main ()
    with exn ->
      Printf.eprintf "** fatal error: %s\n%!" (Printexc.to_string exn)

end
