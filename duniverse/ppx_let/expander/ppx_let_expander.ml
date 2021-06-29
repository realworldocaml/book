open Base
open Ppxlib
open Ast_builder.Default

module List = struct
  include List

  let reduce_exn l ~f =
    match l with
    | [] -> invalid_arg "List.reduce_exn"
    | hd :: tl -> fold_left tl ~init:hd ~f
  ;;
end

module Extension_name = struct
  type t =
    | Sub
    | Sub_open
    | Bind
    | Bind_open
    | Bindn
    | Bindn_open
    | Map
    | Map_open
    | Mapn
    | Mapn_open

  let operator_name = function
    | Sub | Sub_open -> "sub"
    | Bind | Bind_open | Bindn | Bindn_open -> "bind"
    | Map | Map_open | Mapn | Mapn_open -> "map"
  ;;

  let to_string = function
    | Sub -> "sub"
    | Sub_open -> "sub_open"
    | Bind -> "bind"
    | Bindn -> "bindn"
    | Bindn_open -> "bindn_open"
    | Bind_open -> "bind_open"
    | Map -> "map"
    | Map_open -> "map_open"
    | Mapn -> "mapn"
    | Mapn_open -> "mapn_open"
  ;;
end

let let_syntax = "Let_syntax"

let let_syntax ~modul : Longident.t =
  match modul with
  | None -> Lident let_syntax
  | Some id -> Ldot (Ldot (id.txt, let_syntax), let_syntax)
;;

let open_on_rhs ~loc ~modul =
  pmod_ident ~loc (Located.mk ~loc (Longident.Ldot (let_syntax ~modul, "Open_on_rhs")))
;;

let eoperator ~loc ~modul func =
  let lid : Longident.t = Ldot (let_syntax ~modul, func) in
  pexp_ident ~loc (Located.mk ~loc lid)
;;

let expand_with_tmp_vars ~loc bindings expr ~f =
  match bindings with
  | [ _ ] -> f ~loc bindings expr
  | _ ->
    let tmp_vars =
      List.map bindings ~f:(fun _ -> gen_symbol ~prefix:"__let_syntax" ())
    in
    let s_rhs_tmp_var (* s/rhs/tmp_var *) =
      List.map2_exn bindings tmp_vars ~f:(fun vb var ->
        let loc = { vb.pvb_expr.pexp_loc with loc_ghost = true } in
        { vb with pvb_expr = evar ~loc var })
    in
    let s_lhs_tmp_var (* s/lhs/tmp_var *) =
      List.map2_exn bindings tmp_vars ~f:(fun vb var ->
        let loc = { vb.pvb_pat.ppat_loc with loc_ghost = true } in
        { vb with
          pvb_pat = pvar ~loc var
        ; pvb_loc = { vb.pvb_loc with loc_ghost = true }
        })
    in
    pexp_let ~loc Nonrecursive s_lhs_tmp_var (f ~loc s_rhs_tmp_var expr)
;;

let expand_with_op_n_function ~loc ~modul ~op_name bindings expr =
  let n = List.length bindings in
  let operator =
    match n with
    | 1 -> eoperator ~loc ~modul op_name
    | n -> eoperator ~loc ~modul (Printf.sprintf "%s%d" op_name n)
  in
  let bindings_args =
    bindings |> List.map ~f:(fun { pvb_expr; _ } -> Nolabel, pvb_expr)
  in
  let func =
    List.fold_right bindings ~init:expr ~f:(fun { pvb_pat; _ } lower ->
      pexp_fun ~loc Nolabel None pvb_pat lower)
  in
  let args = List.append bindings_args [ Labelled "f", func ] in
  pexp_apply ~loc operator args
;;

let bind_apply ~loc ~modul extension_name ~arg ~fn =
  pexp_apply
    ~loc
    (eoperator ~loc ~modul (Extension_name.operator_name extension_name))
    [ Nolabel, arg; Labelled "f", fn ]
;;

let maybe_open extension_name ~to_open:module_to_open expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  match (extension_name : Extension_name.t) with
  | Sub | Bind | Map | Mapn | Bindn -> expr
  | Sub_open | Bind_open | Bindn_open | Map_open | Mapn_open ->
    pexp_open ~loc (open_infos ~loc ~override:Override ~expr:(module_to_open ~loc)) expr
;;

let assert_bindings_length ~loc ~extension_name ~bindings =
  match extension_name, List.length bindings with
  | _, 0 -> invalid_arg "expand_let: list of bindings must be non-empty"
  | (Extension_name.Sub | Sub_open), n when n > 1 ->
    (* let%sub doesn't allow more than one binding *)
    Location.raise_errorf ~loc "let%%sub cannot be used with 'and'"
  | _ -> ()
;;

let expand_let extension_name ~loc ~modul bindings body =
  assert_bindings_length ~loc ~extension_name ~bindings;
  (* Build expression [both E1 (both E2 (both ...))] *)
  let nested_boths =
    let rev_boths = List.rev_map bindings ~f:(fun vb -> vb.pvb_expr) in
    List.reduce_exn rev_boths ~f:(fun acc e ->
      let loc = { e.pexp_loc with loc_ghost = true } in
      eapply ~loc (eoperator ~loc ~modul "both") [ e; acc ])
  in
  (* Build pattern [(P1, (P2, ...))] *)
  let nested_patterns =
    let rev_patts = List.rev_map bindings ~f:(fun vb -> vb.pvb_pat) in
    List.reduce_exn rev_patts ~f:(fun acc p ->
      let loc = { p.ppat_loc with loc_ghost = true } in
      ppat_tuple ~loc [ p; acc ])
  in
  bind_apply
    ~loc
    ~modul
    extension_name
    ~arg:nested_boths
    ~fn:(pexp_fun ~loc Nolabel None nested_patterns body)
;;

let expand_match extension_name ~loc ~modul expr cases =
  bind_apply
    ~loc
    ~modul
    extension_name
    ~arg:(maybe_open extension_name ~to_open:(open_on_rhs ~modul) expr)
    ~fn:(pexp_function ~loc cases)
;;

let expand_if extension_name ~loc expr then_ else_ =
  expand_match
    extension_name
    ~loc
    expr
    [ case ~lhs:(pbool ~loc true) ~guard:None ~rhs:then_
    ; case ~lhs:(pbool ~loc false) ~guard:None ~rhs:else_
    ]
;;

let expand_while ~loc ~modul ~cond ~body =
  let loop_name = gen_symbol ~prefix:"__let_syntax_loop" () in
  let ploop = pvar ~loc loop_name in
  let eloop = evar ~loc loop_name in
  let loop_call = pexp_apply ~loc eloop [ Nolabel, eunit ~loc ] in
  let loop_body =
    let then_ = bind_apply ~loc ~modul Bind ~arg:body ~fn:eloop in
    let else_ =
      pexp_apply ~loc (eoperator ~loc ~modul "return") [ Nolabel, eunit ~loc ]
    in
    expand_if ~modul Bind ~loc cond then_ else_
  in
  let loop_func = pexp_fun ~loc Nolabel None (punit ~loc) loop_body in
  pexp_let ~loc Recursive [ value_binding ~loc ~pat:ploop ~expr:loop_func ] loop_call
;;

let expand ~modul extension_name expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, expr) ->
      let bindings =
        List.map bindings ~f:(fun vb ->
          let pvb_pat =
            (* Temporary hack tentatively detecting that the parser
               has expanded `let x : t = e` into `let x : t = (e : t)`.

               For reference, here is the relevant part of the parser:
               https://github.com/ocaml/ocaml/blob/4.07/parsing/parser.mly#L1628 *)
            match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
            | ( Ppat_constraint (p, { ptyp_desc = Ptyp_poly ([], t1); _ })
              , Pexp_constraint (_, t2) )
              when phys_equal t1 t2 || Poly.equal t1 t2 -> p
            | _ -> vb.pvb_pat
          in
          { vb with
            pvb_pat
          ; pvb_expr =
              maybe_open extension_name ~to_open:(open_on_rhs ~modul) vb.pvb_expr
          })
      in
      (match extension_name with
       | Mapn | Bindn | Mapn_open | Bindn_open ->
         expand_with_tmp_vars
           ~loc
           bindings
           expr
           ~f:
             (expand_with_op_n_function
                ~modul
                ~op_name:(Extension_name.operator_name extension_name))
       | Sub | Sub_open | Bind | Bind_open | Map | Map_open ->
         expand_with_tmp_vars ~loc bindings expr ~f:(expand_let extension_name ~modul))
    | Pexp_let (Recursive, _, _) ->
      Location.raise_errorf
        ~loc
        "'let%%%s' may not be recursive"
        (Extension_name.to_string extension_name)
    | Pexp_match (expr, cases) ->
      (match extension_name with
       | Sub | Sub_open -> Location.raise_errorf ~loc "match%%sub is not supported"
       | _ -> expand_match extension_name ~loc ~modul expr cases)
    | Pexp_ifthenelse (expr, then_, else_) ->
      let else_ =
        match else_ with
        | Some else_ -> else_
        | None ->
          Location.raise_errorf
            ~loc
            "'if%%%s' must include an else branch"
            (Extension_name.to_string extension_name)
      in
      (match extension_name with
       | Sub | Sub_open -> Location.raise_errorf ~loc "if%%sub is not supported"
       | _ -> expand_if extension_name ~loc ~modul expr then_ else_)
    | Pexp_while (cond, body) ->
      (match (extension_name : Extension_name.t) with
       | Map | Map_open | Mapn | Mapn_open ->
         Location.raise_errorf
           ~loc
           "while%%map is not supported. use while%%bind instead."
       | Bindn | Bindn_open ->
         Location.raise_errorf
           ~loc
           "while%%bindn is not supported. use while%%bind instead."
       | Sub | Sub_open -> Location.raise_errorf ~loc "while%%sub is not supported"
       | Bind | Bind_open -> expand_while ~loc ~modul ~cond ~body)
    | _ ->
      Location.raise_errorf
        ~loc
        "'%%%s' can only be used with 'let', 'match', 'while', and 'if'"
        (Extension_name.to_string extension_name)
  in
  { expansion with pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes }
;;
