open Base
open Stdio
open Ppxlib
open Ast_builder.Default

module Filename = Caml.Filename
module Env = Interpreter.Env
module Value = Interpreter.Value


module Of_item = struct
  (* boilerplate code to pull extensions out of different ast nodes *)
  open Token

  let directive_or_block_of_ext ~item ({ txt = ext_name; loc }, payload) attrs =
    match Directive.of_string_opt ext_name with
    | None -> (* not one of our extensions *) Block [item]
    | Some dir ->
      assert_no_attributes attrs;
      Directive (dir, loc, payload)

  let structure item = match item.pstr_desc with
    | Pstr_extension (ext, attrs) -> directive_or_block_of_ext ~item ext attrs
    | _ -> Block [item]

  let signature item = match item.psig_desc with
    | Psig_extension (ext, attrs) -> directive_or_block_of_ext ~item ext attrs
    | _ -> Block [item]

  let class_structure item = match item.pcf_desc with
    | Pcf_extension ext -> directive_or_block_of_ext ~item ext []
    | _ -> Block [item]

  let class_signature item = match item.pctf_desc with
    | Pctf_extension ext -> directive_or_block_of_ext ~item ext []
    | _ -> Block [item]

end

module Ast_utils = struct

  let get_expr ~loc payload =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (e, attrs); _ }] ->
      assert_no_attributes attrs;
      e
    | _ ->
      Location.raise_errorf ~loc
        "optcomp: invalid directive syntax, expected single expression."

  let assert_no_arguments ~loc payload =
    match payload with
    | PStr [] -> ()
    | _ ->
      Location.raise_errorf ~loc
        "optcomp: invalid directive syntax, expected no arguments."

  let make_apply_fun ~loc name expr =
    let iname = { txt = Lident name; loc } in
    eapply ~loc (pexp_ident ~loc iname) [expr]

  let get_ident ~loc payload =
    let e = get_expr ~loc payload in
    Interpreter.lid_of_expr e

  let get_var ~loc payload =
    let e = get_expr ~loc payload in
    Interpreter.var_of_expr e

  let get_var_expr ~loc payload =
    let apply_e = get_expr ~loc payload in
    match apply_e.pexp_desc with
    | Pexp_apply (var_e, [Nolabel, val_e]) -> Interpreter.var_of_expr var_e, Some val_e
    | Pexp_construct (var_li, Some val_e) -> Interpreter.var_of_lid var_li, Some val_e
    | Pexp_apply (var_e, []) -> Interpreter.var_of_expr var_e, None
    | Pexp_construct (var_li, None) -> Interpreter.var_of_lid var_li, None
    | _ ->
      Location.raise_errorf ~loc
        "optcomp: invalid directive syntax, expected var and expr"

  let get_string ~loc payload =
    let e = get_expr ~loc payload in
    match e with
    | { pexp_desc = Pexp_constant (Pconst_string (x, _ )); _ } -> x
    | _ -> Location.raise_errorf ~loc "optcomp: invalid directive syntax, expected string"

end

module Token_stream : sig
  type 'a t = 'a Token.t list

  val of_items : 'a list -> of_item:('a -> 'a Token.t) -> 'a t
end = struct

  type 'a t = 'a Token.t list

  type ftype = Ocaml | C

  let resolve_import ~loc ~filename : string * ftype =
    let ext = Filename.extension (Filename.basename filename) in
    let ftype = match ext with
      | ".ml" | ".mlh" -> Ocaml
      | ".h" -> C
      | _ -> Location.raise_errorf ~loc "optcomp: unknown file extension: %s\n\
                                         Must be one of: .ml, .mlh or .h." ext
    in
    let fbase = Filename.dirname loc.loc_start.pos_fname in
    let fpath =
      if Filename.is_relative filename
      then Filename.concat fbase filename
      else filename
    in
    (fpath, ftype)

  let import_open ~loc payload =
    let filename = Ast_utils.get_string ~loc payload in
    let fpath, ftype = resolve_import ~loc ~filename in
    let in_ch =
      try In_channel.create fpath
      with exn ->
        let msg = match exn with
          | Sys_error msg -> msg
          | _ -> Exn.to_string exn
        in
        Location.raise_errorf ~loc "optcomp: cannot open imported file: %s: %s" fpath msg
    in
    (* disable old optcomp on imported files, or it consumes all variables :( *)
    Lexer.set_preprocessor (fun () -> ()) (fun x -> x);
    let lexbuf = Lexing.from_channel in_ch in
    lexbuf.lex_curr_p <- { pos_fname = fpath; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    in_ch, lexbuf, ftype

 let unroll (stack : 'a Token.t list) : ('a Token.t * 'a Token.t list) =
   let bs, _, rest_rev =
     List.fold stack ~init:([], false, []) ~f:(fun (bs, found, rest) x ->
       match x, found with
       | Block b, false -> b @ bs, false, rest
       | _ -> bs, true, x :: rest
     )
   in
   Block bs, List.rev rest_rev

 let rec of_items : 'a. 'a list -> of_item:('a -> 'a Token.t) -> 'a t =
   fun items ~of_item ->
     let of_items_st x = of_items ~of_item:Of_item.structure x in
     let tokens_rev =
       List.fold items ~init:[] ~f:(fun acc item ->
         match of_item item with
         | Directive (dir, loc, payload) as token ->
           let last_block, rest = unroll acc in
           begin match dir with
           | Import ->
             let in_ch, lexbuf, ftype = import_open ~loc payload in
             let new_tokens =
               match ftype with
               | C -> Cparser.parse_loop lexbuf
               | Ocaml ->
                 let st_items = Parse.implementation lexbuf in
                 Token.just_directives_exn ~loc (of_items_st st_items)
             in
             In_channel.close in_ch;
             List.rev new_tokens @ (last_block :: rest)
           | _ -> token :: last_block :: rest
           end
         | _ -> begin match acc with
           | Block items :: acc -> Block (items @ [item]) :: acc
           | _ -> Block [item] :: acc
         end
       )
     in
     List.rev tokens_rev
end

module Meta_ast : sig
  type 'a t

  val of_tokens : 'a Token.t list -> 'a t
  val eval
    :  drop_item:('a -> unit)
    -> eval_item:(Env.t -> 'a -> 'a)
    -> env:Env.t
    -> 'a t
    -> Env.t * 'a list
  val attr_mapper :
    to_loc:('a -> location)
    -> to_attrs:('a -> attributes)
    -> replace_attrs:('a -> attributes -> 'a)
    -> env:Env.t
    -> 'a
    -> 'a option

end = struct

  open Ast_utils

  type 'a t =
    | Leaf of 'a list
    | If of expression * 'a t * 'a t
    | Block of 'a t list
    | Define of string Location.loc * expression option
    | Undefine of string Location.loc
    | Import of string Location.loc
    | Error of string Location.loc
    | Warning of string Location.loc

  type 'a partial_if =
    | EmptyIf of ('a t -> 'a t -> 'a t) (* [If] waiting for both blocks *)
    | PartialIf of ('a t -> 'a t)       (* [If] waiting for else block *)

  type 'a temp_ast =
    | Full of 'a t
    | Partial of 'a partial_if loc

  let deprecated_ifs ~loc =
    Location.raise_errorf ~loc "optcomp: elif(n)def is deprecated, use elif defined()."

  let unroll_exn ~loc (acc:'a temp_ast list) : ('a t * 'a partial_if * 'a temp_ast list) =
    (* split by first EmptyIf/PartialIf *)
    let pre, if_fun, post = List.fold acc ~init:([], None, []) ~f:(
      fun (pre, found, post) x ->
        match found with
        | Some _ -> pre, found, x::post
        | None -> match x with
          | Partial { txt = f; _} -> pre, Some f, post
          | Full ast -> ast::pre, None, post
    ) in match if_fun with
    | None -> Location.raise_errorf ~loc "optcomp: else/endif/elif outside of if"
    | Some f -> Block pre, f, List.rev post

  let make_if ~loc cond =
    let if_fun ast1 ast2 = If (cond, ast1, ast2) in
    Partial { txt = (EmptyIf if_fun); loc }

  let of_tokens (tokens: 'a Token.t list) : ('a t) =
    let pre_parsed =
      List.fold tokens ~init:([] : 'a temp_ast list) ~f:(fun acc token ->
        match token with
        | Token.Block [] -> acc
        | Token.Block b -> Full (Leaf b) :: acc
        | Token.Directive (dir, loc, payload) ->
          match dir with
          | If -> make_if ~loc (get_expr ~loc payload) :: acc
          | Endif ->
            assert_no_arguments ~loc payload;
            let (last_block, if_fun, tail) = unroll_exn ~loc acc in
            begin match if_fun with
            | PartialIf f -> Full (f last_block) :: tail
            | EmptyIf f -> Full (f last_block (Block [])) :: tail
            end
          | Elif ->
            let cond = get_expr ~loc payload in
            let (last_block, if_fun, tail) = unroll_exn ~loc acc in
            begin match if_fun with
            | EmptyIf f ->
              let new_if_fun ast1 ast2 = f last_block (If (cond, ast1, ast2)) in
              Partial { txt = (EmptyIf new_if_fun); loc } :: tail
            | PartialIf _ ->
              Location.raise_errorf ~loc "optcomp: elif after else clause."
            end
          | Else ->
            assert_no_arguments ~loc payload;
            let (last_block, if_fun, tail) = unroll_exn ~loc acc in
            begin match if_fun with
            | EmptyIf f -> Partial { txt = PartialIf (f last_block); loc } :: tail
            | PartialIf _ ->
              Location.raise_errorf ~loc "optcomp: second else clause."
            end

          | Define ->
            let ident, expr = get_var_expr ~loc payload in
            Full (Define (ident, expr)) :: acc
          | Undef -> Full (Undefine (get_var ~loc payload)) :: acc
          | Error -> Full (Error { txt = (get_string ~loc payload); loc }) :: acc
          | Warning -> Full (Warning { txt = (get_string ~loc payload); loc }) :: acc
          | Import -> Full (Import { txt = (get_string ~loc payload); loc }) :: acc
          | Ifdef ->
            let ident = pexp_ident ~loc (get_ident ~loc payload) in
            let expr = make_apply_fun ~loc "defined" ident in
            make_if ~loc expr :: acc
          | Ifndef ->
            let ident = pexp_ident ~loc (get_ident ~loc payload) in
            let expr = make_apply_fun ~loc "not_defined" ident in
            make_if ~loc expr :: acc
          | Elifdef -> deprecated_ifs ~loc
          | Elifndef -> deprecated_ifs ~loc
      )
    in
    let extract_full = function
      | Full x -> x
      | Partial { loc; _ } -> Location.raise_errorf ~loc "optcomp: unterminated if"
    in
    Block (List.rev_map pre_parsed ~f:extract_full)

  let eval ~drop_item ~eval_item ~env ast =
    let rec drop ast = match ast with
      | Leaf l -> List.iter l ~f:drop_item
      | Block (ast::asts) -> drop ast; drop (Block asts)
      | If (cond, ast1, ast2) -> begin
        Attribute.explicitly_drop#expression cond;
        drop ast1;
        drop ast2
      end
      | _ -> ()
    in
    let rec aux_eval ~env (ast : 'a t) : (Env.t * 'a list list) =
      match ast with
      | Leaf l ->
        let l' = List.map l ~f:(eval_item env) in
        env, [l']
      | Block (ast::asts) ->
        let (new_env, res) = aux_eval ~env ast in
        let (newer_env, ress) = aux_eval ~env:new_env (Block asts) in
        newer_env, res @ ress
      | Block [] -> env, []
      | Define (ident, Some expr) ->
        Env.add env ~var:ident ~value:(Interpreter.eval env expr), []
      | Define (ident, None) -> Env.add env ~var:ident ~value:(Value.Tuple []), []
      | Undefine ident -> Env.undefine env ident, []
      | Import { loc; _ } ->
        Location.raise_errorf ~loc "optcomp: import not supported in this context."
      | If (cond, ast1, ast2) ->
        let cond =
          (* Explicitely allow the following pattern:
             {[
               [%%ifndef FOO]
               [%%define FOO]
             ]}
          *)
          match cond.pexp_desc, ast1 with
          | Pexp_apply (
              { pexp_desc = Pexp_ident { txt = Lident "not_defined"; _ }; _ },
              [Nolabel, ({ pexp_desc = Pexp_ident { txt = Lident i1; loc }; _ } as expr)]
            ),
            Block (Define ({ txt = i2; _}, None) :: _)
            when String.(=) i1 i2 ->
            make_apply_fun ~loc "not_defined_permissive" expr
          | _ -> cond
        in
        begin match (Interpreter.eval env cond) with
        | Bool b ->
          drop (if b then ast2 else ast1);
          aux_eval ~env (if b then ast1 else ast2)
        | v ->
          Location.raise_errorf ~loc:cond.pexp_loc
            "optcomp: if condition evaluated to non-bool: %s" (Value.to_string v)
        end
      | Error { loc; txt } -> Location.raise_errorf ~loc "%s" txt
      | Warning { txt; loc } ->
        let ppf = Caml.Format.err_formatter in
        Caml.Format.fprintf ppf "%a:@.Warning %s@." Location.print loc txt;
        env, []
    in
    let new_env, res = aux_eval ~env ast in
    (new_env, List.join res)

  let attr_mapper ~to_loc ~to_attrs ~replace_attrs ~env item =
    let loc = to_loc item in
    let is_our_attribute ({ txt; _}, _) = Token.Directive.matches txt ~expected:"if" in
    let our_as, other_as = List.partition_tf (to_attrs item) ~f:is_our_attribute in
    match our_as with
    | [] -> Some item
    | [({ loc; _}, payload) as our_a] ->
      Attribute.mark_as_handled_manually our_a;
      begin match Interpreter.eval env (get_expr ~loc payload) with
      | Bool b -> if b then Some (replace_attrs item other_as) else None
      | v ->
        Location.raise_errorf ~loc
          "optcomp: if condition evaulated to non-bool: %s" (Value.to_string v)
      end
    | _ ->
      Location.raise_errorf ~loc "optcomp: multiple [@if] attributes are not allowed"

end

let rewrite ~drop_item ~eval_item ~of_item ~env (x : 'a list) : Env.t * 'a list =
  let tokens : ('a Token.t list) = Token_stream.of_items x ~of_item in
  let ast = Meta_ast.of_tokens tokens in
  Meta_ast.eval ~drop_item ~eval_item ~env ast
;;

let map =
  object(self)
    inherit [Env.t] Ast_traverse.map_with_context as super

    method structure_gen env x =
      rewrite x ~env ~drop_item:Attribute.explicitly_drop#structure_item
        ~eval_item:self#structure_item
        ~of_item:Of_item.structure

    method signature_gen env x =
      rewrite x ~env ~drop_item:Attribute.explicitly_drop#signature_item
        ~eval_item:self#signature_item
        ~of_item:Of_item.signature

    method! structure env x =
      snd (self#structure_gen env x)

    method! signature env x =
      snd (self#signature_gen env x)

    method! class_structure env x =
      let _, rewritten =
        rewrite x.pcstr_fields ~env ~drop_item:Attribute.explicitly_drop#class_field
          ~eval_item:self#class_field
          ~of_item:Of_item.class_structure
      in
      { x with pcstr_fields = rewritten }

    method! class_signature env x =
      let _, rewritten =
        rewrite x.pcsig_fields ~env
          ~drop_item:Attribute.explicitly_drop#class_type_field
          ~eval_item:self#class_type_field
          ~of_item:Of_item.class_signature
      in
      { x with pcsig_fields = rewritten }

    method! type_kind env x =
      let x =
        match x with
        | Ptype_variant cs ->
          let f =
            Meta_ast.attr_mapper ~env
              ~to_loc:(fun c -> c.pcd_loc)
              ~to_attrs:(fun c -> c.pcd_attributes)
              ~replace_attrs:(fun c attrs -> {c with pcd_attributes = attrs})
          in
          let filtered_cs = List.filter_map cs ~f in
          Ptype_variant filtered_cs
        | _ -> x
      in
      super#type_kind env x

    method! expression_desc env x =
      let f =
        Meta_ast.attr_mapper ~env
          ~to_loc:(fun c -> c.pc_lhs.ppat_loc)
          ~to_attrs:(fun c -> c.pc_lhs.ppat_attributes)
          ~replace_attrs:(fun ({ pc_lhs; _} as c) attrs ->
            {c with pc_lhs = { pc_lhs with ppat_attributes = attrs}}
          )
      in
      let x =
        match x with
        | Pexp_function cs -> Pexp_function (List.filter_map cs ~f)
        | Pexp_match (e, cs) -> Pexp_match (super#expression env e, List.filter_map cs ~f)
        | Pexp_try (e, cs) -> Pexp_try (super#expression env e, List.filter_map cs ~f)
        | _ -> x
      in
      super#expression_desc env x
  end
;;

(* Preserve the enrivonment between invocation using cookies *)
let state = ref Env.init
let () =
  Driver.Cookies.add_simple_handler "ppx_optcomp.env"
    Ast_pattern.__
    ~f:(function
      | None   -> state := Env.init
      | Some x -> state := Interpreter.EnvIO.of_expression x);
  Driver.Cookies.add_post_handler (fun cookies ->
    Driver.Cookies.set cookies "ppx_optcomp.env"
      (Interpreter.EnvIO.to_expression !state))
;;

let preprocess ~f x =
  let new_env, x = f !state x in
  state := new_env;
  x
;;

let () =
  Driver.register_transformation "optcomp"
    ~preprocess_impl:(preprocess ~f:map#structure_gen)
    ~preprocess_intf:(preprocess ~f:map#signature_gen)
;;
