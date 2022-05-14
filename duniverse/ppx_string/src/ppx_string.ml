open Base
open Ppxlib
open Ast_builder.Default

module Where = struct
  type t =
    | Imprecise of Location.t
    | Precise of { mutable position : position }

  let is_precise = function
    | Imprecise _ -> false
    | Precise _ -> true
  ;;

  let advance position char =
    let pos_cnum = position.pos_cnum + 1 in
    match char with
    | '\n' ->
      { position with pos_lnum = position.pos_lnum + 1; pos_bol = pos_cnum; pos_cnum }
    | _ -> { position with pos_cnum }
  ;;

  let skip t string =
    match t with
    | Imprecise _ -> ()
    | Precise at ->
      for pos = 0 to String.length string - 1 do
        at.position <- advance at.position string.[pos]
      done
  ;;

  let loc_start = function
    | Imprecise loc -> loc.loc_start
    | Precise { position } -> position
  ;;

  let loc_end = function
    | Imprecise loc -> loc.loc_end
    | Precise { position } -> position
  ;;

  let skip_with_loc t string =
    let loc_start = loc_start t in
    skip t string;
    let loc_end = loc_end t in
    { loc_ghost = true; loc_start; loc_end }
  ;;

  let has_escapes ~loc ~string ~delimiter =
    match delimiter with
    | Some _ -> false
    | None ->
      let unescaped_len = 1 + String.length string + 1 in
      let actual_len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
      unescaped_len <> actual_len
  ;;

  let literal_prefix ~delimiter =
    match delimiter with
    | None -> "\""
    | Some id -> Printf.sprintf "{%s|" id
  ;;

  let create ~loc ~string ~delimiter =
    if has_escapes ~loc ~string ~delimiter
    then Imprecise { loc with loc_ghost = true }
    else (
      let t = Precise { position = loc.loc_start } in
      skip t (literal_prefix ~delimiter);
      t)
  ;;
end

let add_literal string ~where ~start ~until ~acc =
  if start >= until
  then acc
  else (
    let literal = String.sub string ~pos:start ~len:(until - start) in
    let loc = Where.skip_with_loc where literal in
    estring ~loc literal :: acc)
;;

let set_locs loc =
  object
    inherit Ast_traverse.map
    method! location _ = loc
  end
;;

let parse_error ~loc ~name string =
  Location.raise_errorf ~loc "invalid %s: %S" name string
;;

let parse_expression ~where ~loc ~name string =
  let lexbuf = Lexing.from_string string in
  lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc.loc_start;
  match Parse.expression lexbuf with
  | exception _ -> parse_error ~loc ~name string
  | expr -> if Where.is_precise where then expr else (set_locs loc)#expression expr
;;

let parse_ident ~where ~loc ~name module_path =
  (* We want to parse a module path here, such as [Core.Int]. Whitespace, comments, etc.
     have been allowed here historically.

     Parsing a module path using [Longident.parse] would be too restrictive and disallow
     whitespace, comments, etc.

     Parsing [module_path ^ ".to_string"] assigns locations incorrectly because of the
     synthetic suffix.

     So we parse [module_path] as an expression. A valid module path, used in an
     expression context, looks like a nullary variant constructor. So we convert nullary
     variant constructors to module paths, and reject all other expressions. *)
  match parse_expression ~where ~loc ~name module_path with
  | { pexp_desc = Pexp_construct (ident, None); _ } ->
    pexp_ident ~loc { ident with txt = Ldot (ident.txt, "to_string") }
  | _ -> parse_error ~loc ~name module_path
;;

let parse_body ~where string =
  let loc = Where.skip_with_loc where string in
  parse_expression ~where ~loc ~name:"#{...} expression" string
;;

let parse_module_path ~where string =
  let loc = Where.skip_with_loc where string in
  parse_ident ~where ~loc ~name:"%{...} module path" string
;;

let parse_pad_length ~where string =
  let loc = Where.skip_with_loc where string in
  parse_expression ~where ~loc ~name:"#{...} pad length" string
;;

let add_interpreted string ~where ~start ~until ~acc =
  Where.skip where "%{";
  let loc_start = Where.loc_start where in
  let expression =
    let string = String.sub string ~pos:start ~len:(until - start) in
    let value, module_path, pad_length =
      match String.rsplit2 string ~on:'#' with
      | None ->
        let value = parse_body ~where string in
        value, None, None
      | Some (body, formatting) ->
        let body = parse_body ~where body in
        Where.skip where "#";
        let module_path, pad_length =
          match String.rsplit2 formatting ~on:':' with
          | None ->
            let fn = parse_module_path ~where formatting in
            Some fn, None
          | Some (module_path, pad_length) ->
            let fn =
              if String.is_empty module_path
              then None
              else Some (parse_module_path ~where module_path)
            in
            Where.skip where ":";
            let len = parse_pad_length ~where pad_length in
            fn, Some len
        in
        body, module_path, pad_length
    in
    let unpadded =
      match module_path with
      | None -> fun ~loc:_ -> value
      | Some fn -> fun ~loc -> pexp_apply ~loc fn [ Nolabel, value ]
    in
    match pad_length with
    | None -> unpadded
    | Some len ->
      fun ~loc ->
        let ex_var = gen_symbol ~prefix:"__string_exp" () in
        let ex = evar ~loc ex_var in
        let lenvar = gen_symbol ~prefix:"__string_len" () in
        [%expr
          let [%p pvar ~loc ex_var] = [%e unpadded ~loc] in
          let [%p pvar ~loc lenvar] = Stdlib.String.length [%e ex] in
          Stdlib.( ^ )
            (Stdlib.String.make (Stdlib.max 0 ([%e len] - [%e evar ~loc lenvar])) ' ')
            [%e ex]]
  in
  let loc_end = Where.loc_end where in
  Where.skip where "}";
  expression ~loc:{ loc_ghost = true; loc_start; loc_end } :: acc
;;

type interpreted =
  { percent : int
  ; lbrace : int
  ; rbrace : int
  }

let find_interpreted string ~where ~pos =
  String.substr_index string ~pos ~pattern:"%{"
  |> Option.map ~f:(fun percent ->
    let lbrace = percent + 1 in
    match String.substr_index string ~pos:(lbrace + 1) ~pattern:"}" with
    | None ->
      Where.skip where (String.sub string ~pos ~len:(percent - pos));
      let loc = Where.skip_with_loc where "%{" in
      Location.raise_errorf ~loc "unterminated %%{"
    | Some rbrace -> { percent; lbrace; rbrace })
;;

let rec expand_expressions_from string ~where ~pos ~acc =
  match find_interpreted string ~where ~pos with
  | None ->
    let len = String.length string in
    let acc = add_literal string ~where ~start:pos ~until:len ~acc in
    List.rev acc
  | Some { percent; lbrace; rbrace } ->
    let acc = add_literal string ~where ~start:pos ~until:percent ~acc in
    let acc = add_interpreted string ~where ~start:(lbrace + 1) ~until:rbrace ~acc in
    expand_expressions_from string ~where ~pos:(rbrace + 1) ~acc
;;

let expand_expressions ~loc ~delimiter string =
  let where = Where.create ~loc ~delimiter ~string in
  expand_expressions_from string ~where ~pos:0 ~acc:[]
;;

let concatenate ~loc expressions =
  match expressions with
  | [] -> [%expr ""]
  | [ expr ] -> [%expr ([%e expr] : Stdlib.String.t)]
  | multiple -> [%expr Stdlib.String.concat "" [%e elist ~loc multiple]]
;;

let expand ~expr_loc ~string_loc ~string ~delimiter =
  concatenate ~loc:expr_loc (expand_expressions ~loc:string_loc ~delimiter string)
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_string"
    ~extensions:
      [ Extension.declare
          "ppx_string.string"
          Extension.Context.expression
          Ast_pattern.(
            pstr (pstr_eval (pexp_constant (pconst_string __' __ __)) nil ^:: nil))
          (fun ~loc:expr_loc ~path:_ { loc = string_loc; txt = string } _ delimiter ->
             Merlin_helpers.hide_expression
               (expand ~expr_loc ~string_loc ~string ~delimiter))
      ]
;;
