open Base
open Ppxlib
open Ast_builder.Default

module Part = struct
  type t =
    | String of string loc
    | Expr of
        { expr : string loc
        ; converter : string loc option
        }
end

(** [relative_position ~base s ~offset] computes the absolute position of [offset] in the
    string [s] assuming the beginning of the string is at position [base] *)
let relative_position ~base:pos s ~offset =
  String.foldi s ~init:pos ~f:(fun i (pos : position) cur ->
    if i >= offset
    then pos
    else (
      match cur with
      | '\n' ->
        { pos with
          pos_lnum = pos.pos_lnum + 1
        ; pos_bol = pos.pos_cnum + 1
        ; pos_cnum = pos.pos_cnum + 1
        }
      | _ -> { pos with pos_cnum = pos.pos_cnum + 1 }))
;;

(** [relative_location ~base ~start ~end_] compute the absolute location of the relative
    location \[start end_\) in the string [base.txt] *)
let relative_location ~base:{ loc; txt } ~start ~end_ =
  { loc with
    loc_start = relative_position ~base:loc.loc_start txt ~offset:start
  ; loc_end = relative_position ~base:loc.loc_start txt ~offset:end_
  }
;;

let to_parts (s : string loc) =
  let string start end_ =
    Part.String
      { txt = String.sub s.txt ~pos:start ~len:(end_ - start)
      ; loc = relative_location ~base:s ~start ~end_
      }
  in
  let rec loop acc pos =
    match String.substr_index ~pos s.txt ~pattern:"%{" with
    | None -> string pos (String.length s.txt) :: acc
    | Some start ->
      let acc = string pos start :: acc in
      let pos = start + 2 in
      let end_ =
        match String.index_from s.txt pos '}' with
        | None -> Location.raise_errorf ~loc:s.loc "unterminated %%{"
        | Some end_ -> end_
      in
      let string_expr = String.sub s.txt ~pos ~len:(end_ - pos) in
      let acc =
        match String.rsplit2 ~on:'#' string_expr with
        | None ->
          let loc = relative_location ~base:s ~start:pos ~end_ in
          Part.Expr { expr = { txt = string_expr; loc }; converter = None } :: acc
        | Some (string_expr, conversion_module) ->
          let conv =
            { txt = conversion_module ^ ".to_string"
            ; loc =
                relative_location
                  ~base:s
                  ~start:(pos + String.length string_expr + 1)
                  ~end_
            }
          in
          Part.Expr
            { expr =
                { txt = string_expr
                ; loc =
                    relative_location
                      ~base:s
                      ~start:pos
                      ~end_:(pos + String.length string_expr)
                }
            ; converter = Some conv
            }
          :: acc
      in
      loop acc (end_ + 1)
  in
  List.rev (loop [] 0)
;;


let to_parts { loc; txt = s } =
  if String.equal loc.loc_start.pos_fname loc.loc_end.pos_fname
  && Caml.Sys.file_exists loc.loc_start.pos_fname
  then (
    match
      Stdio.In_channel.with_file loc.loc_start.pos_fname ~f:(fun ic ->
        Stdio.In_channel.seek ic (Int64.of_int loc.loc_start.pos_cnum);
        let buf_len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
        let buf = Bytes.create buf_len in
        Stdio.In_channel.really_input_exn ic ~buf ~pos:0 ~len:buf_len;
        Bytes.to_string buf)
    with
    | s_from_file ->
      let from_ast = to_parts { loc; txt = s } in
      let from_file = to_parts { loc; txt = s_from_file } in
      (* If we have access to the original file, we extract location from it. *)
      (* Ideally, one should check that [from_file] and [from_ast] are equal (modulo
         encoding in strings). Note that we only check the general shapes are equal.
         The worse that can happen here is an error message with slightly incorrect
         locations. *)
      List.zip_exn from_ast from_file
      |> List.map ~f:(fun ((x, y) : Part.t * Part.t) ->
        match x, y with
        | String { txt; _ }, String { txt = _raw_string; loc } ->
          Part.String { txt; loc }
        | Expr { expr = e1; converter = c1 }, Expr { expr = e2; converter = c2 } ->
          assert (Bool.equal (Option.is_some c1) (Option.is_some c2));
          Part.Expr
            { expr = { e1 with loc = e2.loc }
            ; converter =
                Option.map2 c1 c2 ~f:(fun c1 c2 -> { c1 with loc = c2.loc })
            }
        | String _, Expr _ | Expr _, String _ -> assert false)
    | exception _ -> to_parts { loc; txt = s })
  else to_parts { loc; txt = s }
;;

let parse (tokens : Part.t list) =
  let parse_expression ~loc string =
    let lexbuf = Lexing.from_string string in
    lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
    lexbuf.lex_curr_p <- loc.loc_start;
    Ppxlib.Parse.expression lexbuf
  in
  List.filter_map tokens ~f:(function
    | Part.String { txt = ""; _ } -> None
    | String { txt; loc } -> Some (estring ~loc txt)
    | Expr { expr; converter } ->
      let e = parse_expression ~loc:expr.loc expr.txt in
      (match converter with
       | None -> Some e
       | Some conv ->
         let conversion_module = parse_expression ~loc:conv.loc conv.txt in
         Some
           (eapply
              ~loc:{ expr.loc with loc_end = conv.loc.loc_end }
              conversion_module
              [ e ])))
;;

let concat ~loc = function
  | [] -> estring ~loc ""
  | [ x ] -> x
  | _ :: _ :: _ as l ->
    eapply ~loc (evar ~loc "Stdlib.String.concat") [ estring ~loc ""; elist ~loc l ]
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_string"
    ~extensions:
      [ Extension.declare
          "ppx_string.string"
          Extension.Context.expression
          Ast_pattern.(pstr (pstr_eval (estring __') nil ^:: nil))
          (fun ~loc:_ ~path:_ sym ->
             let tokens = to_parts sym in
             let exprs = parse tokens in
             Merlin_helpers.hide_expression (concat ~loc:sym.loc exprs))
      ]
;;
