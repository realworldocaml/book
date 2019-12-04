open Base
open Ppxlib

let annotated_ignores = ref false;;
let check_comments = ref false;;
let compat_32 = ref false

let errorf ~loc fmt =
  Location.raise_errorf ~loc
    (Caml.(^^) "Jane Street style: " fmt)
;;

module Ignored_reason = struct
  type t = Argument_to_ignore | Underscore_pattern
  let fail ~loc _t =
    errorf ~loc "Ignored expression must come with a type annotation"
end

module Invalid_deprecated = struct
  type t =
    | Not_a_string
    | Missing_date
    | Invalid_month
  let fail ~loc = function
    | Not_a_string ->
      errorf ~loc "Invalid [@@deprecated payload], must be a string"
    | Missing_date ->
      errorf ~loc "deprecated message must start with the date in this format: \
                   [since YYYY-MM]"
    | Invalid_month ->
      errorf ~loc "invalid month in deprecation date"
end

module Invalid_constant = struct
  type t = string * string
  let fail ~loc ((s, typ) : t) =
    Location.raise_errorf ~loc
      "Integer literal %s exceeds the range of representable \
       integers of type %s on 32bit architectures" s typ
end

type error =
  | Invalid_deprecated of Invalid_deprecated.t
  | Missing_type_annotation of Ignored_reason.t
  | Invalid_constant of Invalid_constant.t
  | Docstring_on_open

let fail ~loc = function
  | Invalid_deprecated e -> Invalid_deprecated.fail e ~loc
  | Missing_type_annotation e -> Ignored_reason.fail e ~loc
  | Invalid_constant e -> Invalid_constant.fail e ~loc
  | Docstring_on_open ->
    errorf ~loc
      "A documentation comment is attached to this [open] which will be dropped by odoc."
;;

let check_deprecated_string ~f ~loc s =
  match Caml.Scanf.sscanf s "[since %u-%u]" (fun y m -> (y, m)) with
  | exception _ -> f ~loc (Invalid_deprecated Missing_date)
  | (_year, month) ->
    if month = 0 || month > 12 then f ~loc (Invalid_deprecated Invalid_month)
;;

let not_really_a_binding ~ext_name:s =
  List.mem [
    "test"; "test_unit"; "test_module";
    "bench"; "bench_fun"; "bench_module";
    "expect"; "expect_test";
  ] s ~equal:String.equal
;;

let ignored_expr_must_be_annotated ignored_reason (expr : Parsetree.expression) ~f =
  match expr.pexp_desc with
  (* explicitely annotated -> good *)
  | Pexp_constraint _
  | Pexp_coerce _
  (* no need to warn people trying to silence other warnings *)
  | Pexp_construct _
  | Pexp_ident _
  | Pexp_fun _
  | Pexp_function _
    -> ()
  | _ -> f ~loc:expr.pexp_loc (Missing_type_annotation ignored_reason)
;;

let constant_with_loc =
  let max_int_31 = Int32.(-) (Int32.shift_left 1l 30) 1l in
  let min_int_31 = Int32.neg (Int32.shift_left 1l 30) in
  fun ~loc c ->
  if !compat_32
  then match c with
    | Pconst_integer (s,Some 'n') ->
      begin
        try ignore (Int32.of_string s)
        with _ ->
          fail ~loc (Invalid_constant (s, "nativeint"))
      end
    | Pconst_integer (s,None) ->
      begin
        try
          let i = Int32.of_string s in
          if Int32.(i < min_int_31 || i > max_int_31) then failwith "out of bound"
        with _ ->
          fail ~loc (Invalid_constant (s, "int"))
      end
    | _ -> ()

let is_deprecated = function
  | "ocaml.deprecated" | "deprecated" -> true
  | _ -> false

let check_deprecated attr =
  if is_deprecated (fst attr).txt then
    errorf ~loc:(loc_of_attribute attr)
      "Invalid deprecated attribute, it will be ignored by the compiler"

let iter_style_errors ~f = object (self)
  inherit Ast_traverse.iter as super

  method! attribute (name, payload) =
    let loc = loc_of_attribute (name, payload) in
    if !Dated_deprecation.enabled && is_deprecated name.txt then
      match
        Ast_pattern.(parse (single_expr_payload (estring __'))) loc payload (fun s -> s)
      with
      | exception _ -> f ~loc (Invalid_deprecated Not_a_string)
      | { Location. loc; txt = s } -> check_deprecated_string ~f ~loc s

    method! open_description od =
      if !check_comments then (
        let has_doc_comments =
          List.exists od.popen_attributes ~f:(fun (attr_name, _) ->
            match attr_name.txt with
            | "ocaml.doc" | "doc" -> true
            | _ -> false
          )
        in
        if has_doc_comments then f ~loc:od.popen_loc Docstring_on_open
      );
      super#open_description od

  method! value_binding vb =
    if !annotated_ignores then (
      let loc = vb.Parsetree.pvb_loc in
      match Ast_pattern.(parse ppat_any) loc vb.Parsetree.pvb_pat () with
      | exception _ -> ()
      | () -> ignored_expr_must_be_annotated Underscore_pattern ~f vb.Parsetree.pvb_expr
    );
    super#value_binding vb

  method! extension (ext_name, payload as ext) =
    if not !annotated_ignores then super#extension ext else
    if not (not_really_a_binding ~ext_name:ext_name.Location.txt) then
      self#payload payload
    else
      (* We want to allow "let % test _ = ..." (and similar extensions which don't
        actually bind) without warning. *)
      match payload with
      | PStr str ->
        let check_str_item i =
          let loc = i.Parsetree.pstr_loc in
          Ast_pattern.(parse (pstr_value __ __)) loc i
            (fun _rec_flag vbs ->
              List.iter ~f:super#value_binding vbs)
        in
        List.iter ~f:check_str_item str
      | _ ->
        super#payload payload

  method! expression e =
    if !annotated_ignores then (
      match e with
      | [%expr ignore [%e? ignored]] ->
        ignored_expr_must_be_annotated Argument_to_ignore ~f ignored
      | _ -> ()
    );
    begin match e with
    | {pexp_desc = Pexp_constant c; pexp_loc; _ } ->
      constant_with_loc ~loc:pexp_loc c
    | _ -> ()
    end;
    super#expression e

  method! pattern e =
    begin match e with
    | {ppat_desc = Ppat_constant c; ppat_loc; _ } ->
      constant_with_loc ~loc:ppat_loc c
    | _ -> ()
    end;
    super#pattern e

  method! core_type t =
    List.iter t.ptyp_attributes ~f:check_deprecated;
    super#core_type t
end

let check = iter_style_errors ~f:fail

module Comments_checking = struct
  let errorf ~loc fmt =
    Location.raise_errorf ~loc (Caml.(^^) "Documentation error: " fmt)

  (* Assumption in the following functions: [s <> ""] *)

  let is_cr_comment s =
    let s = String.strip s in
    (String.is_prefix s ~prefix:"CR") ||
    (String.is_prefix s ~prefix:"XX") ||
    (String.is_prefix s ~prefix:"XCR") ||
    (String.is_prefix s ~prefix:"JS-only")

  let is_cinaps s = Char.equal s.[0] '$'

  let is_doc_comment s = Char.equal s.[0] '*'

  let is_ignored_comment s = Char.equal s.[0] '_'

  let can_appear_in_mli s = is_doc_comment s || is_ignored_comment s || is_cr_comment s || is_cinaps s

  let syntax_check_doc_comment ~loc comment =
    match Octavius.parse (Lexing.from_string comment) with
    | Ok _ -> ()
    | Error { Octavius.Errors. error ; location } ->
      let octavius_msg = Octavius.Errors.message error in
      let octavius_loc =
        let { Octavius.Errors. start ; finish } = location in
        let loc_start = loc.Location.loc_start in
        let open Lexing in
        let loc_start =
          let pos_bol = if start.line = 1 then loc_start.pos_bol else 0 in
          { loc_start with
            pos_bol;
            pos_lnum = loc_start.pos_lnum + start.line - 1;
            pos_cnum =
              if start.line = 1 then
                loc_start.pos_cnum + start.column
              else
                start.column
          }
        in
        let loc_end =
          let pos_bol = if finish.line = 1 then loc_start.pos_bol else 0 in
          { loc_start with
            pos_bol;
            pos_lnum = loc_start.pos_lnum + finish.line - 1;
            pos_cnum =
              if finish.line = 1 then
                loc_start.pos_cnum + finish.column
              else
                finish.column
          }
        in
        { loc with Location. loc_start; loc_end }
      in
      errorf ~loc:octavius_loc
        "%s\nYou can look at \
         http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec318\n\
         for a description of the recognized syntax."
        octavius_msg

  let is_intf_dot_ml fname =
    String.is_suffix (Caml.Filename.chop_extension fname) ~suffix:"_intf"

  let check_all ?(intf=false) () =
    List.iter ~f:(fun (comment, loc) ->
      let intf = intf || is_intf_dot_ml loc.Location.loc_start.Lexing.pos_fname in
      if (String.(<>) comment "") then (
        (* Ensures that all comments present in the file are either ocamldoc comments
           or (*_ *) comments. *)
        if intf && not (can_appear_in_mli comment) then begin
          errorf ~loc
            "That kind of comment shouldn't be present in interfaces.\n\
             Either turn it to a documentation comment or use the special (*_ *) form."
        end;
        if is_doc_comment comment then syntax_check_doc_comment ~loc comment
      )
    ) (Lexer.comments ())
end

let () =
  Driver.add_arg "-annotated-ignores"
    (Set annotated_ignores)
    ~doc:" If set, forces all ignored expressions (either under ignore or \
          inside a \"let _ = ...\") to have a type annotation."
;;

let () =
  Driver.add_arg "-compat-32"
    (Set compat_32)
    ~doc:" If set, checks that all constants are representable on 32bit architectures."
;;

(* Enable warning 50 by default, one can opt-out with [-dont-check-doc-comments-attachment] *)
let () =
  (* A bit hackish: as we're running ppx_driver with -pp the parsing is done
     by ppx_driver and not ocaml itself, so giving "-w @50" to ocaml (as we
     did up to now) had no incidence.
     We want to enable the warning here. For some reason one can't just enable
     a warning programatically, one has to call [parse_options]... *)
  Ocaml_common.Warnings.parse_options false "+50"

let () =
  let disable_w50 () = Ocaml_common.Warnings.parse_options false "-50" in
  Driver.add_arg "-dont-check-doc-comments-attachment" (Unit disable_w50)
    ~doc:" ignore warning 50 on the file."
;;

let () =
  let enable_checks () = check_comments := true in
  Driver.add_arg "-check-doc-comments" (Unit enable_checks)
    ~doc:" If set, ensures that all comments in .mli files are either \
          documentation or (*_ *) comments. Also check the syntax of doc comments."
;;

let () =
  let enable () = Dated_deprecation.enabled := true in
  let disable () = Dated_deprecation.enabled := false in
  Driver.add_arg "-dated-deprecation" (Unit enable)
    ~doc:{| If set, ensures that all `[@@deprecated]` attributes must contain \
            the date of deprecation, using the format `"[since MM-YYYY] ..."`.|};
  Driver.add_arg "-no-dated-deprecation" (Unit disable)
    ~doc:" inverse of -dated-deprecation."

let () =
  Driver.register_transformation "js_style"
    ~intf:(fun sg ->
      check#signature sg;
      if !check_comments then Comments_checking.check_all ~intf:true ();
      sg
    )
    ~impl:(fun st ->
      check#structure st;
      if !check_comments then Comments_checking.check_all ();
      st
    )
;;
