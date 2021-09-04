open Base
open Ppxlib

let annotated_ignores = ref true
let check_comments = ref false
let compat_32 = ref false
let allow_toplevel_expression = ref false
let check_underscored_literal = ref true
let cold_instead_of_inline_never = ref true
let require_dated_deprecation = ref In_janestreet.in_janestreet
let allow_letop_uses = ref (not In_janestreet.in_janestreet)

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

module Suspicious_literal = struct
  type t = string * string
  let fail ~loc ((s, typ) : t) =
    Location.raise_errorf ~loc
      "The %s literal %s contains underscores at suspicious positions" typ s
end

module Invalid_ocamlformat_attribute = struct
  type t = string

  let fail ~loc (reason : t) =
    Location.raise_errorf ~loc
      "Invalid ocamlformat attribute. %s" reason

  let kind { attr_name = name; attr_payload = payload; attr_loc = _; } =
    match name.txt, payload with
    | "ocamlformat", PStr ([%str "disable"] | [%str "enable"]) -> `Enable_disable
    | "ocamlformat", _ -> `Other
    | _ -> `Not_ocamlformat
end

type error =
  | Invalid_deprecated of Invalid_deprecated.t
  | Missing_type_annotation of Ignored_reason.t
  | Invalid_constant of Invalid_constant.t
  | Suspicious_literal of Suspicious_literal.t
  | Invalid_ocamlformat_attribute of Invalid_ocamlformat_attribute.t
  | Docstring_on_open
  | Use_of_letop of { op_name : string }

let fail ~loc = function
  | Invalid_deprecated e -> Invalid_deprecated.fail e ~loc
  | Missing_type_annotation e -> Ignored_reason.fail e ~loc
  | Invalid_constant e -> Invalid_constant.fail e ~loc
  | Suspicious_literal e -> Suspicious_literal.fail e ~loc
  | Invalid_ocamlformat_attribute e -> Invalid_ocamlformat_attribute.fail e ~loc
  | Docstring_on_open ->
    errorf ~loc
      "A documentation comment is attached to this [open] which will be dropped by odoc."
  | Use_of_letop { op_name } ->
    errorf ~loc
      "This use of ( %s ) is forbidden.@.\
       ppx_let is currently more featureful, please use that instead to keep a consistent \
       style"
      op_name
;;

let local_ocamlformat_config_disallowed =
  Invalid_ocamlformat_attribute "Ocamlformat cannot be configured locally"

let check_deprecated_string ~f ~loc s =
  match Caml.Scanf.sscanf s "[since %u-%u]" (fun y m -> (y, m)) with
  | exception _ -> f ~loc (Invalid_deprecated Missing_date)
  | (_year, month) ->
    if month = 0 || month > 12 then f ~loc (Invalid_deprecated Invalid_month)
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

module Constant = struct
  let max_int_31 = Int64.(-) (Int64.shift_left 1L 30) 1L
  let min_int_31 = Int64.neg (Int64.shift_left 1L 30)
  let check_compat_32 ~loc c =
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
            let i = Int64.of_string s in
            if Int64.(i < min_int_31 || i > max_int_31) then failwith "out of bound"
          with _ ->
            fail ~loc (Invalid_constant (s, "int"))
        end
      | _ -> ()

  let check_underscored ~loc c =
    if !check_underscored_literal
    then (
      let check_segment ~name ~start ~stop ~kind s = (* start and stop are inclusive *)
        let incr = if start < stop then 1 else -1 in
        let modulo =
          match kind with
          | `Decimal -> 3
          | `Hexadecimal
          | `Binary
          | `Octal -> 2
        in
        let rec loop string_pos ~number_offset =
          let number_offset =
            match s.[string_pos] with
            | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
              number_offset + 1
            | '_' ->
              if number_offset % modulo <> 0
              then fail ~loc (Suspicious_literal (s, name))
              else number_offset
            | _ -> assert false
          in
          if stop <> string_pos
          then loop (string_pos + incr) ~number_offset
        in
        loop start ~number_offset:0
      in
      let parse_prefix s =
        let i =
          match s.[0] with
          | '-' | '+' -> 1
          | _ -> 0
        in
        if String.length s >= i + 2
        then begin
          match s.[i], s.[i + 1] with
          | '0', ('x' | 'X') -> `Hexadecimal, i + 2
          | '0', ('b' | 'B') -> `Binary, i + 2
          | '0', ('o' | 'O') -> `Octal, i + 2
          | _ -> `Decimal, i
        end else `Decimal, i
      in
      let should_check =
        let has_double_underscores s = String.is_substring ~substring:"__" s in
        let has_underscore s = String.exists ~f:(fun c -> Char.(=) c '_') s in
        fun s -> has_underscore s && not (has_double_underscores s)
      in
      match c with
      | Pconst_integer (s, _) ->
        if should_check s
        then
          let kind, lower = parse_prefix s in
          check_segment ~name:"integer" ~start:(String.length s - 1) ~stop:lower ~kind s
      | Pconst_float (s, _) ->
        if should_check s
        then
          let kind, lower = parse_prefix s in
          let upper = (* only validate the mantissa *)
            let power_split =
              match kind with
              | `Decimal ->
                String.lfindi s ~f:(fun _ c ->
                  match c with 'e' | 'E' -> true  | _ -> false)
              | `Hexadecimal ->
                String.lfindi s ~f:(fun _ c ->
                  match c with 'p' | 'P' -> true | _ -> false)
              | `Binary | `Octal ->
                assert false
            in
            match power_split with
            | None -> String.length s - 1
            | Some i -> i - 1
          in
          let name = "float" in
          begin match String.index_from s lower '.' with
          | None -> check_segment ~name ~start:upper ~stop:lower ~kind s
          | Some i ->
            if lower <> i then check_segment ~name ~start:(i-1) ~stop:lower ~kind s;
            if upper <> i then check_segment ~name ~start:(i+1) ~stop:upper ~kind s
          end
      | Pconst_char _ | Pconst_string _ -> ())


  let check ~loc c =
    check_compat_32 ~loc c;
    check_underscored ~loc c
end
let is_deprecated = function
  | "ocaml.deprecated" | "deprecated" -> true
  | _ -> false

let is_inline = function
  | "ocaml.inline" | "inline" -> true
  | _ -> false

let check_deprecated attr =
  if is_deprecated attr.attr_name.txt then
    errorf ~loc:(loc_of_attribute attr)
      "Invalid deprecated attribute, it will be ignored by the compiler"

let is_mlt_or_mdx fname =
  String.is_suffix fname ~suffix:".mlt"
  || String.is_suffix fname ~suffix:".mdx"
  || String.equal "//toplevel//" fname

let iter_style_errors ~f = object (self)
  inherit Ast_traverse.iter as super

  method! attribute ({ attr_name = name; attr_payload = payload; attr_loc = _ } as attr) =
    let loc = loc_of_attribute attr in
    (if !require_dated_deprecation && is_deprecated name.txt then
       match
         Ast_pattern.(parse (single_expr_payload (estring __'))) loc payload (fun s -> s)
       with
       | exception _ -> f ~loc (Invalid_deprecated Not_a_string)
       | { Location. loc; txt = s } -> check_deprecated_string ~f ~loc s
    );
    (match Invalid_ocamlformat_attribute.kind attr with
     | `Enable_disable ->
       f ~loc
         (Invalid_ocamlformat_attribute
            "Ocamlformat can only be disabled at toplevel\n\
             (e.g [@@@ocamlformat \"disable\"])")
     | `Other ->
       f ~loc local_ocamlformat_config_disallowed
     | `Not_ocamlformat -> ()
    )

  method! payload p =
    match p with
    | PStr l ->
      (* toplevel expressions in payload are fine. *)
      List.iter l ~f:(fun item ->
        self#check_structure_item item ~allow_toplevel_expression:true)
    | _ -> super#payload p

  method! open_description od =
    if !check_comments then (
      let has_doc_comments =
        List.exists od.popen_attributes ~f:(fun { attr_name; _ } ->
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

  method! expression e =
    begin match e with
    | {pexp_desc = Pexp_constant c; pexp_loc; _ } ->
      Constant.check ~loc:pexp_loc c
    | [%expr ignore [%e? ignored]] when !annotated_ignores ->
      ignored_expr_must_be_annotated Argument_to_ignore ~f ignored
    | {pexp_desc = Pexp_letop { let_ ; _ }; _ } when not !allow_letop_uses ->
      fail ~loc:let_.pbop_op.loc (Use_of_letop { op_name = let_.pbop_op.txt })
    | _ -> ()
    end;
    super#expression e

  method! pattern e =
    begin match e with
    | {ppat_desc = Ppat_constant c; ppat_loc; _ } ->
      Constant.check ~loc:ppat_loc c
    | _ -> ()
    end;
    super#pattern e

  method! core_type t =
    List.iter t.ptyp_attributes ~f:check_deprecated;
    super#core_type t

  method private check_structure_item t ~allow_toplevel_expression =
    (match t.pstr_desc with
     | Pstr_eval (_,_) when
         not allow_toplevel_expression
         && not (is_mlt_or_mdx t.pstr_loc.Location.loc_start.Lexing.pos_fname) ->
       errorf ~loc:t.pstr_loc
         "Toplevel expression are not allowed here."
     | Pstr_attribute a ->
       (match Invalid_ocamlformat_attribute.kind a with
        | `Enable_disable -> ()
        | `Other ->
          f ~loc:t.pstr_loc local_ocamlformat_config_disallowed
        | `Not_ocamlformat -> super#structure_item t)
     | _ -> super#structure_item t)

  method! structure_item t =
    self#check_structure_item t ~allow_toplevel_expression:!allow_toplevel_expression

  method! signature_item t =
    (match t.psig_desc with
     | Psig_attribute a ->
       (match Invalid_ocamlformat_attribute.kind a with
        | `Enable_disable -> ()
        | `Other ->
          f ~loc:t.psig_loc local_ocamlformat_config_disallowed
        | `Not_ocamlformat -> super#signature_item t)
     | _ -> super#signature_item t)
end

let check = iter_style_errors ~f:fail

let enforce_cold = object
  inherit [Driver.Lint_error.t list] Ast_traverse.fold
  method! attribute attr acc =
    let loc = loc_of_attribute attr in
    if !cold_instead_of_inline_never && is_inline attr.attr_name.txt then
      match
        Ast_pattern.(parse (single_expr_payload (pexp_ident __'))) loc attr.attr_payload Fn.id
      with
      | exception _ -> acc
      | { Location. loc; txt = Lident "never" } ->
        (Driver.Lint_error.of_string
           loc
           "Attribute error: please use [@cold] instead of [@inline never]") :: acc
      | _ -> acc
    else
      acc
end

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
  (* We rely on the fact that let%test and similar things are preprocessed before we run,
     because ppx_driver applies the [~extension] arguments of
     [Driver.register_transformation] before applying the [~impl] argument that
     ppx_js_style uses.
     It means that [let%test _ = ..] doesn't count as unannotated ignore, although
     [let%bind _ = ..] also doesn't count as unannotated ignore for the same reason. *)
  Driver.add_arg "-annotated-ignores"
    (Set annotated_ignores)
    ~doc:" If set, forces all ignored expressions (either under ignore or \
          inside a \"let _ = ...\") to have a type annotation. (This is the default.)"
;;

let () =
  let disable_annotated_ignores () = annotated_ignores := false in
  Driver.add_arg "-allow-unannotated-ignores"
    (Unit disable_annotated_ignores)
    ~doc:" If set, allows ignored expressions (either under ignore or inside a \"let _ = \
          ...\") not to have a type annotation."
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
  ignore (Ocaml_common.Warnings.parse_options false "+50")

let () =
  let disable_w50 () = ignore (Ocaml_common.Warnings.parse_options false "-50") in
  Driver.add_arg "-dont-check-doc-comments-attachment" (Unit disable_w50)
    ~doc:" ignore warning 50 on the file."
;;

let () =
  let disable_check_underscored_literal () = check_underscored_literal := false in
  Driver.add_arg "-dont-check-underscored-literal" (Unit disable_check_underscored_literal)
    ~doc:" do not check position of underscores in numbers."
;;

let () =
  let enable_checks () = check_comments := true in
  Driver.add_arg "-check-doc-comments" (Unit enable_checks)
    ~doc:" If set, ensures that all comments in .mli files are either \
          documentation or (*_ *) comments. Also check the syntax of doc comments."
;;

let () =
  let allow_top_expr () = allow_toplevel_expression := true in
  Driver.add_arg "-allow-toplevel-expression" (Unit allow_top_expr)
    ~doc:" If set, allow toplevel expression."
;;

let () =
  let enable () = require_dated_deprecation := true in
  let disable () = require_dated_deprecation := false in
  Driver.add_arg "-dated-deprecation" (Unit enable)
    ~doc:{| If set, ensures that all `[@@deprecated]` attributes must contain \
            the date of deprecation, using the format `"[since MM-YYYY] ..."`.|};
  Driver.add_arg "-no-dated-deprecation" (Unit disable)
    ~doc:" inverse of -dated-deprecation."

let () =
  let allow () = allow_letop_uses := true in
  let forbid () = allow_letop_uses := false in
  Driver.add_arg "-allow-let-operators" (Unit allow)
    ~doc:{| allow uses of let-operators|};
  Driver.add_arg "-forbid-let-operators" (Unit forbid)
    ~doc:{| forbid uses of let-operators|}

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
    (* note: we do not use ~impl because we want the check to run before ppx
       processing (ppx_cold will replace `[@cold]` with `[@inline never] ...`)*)
    ~lint_impl:(fun st -> enforce_cold#structure st [])
;;
