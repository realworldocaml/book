open Base
open Ppxlib
open Ast_builder.Default

(* returns the index of the conversion spec (unless the end of string is reached) *)
let rec skip_over_format_flags fmt i =
  if i >= String.length fmt
  then `Eoi
  else match fmt.[i] with
  | '*' | '#' | '-' | ' ' | '+' | '_' | '0'..'9' | '.' ->
    skip_over_format_flags fmt (i + 1)
  | _ -> `Ok i

(* doesn't check to make sure the format string is well-formed *)
(* Formats with subformats are skipped for the following reasons:

   One is that they are hard to understand and not often used.

   Another is that subformats like "%(%{Module}%)" won't work, since it
   is impossible to produce a format of type [(Module.t -> 'a,...) format].
*)
let has_subformats (fmt:string) =
  let lim = String.length fmt - 1 in
  let rec loop i =
    if i > lim
    then false
    else
      if Char.equal fmt.[i] '%' then
        match skip_over_format_flags fmt (i + 1) with
        | `Eoi -> false
        | `Ok i ->
          match fmt.[i] with
          | '(' | ')' | '}' -> true
          | _ -> loop (i + 1)
      else loop (i + 1)
  in
  loop 0

(* returns a list of strings where even indexed elements are parts of the format string
   that the preprocessor won't touch and odd indexed elements are the contents of %{...}
   specifications. *)
let explode ~loc (s:string) =
  let len = String.length s in
  (* for cases where we can't parse the string with custom format specifiers, consider
     the string as a regular format string *)
  let as_normal_format_string = [s] in
  if has_subformats s
  then as_normal_format_string
  else
    let sub from to_ = String.sub s ~pos:from ~len:(to_ - from) in
    let rec loop acc from to_ =
      assert (List.length acc % 2 = 0);
      if to_ >= len
      then List.rev (
        if from >= len
        then acc
        else sub from len :: acc
      )
      else
      if Char.(<>) s.[to_] '%'
      then loop acc from (to_ + 1)
      else
        match skip_over_format_flags s (to_ + 1) with
        | `Eoi -> as_normal_format_string
        | `Ok i ->
          match s.[i] with
          | '[' ->
            (* Scan char sets are not allowed by printf-like functions. So we might as
               well disallow them at compile-time so that we can reuse them as magic
               format strings in this implementation. *)
            Location.raise_errorf ~loc
              "ppx_custom_printf: scan char sets are not allowed in \
               custom format strings"
          | '{' ->
            if to_ + 1 <> i then
              Location.raise_errorf ~loc
                "ppx_custom_printf: unexpected format flags before \
                 %%{} specification in %S" s;
            begin match String.index_from s (to_ + 2) '}' with
            | None -> as_normal_format_string
            | Some i ->
              let l =
                sub (to_ + 2) i
                :: sub from to_
                :: acc
              in
              loop l (i + 1) (i + 1)
            end
          | _ ->
            loop acc from (i + 1) (* skip the conversion spec *)
    in
    loop [] 0 0

let processed_format_string ~exploded_format_string =
  let l =
    let rec loop i l =
      match l with
      | s1 :: _s2 :: l -> s1 :: Printf.sprintf "%%%d[.]" i :: loop (i + 1) l
      | [s1] -> [s1]
      | [] -> []
    in
    loop 0 exploded_format_string
  in
  String.concat l ~sep:""

let rec evens = function
  | [] | [_] as l -> l
  | x :: _ :: l -> x :: evens l

let odds = function
  | [] -> []
  | _ :: l -> evens l

(* Returns a pair of:

   - a format string, which is [s] where all custom format specifications have been
     replaced by ["%" ^ string_of_int index ^ "[.]"] where [index] is the number of
     the custom format specification, starting from 0. This string can be passed directly
     to [CamlinternalFormat.fmt_ebb_of_string]
   - an array of custom format specifications, in the order they appear in the original
     string
*)
let extract_custom_format_specifications ~loc s =
  let exploded_format_string = explode ~loc s in
  let processed = processed_format_string ~exploded_format_string in
  let custom_specs = Array.of_list (odds exploded_format_string) in
  (processed, custom_specs)
;;

let gen_symbol = gen_symbol ~prefix:"_custom_printf"

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let strip s =
  let a = ref 0 in
  let b = ref (String.length s - 1) in
  while !a <= !b && is_space s.[!a] do Int.incr a done;
  while !a <= !b && is_space s.[!b] do Int.decr b done;
  if !a > !b then "" else String.sub s ~pos:!a ~len:(!b - !a + 1)
;;

let string_to_expr ~loc s =
  let sexp_converter_opt =
    match String.lsplit2 s ~on:':' with
    | None -> None
    | Some ("sexp", colon_suffix) ->
      Some ([%expr  Ppx_sexp_conv_lib.Sexp.to_string_hum ], colon_suffix)
    | Some (colon_prefix, colon_suffix) ->
      match String.chop_prefix colon_prefix ~prefix:"sexp#" with
      | None -> None
      | Some hash_suffix ->
        Some (pexp_ident ~loc
                (Located.mk ~loc
                   (Longident.parse ("Ppx_sexp_conv_lib.Sexp.to_string_" ^ hash_suffix))),
              colon_suffix)
  in
  match sexp_converter_opt with
  | Some (sexp_converter, unparsed_type) ->
    let lexbuf = Lexing.from_string unparsed_type in
    lexbuf.lex_curr_p <- loc.loc_start;
    let ty = Parse.core_type lexbuf in
    let e = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    let arg = gen_symbol () in
    pexp_fun ~loc Nolabel None (pvar ~loc arg)
      (eapply ~loc sexp_converter [eapply ~loc e [evar ~loc arg]])
  | None ->
    let fail loc =
      Location.raise_errorf ~loc
        "ppx_custom_printf: string %S should be of the form <Module>, \
         <Module>.<identifier>, <Module>#identifier, sexp:<type>, or sexp#mach:<type>"
        s
    in
    let s, has_hash_suffix, to_string =
      match String.lsplit2 s ~on:'#' with
      | None -> s, false, "to_string"
      | Some (s, hash_suffix) -> s, true, "to_string_" ^ hash_suffix
    in
    let to_string_id : Longident.t =
      let s = strip s in
      match s with
      | "" -> Lident to_string
      | _ ->
        match Longident.parse s with
        | Lident n | Ldot (_, n) as id ->
          if String.(<>) n "" && Char.equal (Char.uppercase n.[0]) n.[0] then
            Longident.Ldot (id, to_string)
          else if not has_hash_suffix then
            id
          else
            fail loc
        | _ -> fail loc
    in
    let func = pexp_ident ~loc (Located.mk ~loc to_string_id) in
    (* Eta-expand as the to_string function might take optional arguments *)
    let arg = gen_symbol () in
    pexp_fun ~loc Nolabel None (pvar ~loc arg) (eapply ~loc func [evar ~loc arg])

class lifter ~loc ~custom_specs = object(self)
  inherit [expression] Format_lifter.lift as super
  inherit Ppxlib_metaquot_lifters.expression_lifters loc

  method! fmt
    : type f0 f1 f2 f3 f4 f5. (f0 -> expression)
      -> (f1 -> expression)
      -> (f2 -> expression)
      -> (f3 -> expression)
      -> (f4 -> expression)
      -> (f5 -> expression)
      -> (f0, f1, f2, f3, f4, f5) CamlinternalFormatBasics.fmt
      -> expression
    = fun f0 f1 f2 f3 f4 f5 fmt ->
      let open CamlinternalFormatBasics in
      match fmt with
      (* Recognize the special form "%index[...whatever...]" *)
      | Scan_char_set (Some idx, _, fmt)
        (* [custom_specs] is empty if [explode] couldn't parse the string. In this case we
           can have some scar char sets left. *)
        when idx >= 0 && idx < Array.length custom_specs ->
        let rest =
          self#fmt (fun _ -> assert false) f1 f2 f3 f4 f5
            fmt
        in
        let func = string_to_expr ~loc custom_specs.(idx) in
        [%expr
          Custom(Custom_succ
                   Custom_zero,
                 (fun () -> [%e func]),
                 [%e rest])
        ]
      | _ ->
        super#fmt f0 f1 f2 f3 f4 f5 fmt
end

let expand_format_string ~loc fmt_string =
  let processed_fmt_string, custom_specs =
    extract_custom_format_specifications ~loc fmt_string
  in
  let (CamlinternalFormat.Fmt_EBB fmt) =
    try
      CamlinternalFormat.fmt_ebb_of_string processed_fmt_string
    with e ->
      Location.raise_errorf ~loc "%s"
        (match e with
         (* [fmt_ebb_of_string] normally raises [Failure] on invalid input *)
         | Failure msg -> msg
         | e -> Exn.to_string e)
  in
  let lifter = new lifter ~loc ~custom_specs in
  let format6 = CamlinternalFormatBasics.Format (fmt, fmt_string) in
  let phantom _ = assert false in
  let e =
    lifter#format6
      phantom phantom phantom phantom phantom phantom format6
  in
  [%expr ([%e e] : (_, _, _, _, _, _) CamlinternalFormatBasics.format6)]

let expand e =
  match e.pexp_desc with
  | Pexp_apply ({ pexp_attributes = ident_attrs; _ },
                [ (Nolabel, { pexp_desc = Pexp_constant (Pconst_string (str, _))
                            ; pexp_loc = loc
                            ; pexp_attributes = str_attrs }) ]) ->
    assert_no_attributes ident_attrs;
    assert_no_attributes str_attrs;
    let e' = expand_format_string ~loc str in
    Some { e' with pexp_attributes = e.pexp_attributes }
  | _ -> None
;;

let () =
  Driver.register_transformation "custom_printf"
    ~rules:[ Context_free.Rule.special_function "!" expand ]
;;
