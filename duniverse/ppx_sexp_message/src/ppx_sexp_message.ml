open Base
open Ppxlib
open Ast_builder.Default

let omit_nil_attr =
  Attribute.declare "sexp_message.sexp.omit_nil"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let sexp_atom ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.List [%e x]]

let sexp_inline ~loc l =
  match l with
  | [x] -> x
  | _   -> sexp_list ~loc (elist ~loc l)
;;

(* Same as Ppx_sexp_value.omittable_sexp *)
type omittable_sexp =
  | Present of expression
  | Optional of Location.t * expression * (expression -> expression)
  | Omit_nil of Location.t * expression * (expression -> expression)
  | Absent

let present_or_omit_nil ~loc ~omit_nil expr =
  if omit_nil
  then Omit_nil (loc, expr, Fn.id)
  else Present expr

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Present e -> Present (f e)
  | Optional (loc, e, k) -> Optional (loc, e, (fun e -> f (k e)))
  | Omit_nil (loc, e, k) -> Omit_nil (loc, e, (fun e -> f (k e)))
  | Absent -> Absent


let sexp_of_constraint ~omit_nil ~loc expr ctyp =
  let optional ty =
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    Optional (loc, expr, fun expr -> eapply ~loc sexp_of [expr])
  in
  match ctyp with
  | [%type: [%t? ty] sexp_option] -> optional ty
  | [%type: [%t? ty] option] when omit_nil -> optional ty
  | _ ->
    let expr =
      let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
      eapply ~loc sexp_of [expr]
    in
    let omit_nil_attr = lazy (
      (* this is lazy so using [@omit_nil] inside [%message.omit_nil] is an error (unused
         attribute) *)
      match Attribute.get omit_nil_attr ctyp with
      | Some () -> true
      | None -> false
    ) in
    present_or_omit_nil ~loc expr ~omit_nil:(omit_nil || Lazy.force omit_nil_attr)
;;

let sexp_of_constant ~loc const =
  let f typ =
    eapply ~loc (evar ~loc ("Ppx_sexp_conv_lib.Conv.sexp_of_" ^ typ)) [pexp_constant ~loc const]
  in
  match const with
  | Pconst_integer   _ -> f "int"
  | Pconst_char      _ -> f "char"
  | Pconst_string    _ -> f "string"
  | Pconst_float     _ -> f "float"
;;

let rewrite_here e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
    Ppx_here_expander.lift_position_as_string ~loc:e.pexp_loc
  | _ -> e
;;

let sexp_of_expr ~omit_nil e =
  let e = rewrite_here e in
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant (Pconst_string ("", _)) ->
    Absent
  | Pexp_constant const ->
    present_or_omit_nil ~loc ~omit_nil:false (sexp_of_constant ~loc const)
  | Pexp_constraint (expr, ctyp) ->
    sexp_of_constraint ~omit_nil ~loc expr ctyp
  | _ -> present_or_omit_nil ~loc ~omit_nil:false [%expr Ppx_sexp_conv_lib.Conv.sexp_of_string [%e e]]
;;

let sexp_of_labelled_expr ~omit_nil (label, e) =
  let loc = e.pexp_loc in
  match label, e.pexp_desc with
  | Nolabel, Pexp_constraint (expr, _) ->
    let expr_str = Pprintast.string_of_expression expr in
    let k e = sexp_inline ~loc [sexp_atom ~loc (estring ~loc expr_str); e] in
    wrap_sexp_if_present (sexp_of_expr ~omit_nil e) ~f:k
  | Nolabel, _ ->
    sexp_of_expr ~omit_nil e
  | Labelled "_", _ ->
    sexp_of_expr ~omit_nil e
  | Labelled label, _ ->
    let k e =
      sexp_inline ~loc [sexp_atom ~loc (estring ~loc label); e]
    in
    wrap_sexp_if_present (sexp_of_expr ~omit_nil e) ~f:k
  | Optional _, _ ->
    (* Could be used to encode sexp_option if that's ever needed. *)
    Location.raise_errorf ~loc
      "ppx_sexp_value: optional argument not allowed here"
;;

let sexp_of_labelled_exprs ~omit_nil ~loc labels_and_exprs =
  let l = List.map labels_and_exprs ~f:(sexp_of_labelled_expr ~omit_nil) in
  let res =
    List.fold_left (List.rev l) ~init:(elist ~loc []) ~f:(fun acc e ->
      match e with
      | Absent -> acc
      | Present e -> [%expr [%e e] :: [%e acc] ]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl
        ]
      | Omit_nil (_, e, k) ->
        [%expr
          match [%e e], [%e acc] with
          | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
          | v, tl -> [%e k [%expr v]] :: tl
        ])
  in
  let has_optional_values =
    List.exists l ~f:(function ((Optional _ | Omit_nil _) : omittable_sexp) -> true
                             | Present _ | Absent -> false)
  in
  (* The two branches do the same thing, but when there are no optional values, we can do
     it at compile-time, which avoids making the generated code ugly. *)
  if has_optional_values
  then
    [%expr
      match [%e res] with
      | [h] -> h
      | [] | _ :: _ :: _ as res -> [%e sexp_list ~loc [%expr res]]
    ]
  else
    match res with
    | [%expr [ [%e? h] ] ] -> h
    | _ -> sexp_list ~loc res
;;

let expand ~omit_nil ~loc ~path:_ = function
  | None ->
    sexp_list ~loc (elist ~loc [])
  | Some e ->
    let loc = e.pexp_loc in
    let labelled_exprs =
      match e.pexp_desc with
      | Pexp_apply (f, args) ->
        (Nolabel, f) :: args
      | _ ->
        (Nolabel, e) :: []
    in
    sexp_of_labelled_exprs ~omit_nil ~loc labelled_exprs
;;

let message ~name ~omit_nil =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(map (single_expr_payload __) ~f:(fun f x -> f (Some x)) |||
                 map (pstr nil              ) ~f:(fun f   -> f None))
    (expand ~omit_nil)
;;

let () =
  Driver.register_transformation "sexp_message"
    ~extensions:[
      message ~name:"message" ~omit_nil:false;
      message ~name:"@message.omit_nil" ~omit_nil:true;
    ]
;;
