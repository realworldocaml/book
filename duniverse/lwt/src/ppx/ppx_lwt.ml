open! Ppxlib
open Ast_builder.Default

(** {2 Convenient stuff} *)

let with_loc f {txt ; loc } =
  f ~loc txt

(** Test if a case is a catchall. *)
let is_catchall case =
  let rec is_catchall_pat p = match p.ppat_desc with
    | Ppat_any | Ppat_var _ -> true
    | Ppat_alias (p, _) | Ppat_constraint (p,_) -> is_catchall_pat p
    | _ -> false
  in
  case.pc_guard = None && is_catchall_pat case.pc_lhs

(** Add a wildcard case in there is none. Useful for exception handlers. *)
let add_wildcard_case cases =
  let has_wildcard =
    List.exists is_catchall cases
  in
  if not has_wildcard
  then cases
       @ (let loc = Location.none in
          [case ~lhs:[%pat? exn] ~guard:None ~rhs:[%expr Lwt.fail exn]])
  else cases

(** {3 Internal names} *)

let lwt_prefix = "__ppx_lwt_"

(** {2 Here we go!} *)

let default_loc = ref Location.none

let sequence   = ref true
let strict_seq = ref true

let used_no_sequence_option = ref false
let used_no_strict_sequence_option = ref false

let no_sequence_option () =
  sequence := false;
  used_no_sequence_option := true

let no_strict_sequence_option () =
  strict_seq := false;
  used_no_strict_sequence_option := true

(** let%lwt related functions *)

let gen_name i = lwt_prefix ^ string_of_int i

(** [p = x] ≡ [__ppx_lwt_$i = x] *)
let gen_bindings l =
  let aux i binding =
    { binding with
      pvb_pat = pvar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
    }
  in
  List.mapi aux l

(** [p = x] and e ≡ [Lwt.bind __ppx_lwt_$i (fun p -> e)] *)
let gen_binds e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let name = (* __ppx_lwt_$i, at the position of $x$ *)
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr (fun [%p binding.pvb_pat] -> [%e aux (i+1) t])]
      in
      let new_exp =
          let loc = e_loc in
          [%expr
            let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
            Lwt.backtrace_bind
              (fun exn -> try Reraise.reraise exn with exn -> exn)
              [%e name]
              [%e fun_]
          ]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in aux 0 l

let lwt_sequence mapper ~exp ~lhs ~rhs ~ext_loc =
  let pat= let loc = ext_loc in [%pat? ()] in
  let lhs, rhs = mapper#expression lhs, mapper#expression rhs in
  let loc = exp.pexp_loc in
    [%expr
      let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
      Lwt.backtrace_bind
        (fun exn -> try Reraise.reraise exn with exn -> exn)
        [%e lhs]
        (fun [%p pat] -> [%e rhs])
    ]

(** For expressions only *)
(* We only expand the first level after a %lwt.
   After that, we call the mapper to expand sub-expressions. *)
let lwt_expression mapper exp attributes ext_loc =
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes in
  match exp.pexp_desc with

  (* $e$;%lwt $e'$ ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_sequence (lhs, rhs) ->
    Some (lwt_sequence mapper ~exp ~lhs ~rhs ~ext_loc)
  (* [let%lwt $p$ = $e$ in $e'$] ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl , e) ->
    let new_exp =
      pexp_let
        ~loc:!default_loc
        Nonrecursive
        (gen_bindings vbl)
        (gen_binds exp.pexp_loc vbl e)
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  (* [match%lwt $e$ with $c$] ≡ [Lwt.bind $e$ (function $c$)]
     [match%lwt $e$ with exception $x$ | $c$] ≡
     [Lwt.try_bind (fun () -> $e$) (function $c$) (function $x$)] *)
  | Pexp_match (e, cases) ->
    let exns, cases =
      cases |> List.partition (
        function
        | {pc_lhs = [%pat? exception [%p? _]]; _} -> true
        | _ -> false)
    in
    if cases = [] then
      Location.raise_errorf ~loc:exp.pexp_loc
        "match%%lwt must contain at least one non-exception pattern." ;
    let exns =
      exns |> List.map (
        function
        | {pc_lhs = [%pat? exception [%p? pat]]; _} as case ->
          { case with pc_lhs = pat }
        | _ -> assert false)
    in
    let exns = add_wildcard_case exns in
    let new_exp =
      match exns with
      | [] ->
        let loc = !default_loc in
        [%expr Lwt.bind [%e e] [%e pexp_function ~loc cases]]
      | _  ->
        let loc = !default_loc in
        [%expr Lwt.try_bind (fun () -> [%e e])
                                   [%e pexp_function ~loc cases]
                                   [%e pexp_function ~loc exns]]
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  (* [assert%lwt $e$] ≡
     [try Lwt.return (assert $e$) with exn -> Lwt.fail exn] *)
  | Pexp_assert e ->
    let new_exp =
      let loc = !default_loc in
      [%expr try Lwt.return (assert [%e e]) with exn -> Lwt.fail exn]
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  (* [while%lwt $cond$ do $body$ done] ≡
     [let rec __ppx_lwt_loop () =
        if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
        else Lwt.return_unit
      in __ppx_lwt_loop]
  *)
  | Pexp_while (cond, body) ->
    let new_exp =
      let loc = !default_loc in
      [%expr
        let rec __ppx_lwt_loop () =
          if [%e cond] then Lwt.bind [%e body] __ppx_lwt_loop
          else Lwt.return_unit
        in __ppx_lwt_loop ()
      ]
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  (* [for%lwt $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
     [let __ppx_lwt_bound = $end$ in
     let rec __ppx_lwt_loop $p$ =
       if $p$ COMP __ppx_lwt_bound then Lwt.return_unit
       else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
     in __ppx_lwt_loop $start$]
  *)
  | Pexp_for ({ppat_desc = Ppat_var p_var; _} as p, start, bound, dir, body) ->
    let comp, op =
      let loc = !default_loc in
      match dir with
      | Upto ->   evar ~loc ">", evar ~loc "+"
      | Downto -> evar ~loc "<", evar ~loc "-"
    in
    let p' = with_loc evar p_var in

    let exp_bound = let loc = bound.pexp_loc in [%expr __ppx_lwt_bound] in
    let pat_bound = let loc = bound.pexp_loc in [%pat? __ppx_lwt_bound] in

    let new_exp =
      let loc = !default_loc in
      [%expr
        let [%p pat_bound] : int = [%e bound] in
        let rec __ppx_lwt_loop [%p p] =
          if [%e comp] [%e p'] [%e exp_bound] then Lwt.return_unit
          else Lwt.bind [%e body] (fun () -> __ppx_lwt_loop ([%e op] [%e p'] 1))
        in __ppx_lwt_loop [%e start]
      ]
    in
    Some (mapper#expression { new_exp with pexp_attributes })


  (* [try%lwt $e$ with $c$] ≡
     [Lwt.catch (fun () -> $e$) (function $c$)]
  *)
  | Pexp_try (expr, cases) ->
    let cases = add_wildcard_case cases in
    let new_exp =
      let loc = !default_loc in
        [%expr
          let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
          Lwt.backtrace_catch
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            (fun () -> [%e expr])
            [%e pexp_function ~loc cases]
        ]
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  (* [if%lwt $c$ then $e1$ else $e2$] ≡
     [match%lwt $c$ with true -> $e1$ | false -> $e2$]
     [if%lwt $c$ then $e1$] ≡
     [match%lwt $c$ with true -> $e1$ | false -> Lwt.return_unit]
  *)
  | Pexp_ifthenelse (cond, e1, e2) ->
    let e2 =
      match e2 with
      | None -> let loc = !default_loc in [%expr Lwt.return_unit]
      | Some e -> e
    in
    let cases =
      let loc = !default_loc in
      [
        case ~lhs:[%pat? true] ~guard:None ~rhs:e1 ;
        case ~lhs:[%pat? false] ~guard:None ~rhs:e2 ;
      ]
    in
    let new_exp =
      let loc = !default_loc in
      [%expr Lwt.bind [%e cond] [%e pexp_function ~loc cases]]
    in
    Some (mapper#expression { new_exp with pexp_attributes })

  | _ ->
    None

let warned = ref false

class mapper = object (self)
  inherit Ast_traverse.map as super

  method! structure = begin fun structure ->
      if !warned then
        super#structure structure

      else begin
        warned := true;
        let structure = super#structure structure in
        let loc = Location.in_file !Ocaml_common.Location.input_name in

        let warn_if condition message structure =
          if condition then
            (pstr_attribute ~loc (attribute_of_warning loc message))::structure
          else
            structure
        in

        structure
        |> warn_if (!used_no_strict_sequence_option)
          ("-no-strict-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
        |> warn_if (!used_no_sequence_option)
          ("-no-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
      end
    end

  method! expression = (fun expr ->
      match expr with
      | { pexp_desc=
            Pexp_extension (
              {txt="lwt"; loc= ext_loc},
              PStr[{pstr_desc= Pstr_eval (exp, _);_}]);
          _
        }->
        begin match lwt_expression self exp expr.pexp_attributes ext_loc with
        | Some expr' -> expr'
        | None -> expr
        end
      (* [($e$)[%finally $f$]] ≡
         [Lwt.finalize (fun () -> $e$) (fun () -> $f$)] *)
      | [%expr [%e? exp ] [%finally     [%e? finally]] ]
      | [%expr [%e? exp ] [%lwt.finally [%e? finally]] ] ->
        let new_exp =
          let loc = !default_loc in
            [%expr
              let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
              Lwt.backtrace_finalize
                (fun exn -> try Reraise.reraise exn with exn -> exn)
                (fun () -> [%e exp])
                (fun () -> [%e finally])
            ]
        in
        super#expression
          { new_exp with
            pexp_attributes = expr.pexp_attributes @ exp.pexp_attributes
          }

      | [%expr [%finally     [%e? _ ]]]
      | [%expr [%lwt.finally [%e? _ ]]] ->
        Location.raise_errorf ~loc:expr.pexp_loc
          "Lwt's finally should be used only with the syntax: \"(<expr>)[%%finally ...]\"."

      | _ ->
        super#expression expr)

  method! structure_item = (fun stri ->
      default_loc := stri.pstr_loc;
      match stri with
      | [%stri let%lwt [%p? var] = [%e? exp]] ->
        let warning =
          estring ~loc:!default_loc
            ("let%lwt should not be used at the module item level.\n" ^
             "Replace let%lwt x = e by let x = Lwt_main.run (e)")
        in
        let loc = !default_loc in
        [%stri
          let [%p var] =
            (Lwt_main.run [@ocaml.ppwarning [%e warning]])
              [%e super#expression exp]]

      | x -> super#structure_item x);
end


let args =
  [
    "-no-sequence",
      Arg.Unit no_sequence_option,
      " has no effect (deprecated)";

    "-no-strict-sequence",
      Arg.Unit no_strict_sequence_option,
      " has no effect (deprecated)";
  ]

let () =
  let mapper = new mapper in
  Driver.register_transformation "ppx_lwt"
    ~impl:mapper#structure
    ~intf:mapper#signature ;
  List.iter (fun (key, spec, doc) -> Driver.add_arg key spec ~doc) args
