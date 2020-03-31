(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* ML stub generation *)

[@@@warning "-9-27"]

open Ctypes_static
open Ctypes_path
open Cstubs_errors

type non_lwt = [ `Sequential | `Unlocked ]
type lwt = [ `Lwt_jobs | `Lwt_preemptive ]
type concurrency_policy = [ non_lwt | lwt ]
type errno_policy = [ `Ignore_errno | `Return_errno ]

type lident = string
type ml_type = [ `Ident of path
               | `Appl of path * ml_type list
               | `Pair of ml_type * ml_type
               | `Fn of ml_type * ml_type ]

type ml_external_type = [ `Prim of ml_type list * ml_type ]

type ml_pat = [ `Var of string
              | `Record of (path * ml_pat) list * [`Etc | `Complete]
              | `As of ml_pat * string
              | `Underscore
              | `Con of path * ml_pat list ]

type ml_exp = [ `Ident of path 
              | `Project of ml_exp * path
              | `MakePtr of ml_exp * ml_exp
              | `MakeFunPtr of ml_exp * ml_exp
              | `MakeStructured of ml_exp * ml_exp
              | `Appl of ml_exp * ml_exp
              | `Tuple of ml_exp list
              | `Seq of ml_exp * ml_exp
              | `Let of ml_pat * ml_exp * ml_exp
              | `Unit
              | `Fun of lident list * ml_exp ]

type attributes = { float: bool; noalloc: bool }

type extern = {
  ident : string;
  typ: ml_external_type;
  primname: string;
  primname_byte: string option;
  attributes: attributes;
}

module Emit_ML : sig 
  type appl_parens = ApplParens | NoApplParens
  val ml_exp : appl_parens -> Format.formatter -> ml_exp -> unit
  val ml_pat : appl_parens -> Format.formatter -> ml_pat -> unit
  val ml_external_type : Format.formatter -> ml_external_type -> unit
  val extern : Format.formatter -> extern -> unit
end =
struct
  let fprintf = Format.fprintf

  (* We (only) need to parenthesize function types in certain contexts 
        * on the lhs of a function type: - -> t
        * as the argument to a single-argument type constructor: - t
  *)
  type arrow_parens = ArrowParens | NoArrowParens

  (* We (only) need to parenthesize application expressions in certain contexts 
        * in a projection expression: -.l
        * in a dereference expression: !@ -
        * as an argument in an application: e -
  *)
  type appl_parens = ApplParens | NoApplParens

  let ident = format_path

  let rec ml_type arrow_parens fmt t =
    match arrow_parens, t with
    | _, `Ident i -> ident fmt i
    | _, `Appl (t, []) -> ident fmt t
    | _, `Appl (t, [t']) ->
      fprintf fmt "@[%a@ %a@]" (ml_type ArrowParens) t' ident t
    | _, `Appl (t, ts) ->
      let nargs = List.length ts in
      fprintf fmt "(";
      List.iteri
        (fun i arg ->
          if i = nargs - 1 then (ml_type NoArrowParens) fmt arg
          else fprintf fmt "%a,@ " (ml_type NoArrowParens) arg
        ) ts;
      fprintf fmt ")@ %a" ident t;
    | ArrowParens, `Fn (t, t') ->
      fprintf fmt "@[(%a@ ->@ %a)@]" (ml_type ArrowParens) t (ml_type NoArrowParens) t'
    | NoArrowParens, `Fn (t, t') ->
      fprintf fmt "@[%a@ ->@]@ %a" (ml_type ArrowParens) t (ml_type NoArrowParens) t'
    | _, `Pair (t, t') ->
      fprintf fmt "@[(%a@ *@ %a)@]" (ml_type NoArrowParens) t (ml_type NoArrowParens) t'

  let ml_external_type fmt (`Prim (args, ret) : ml_external_type) =
    List.iter (fprintf fmt "@[%a@ ->@]@ " (ml_type ArrowParens)) args;
    ml_type ArrowParens fmt ret

  let primname_opt fmt = function
    | None -> ()
    | Some primname -> fprintf fmt "%S@ " primname

  let attrs fmt { float; noalloc } =
    begin 
    (* TODO: float support not yet implemented *)
    (* if float then pp_print_string fmt "\"float\""; *)

    (* TODO: fix this.  The may_allocate function determines whether any of
       the functions in the generated C cause OCaml heap allocations.
       However, it doesn't currently account for callbacks: if we pass a
       handle to an OCaml function into C, calling the function can trigger an
       allocation.  We need some way in the interface of the library for the
       client to indicate whether it is safe to assume that a C function
       cannot call back into OCaml. *)
    (* if noalloc then pp_print_string fmt "\"noalloc\"" *)
    end

  let args fmt xs =
    List.iter (fprintf fmt "%s@ ") xs

  let rec ml_exp appl_parens fmt (e : ml_exp) =
    match appl_parens, e with
    | _, `Unit -> fprintf fmt "()"
    | _, `Ident x -> ident fmt x
    | _, `Project (e, l) -> fprintf fmt "%a.%a" (ml_exp ApplParens) e ident l
    | ApplParens, `Appl (f, p) -> fprintf fmt "@[(%a@;<1 2>%a)@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | NoApplParens, `Appl (f, p) -> fprintf fmt "@[%a@ %a@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | ApplParens, `MakePtr (t, e) ->
      fprintf fmt
        "(@[<hov 2>CI.make_ptr@ %a@ %a)@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakePtr (t, e) ->
      fprintf fmt
        "@[<hov 2>CI.make_ptr@ %a@ %a@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | ApplParens, `MakeFunPtr (t, e) ->
      fprintf fmt
        "(@[<hov 2>CI.make_fun_ptr@ %a@ %a)@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakeFunPtr (t, e) ->
      fprintf fmt
        "@[<hov 2>CI.make_fun_ptr@ %a@ %a@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | ApplParens, `MakeStructured (t, e) ->
      fprintf fmt
        "(@[<hov 2>CI.make_structured@ %a@ %a)@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakeStructured (t, e) ->
      fprintf fmt
        "@[<hov 2>CI.make_structured@ %a@ %a@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | _, `Fun (xs, e) ->
      fprintf fmt "(@[<1>fun@ %a->@ %a)@]" args xs (ml_exp NoApplParens) e
    | _, `Tuple es ->
      fprintf fmt "(@[%a)@]" tuple_elements es
    | _, `Seq (e1, e2) ->
      fprintf fmt "(@[%a;@ %a)@]" (ml_exp NoApplParens) e1 (ml_exp NoApplParens) e2
    | ApplParens, `Let (p, e1, e2) ->
      fprintf fmt "(@[let@ %a@ = %a@ in@ %a)@]" (ml_pat NoApplParens) p (ml_exp NoApplParens) e1 (ml_exp NoApplParens) e2
    | NoApplParens, `Let (p, e1, e2) ->
      fprintf fmt "@[let@ %a@ = %a@ in@ %a@]" (ml_pat NoApplParens) p (ml_exp NoApplParens) e1 (ml_exp NoApplParens) e2
  and tuple_elements fmt : ml_exp list -> unit =
    fun xs ->
      let last = List.length xs - 1 in
      List.iteri
        (fun i ->
          if i <> last then fprintf fmt "%a,@ " (ml_exp NoApplParens)
          else fprintf fmt "%a" (ml_exp NoApplParens))
        xs
  and ml_pat appl_parens fmt pat =
    match appl_parens, pat with
    | _, `Var x -> fprintf fmt "%s" x
    | _, `Record (fs, `Etc) -> fprintf fmt "{@[%a_}@]" pat_fields fs
    | _, `Record (fs, `Complete) -> fprintf fmt "{@[%a}@]" pat_fields fs
    | _, `As (p, x) -> fprintf fmt "@[(%a@ as@ %s)@]" (ml_pat NoApplParens) p x
    | _, `Underscore -> fprintf fmt "_"
    | _, `Con (c, []) -> fprintf fmt "%a" format_path c
    | NoApplParens, `Con (c, [p]) ->
      fprintf fmt "@[<2>%a@ @[%a@]@]" format_path c (ml_pat ApplParens) p
    | ApplParens, `Con (c, [p]) ->
      fprintf fmt "(@[<2>%a@ @[%a@])@]" format_path c (ml_pat ApplParens) p
    | ApplParens, `Con (c, ps) ->
      fprintf fmt "(@[<2>%a@ (@[%a)@])@]" format_path c pat_args ps
    | NoApplParens, `Con (c, ps) ->
      fprintf fmt "@[<2>%a@ (@[%a)@]@]" format_path c pat_args ps
  and pat_fields fmt : (path * ml_pat) list -> unit =
    List.iter
      (fun (l, p) ->
        fprintf fmt "@[%a@ =@ %a;@]@ " format_path l (ml_pat NoApplParens) p)
  and pat_args fmt : ml_pat list -> unit =
    fun xs ->
      let last = List.length xs - 1 in
      List.iteri
        (fun i ->
          if i <> last then fprintf fmt "%a,@ " (ml_pat NoApplParens)
          else fprintf fmt "%a" (ml_pat NoApplParens))
        xs

  let extern fmt { ident; typ; primname; primname_byte; attributes } =
    fprintf fmt
      "@[<hov 2>@[external@ %s@]@ @[<h 1>:@ @[%a@]@]@ "
      ident ml_external_type typ;
    fprintf fmt
      "@[=@ @[@[%a@]@[%S@]@ %a@]@]@]@."
      primname_opt primname_byte primname attrs attributes
end

let arity : ml_external_type -> int =
  fun (`Prim (args, _)) -> List.length args

let max_byte_args = 5

let byte_stub_name : string -> ml_external_type -> string option =
  fun name t ->
    let arity = arity t in
    if arity > max_byte_args
    then Some (Printf.sprintf "%s_byte%d" name arity)
    else None

let attributes : type a. a fn -> attributes =
   let open Cstubs_analysis in
   fun fn -> { float = float fn; noalloc = not (may_allocate fn) }

let managed_buffer = `Ident (path_of_string "CI.managed_buffer")
let voidp = `Ident (path_of_string "CI.voidp")
let fatptr = `Appl (path_of_string "CI.fatptr", [`Ident (path_of_string "_")])
let fatfunptr = `Appl (path_of_string "CI.fatfunptr", [`Ident (path_of_string "_")])

(* These functions determine the type that should appear in the extern
   signature *)
let rec ml_typ_of_return_typ : type a. a typ -> ml_type =
  function
  | Void -> `Ident (path_of_string "unit")
  | Primitive p -> `Ident (Cstubs_public_name.ident_of_ml_prim (Ctypes_primitive_types.ml_prim p))
  | Struct _    -> managed_buffer
  | Union _     -> managed_buffer
  | Abstract _  -> managed_buffer
  | Pointer _   -> voidp
  | Funptr _    -> voidp
  | View { ty } -> ml_typ_of_return_typ ty
  | Array _    as a -> internal_error
    "Unexpected array type in the return type: %s" (Ctypes.string_of_typ a)
  | Bigarray _ as a -> internal_error
    "Unexpected bigarray type in the return type: %s" (Ctypes.string_of_typ a)
  | OCaml String -> Ctypes_static.unsupported
    "cstubs does not support OCaml strings as return values"
  | OCaml Bytes -> Ctypes_static.unsupported
    "cstubs does not support OCaml bytes values as return values"
  | OCaml FloatArray -> Ctypes_static.unsupported
    "cstubs does not support OCaml float arrays as return values"

let rec ml_typ_of_arg_typ : type a. a typ -> ml_type = function
  | Void -> `Ident (path_of_string "unit")
  | Primitive p -> `Ident (Cstubs_public_name.ident_of_ml_prim (Ctypes_primitive_types.ml_prim p))
  | Pointer _   -> fatptr
  | Funptr _    -> fatfunptr
  | Struct _    -> fatptr
  | Union _     -> fatptr
  | Abstract _  -> fatptr
  | View { ty } -> ml_typ_of_arg_typ ty
  | Array _    as a -> internal_error
    "Unexpected array in an argument type: %s" (Ctypes.string_of_typ a)
  | Bigarray _ as a -> internal_error
    "Unexpected bigarray in an argument type: %s" (Ctypes.string_of_typ a)
  | OCaml String ->
    `Appl (path_of_string "CI.ocaml",
           [`Ident (path_of_string "string")])
  | OCaml Bytes ->
    `Appl (path_of_string "CI.ocaml",
           [`Ident (path_of_string "bytes")])
  | OCaml FloatArray ->
    `Appl (path_of_string "CI.ocaml",
           [`Appl (path_of_string "array",
                   [`Ident (path_of_string "float")])])

type polarity = In | Out

let flip = function
  | In -> Out
  | Out -> In

let ml_typ_of_typ = function
    In -> ml_typ_of_arg_typ
  | Out -> ml_typ_of_return_typ

let lwt_job_type = Ctypes_path.path_of_string "Lwt_unix.job"
let int_type = `Ident (Ctypes_path.path_of_string "Signed.sint")

let rec ml_external_type_of_fn :
  type a. concurrency:concurrency_policy -> errno:errno_policy ->
  a fn -> polarity -> ml_external_type =
  fun ~concurrency ~errno fn polarity -> match fn, concurrency, errno with
    | Returns t, (#non_lwt|`Lwt_preemptive), `Ignore_errno ->
      `Prim ([], ml_typ_of_typ polarity t)
    | Returns t, (#non_lwt|`Lwt_preemptive), `Return_errno ->
      `Prim ([], `Pair (ml_typ_of_typ polarity t, int_type))
    | Returns t, `Lwt_jobs, `Ignore_errno ->
      `Prim ([], `Appl (lwt_job_type, [ml_typ_of_typ polarity t]))
    | Returns t, `Lwt_jobs, `Return_errno ->
      `Prim ([], `Appl (lwt_job_type, [`Pair (ml_typ_of_typ polarity t, int_type)]))
    | Function (f, t), _, _ ->
      let `Prim (l, t) = ml_external_type_of_fn ~concurrency ~errno t polarity in
      `Prim (ml_typ_of_typ (flip polarity) f :: l, t)

let var_counter = ref 0
let fresh_var () =
  incr var_counter;
  Printf.sprintf "x%d" !var_counter

let extern ~concurrency ~errno ~stub_name ~external_name fmt fn =
  let ext =
    let typ = ml_external_type_of_fn ~concurrency ~errno fn Out in
    ({ ident = external_name;
       typ = typ;
       primname = stub_name;
       primname_byte = byte_stub_name stub_name typ;
       attributes = attributes fn; }) in
  Format.fprintf fmt "%a@." Emit_ML.extern ext

let static_con c args =
  `Con (Ctypes_path.path_of_string ("CI." ^ c), args)

let local_con c args =
  `Con (Ctypes_path.path_of_string c, args)

let map_result_id = Ctypes_path.path_of_string "map_result"
let make_ptr = Ctypes_path.path_of_string "CI.make_ptr"
let make_fun_ptr = Ctypes_path.path_of_string "CI.make_fun_ptr"
let make_structured = Ctypes_path.path_of_string "CI.make_structured"

let map_result ~concurrency ~errno f e =
  let map_result f x = `Appl (`Appl (`Ident map_result_id, f), x) in
  match concurrency, errno, f with
    #non_lwt, `Ignore_errno, `MakePtr x ->
    `MakePtr (`Ident (path_of_string x), e)
  | #non_lwt, `Ignore_errno, `MakeFunPtr x ->
    `MakeFunPtr (`Ident (path_of_string x), e)
  | #non_lwt, `Ignore_errno, `MakeStructured x ->
    `MakeStructured (`Ident (path_of_string x), e)
  | #non_lwt, `Ignore_errno, `Appl x ->
    `Appl (`Ident (path_of_string x), e)
  | _, _, `MakePtr x ->
    map_result (`Appl (`Ident make_ptr, `Ident (path_of_string x))) e
  | _, _, `MakeFunPtr x ->
    map_result (`Appl (`Ident make_fun_ptr, `Ident (path_of_string x))) e
  | _, _, `MakeStructured x ->
    map_result (`Appl (`Ident make_structured, `Ident (path_of_string x))) e
  | _, _, `Appl x ->
    map_result (`Ident (path_of_string x)) e 

type pattern_exp_return = ml_pat * ml_exp option * (ml_pat * ml_exp) list

let rec pattern_and_exp_of_typ : type a. concurrency:concurrency_policy -> errno:errno_policy ->
  a typ -> ml_exp -> polarity -> (ml_pat * ml_exp) list -> pattern_exp_return =
  fun ~concurrency ~errno typ e pol binds -> match typ with
  | Void ->
    (static_con "Void" [], None, binds)
  | Primitive p ->
    let id = Cstubs_public_name.constructor_cident_of_prim ~module_name:"CI" p in
    (static_con "Primitive" [`Con (id, [])], None, binds)
  | Pointer _ ->
    begin match pol with
    | In ->
      let pat = static_con "Pointer" [`Underscore] in
      let x = fresh_var () in
      (pat, Some (`Ident (path_of_string x)), binds @ [static_con "CPointer" [`Var x], e])
    | Out ->
      let x = fresh_var () in
      let pat = static_con "Pointer" [`Var x] in
      (pat, Some (map_result ~concurrency ~errno (`MakePtr x) e), binds)
    end
  | Funptr _ ->
    begin match pol with
    | In ->
      let pat = static_con "Funptr" [`Underscore] in
      let x = fresh_var () in
      (pat, Some (`Ident (path_of_string x)), binds @ [static_con "Static_funptr" [`Var x], e])
    | Out ->
      let x = fresh_var () in
      let pat = static_con "Funptr" [`Var x] in
      (pat, Some (map_result ~concurrency ~errno (`MakeFunPtr x) e), binds)
    end
  | Struct _ ->
    begin match pol with
    | In ->
      let pat = static_con "Struct" [`Underscore] in
      let x = fresh_var () in
      (pat, Some (`Ident (path_of_string x)),
       binds @ [static_con "CPointer" [`Var x],
                `Appl (`Ident (path_of_string "Ctypes.addr"), e)])
    | Out ->
      let x = fresh_var () in
      let pat = `As (static_con "Struct" [`Underscore], x) in
      (pat, Some (map_result ~concurrency ~errno (`MakeStructured x) e), binds)
    end
  | Union _ ->
    begin match pol with
    | In ->
      let pat = static_con "Union" [`Underscore] in
      let x = fresh_var () in
      (pat, Some (`Ident (path_of_string x)),
       binds @ [static_con "CPointer" [`Var x],
                `Appl (`Ident (path_of_string "Ctypes.addr"), e)])
    | Out ->
      let x = fresh_var () in
      let pat = `As (static_con "Union" [`Underscore], x) in
      (pat, Some (map_result ~concurrency ~errno (`MakeStructured x) e), binds)
    end
  | View { ty } ->
    begin match pol  with
    | In ->
      let x = fresh_var () in
      let y = fresh_var () in
      let e = `Appl (`Ident (path_of_string x), e) in
      let (p, None, binds), e | (p, Some e, binds), _ =
        pattern_and_exp_of_typ ~concurrency ~errno ty e pol binds, e in
      let pat = static_con "View"
        [`Record ([path_of_string "CI.ty", p;
                   path_of_string "write", `Var x], `Etc)] in
      (pat, Some (`Ident (Ctypes_path.path_of_string y)), (`Var y, e) :: binds)
    | Out ->
      let (p, None, binds), e | (p, Some e, binds), _ =
        pattern_and_exp_of_typ ~concurrency ~errno ty e pol binds, e in
      let x = fresh_var () in
      let pat = static_con "View"
        [`Record ([path_of_string "CI.ty", p;
                   path_of_string "read", `Var x], `Etc)] in
      (pat, Some (map_result ~concurrency ~errno (`Appl x) e), binds)
    end
  | OCaml ty ->
    begin match pol, ty with
    | In, String -> (static_con "OCaml" [static_con "String" []], None, binds)
    | In, Bytes -> (static_con "OCaml" [static_con "Bytes" []], None, binds)
    | In, FloatArray -> (static_con "OCaml" [static_con "FloatArray" []], None, binds)
    | Out, String -> Ctypes_static.unsupported
      "cstubs does not support OCaml strings as return values"
    | Out, Bytes -> Ctypes_static.unsupported
      "cstubs does not support OCaml bytes values as return values"
    | Out, FloatArray -> Ctypes_static.unsupported
      "cstubs does not support OCaml float arrays as return values"
    end
  | Abstract _ as ty -> internal_error
    "Unexpected abstract type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Array _ as ty -> internal_error
    "Unexpected array type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Bigarray _ as ty -> internal_error
    "Unexpected bigarray type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)

(* Build a pattern (without variables) that matches the argument *)
let rec pattern_of_typ : type a. a typ -> ml_pat = function
    Void -> static_con "Void" []
  | Primitive p ->
    let id = Cstubs_public_name.constructor_cident_of_prim ~module_name:"CI" p in
    static_con "Primitive" [`Con (id, [])]
  | Pointer _ ->
    static_con "Pointer" [`Underscore]
  | Funptr _ ->
    static_con "Funptr" [`Underscore]
  | Struct _ ->
    static_con "Struct" [`Underscore]
  | Union _ ->
    static_con "Union" [`Underscore]
  | View { ty } ->
    static_con "View"
      [`Record ([path_of_string "CI.ty", pattern_of_typ ty], `Etc)]
  | Array (_, _) ->
     static_con "Array" [`Underscore; `Underscore]
  | Bigarray _ ->
     static_con "Bigarray" [`Underscore]
  | OCaml String ->
    Ctypes_static.unsupported
      "cstubs does not support OCaml strings as global values"
  | OCaml Bytes ->
    Ctypes_static.unsupported
      "cstubs does not support OCaml bytes values as global values"
  | OCaml FloatArray ->
    Ctypes_static.unsupported
      "cstubs does not support OCaml float arrays as global values"
  | Abstract _ as ty ->
    internal_error
      "Unexpected abstract type encountered during ML code generation: %s"
      (Ctypes.string_of_typ ty)

type wrapper_state = {
  pat: ml_pat;
  exp: ml_exp;
  args: lident list;
  trivial: bool;
  binds: (ml_pat * ml_exp) list;
}

let lwt_unix_run_job = Ctypes_path.path_of_string "Lwt_unix.run_job"
let lwt_preemptive_detach = Ctypes_path.path_of_string "Lwt_preemptive.detach"

let run_exp ~concurrency exp = match concurrency with
    #non_lwt -> exp
  | `Lwt_jobs -> `Appl (`Ident lwt_unix_run_job, exp)
  | `Lwt_preemptive -> `Appl
                         (`Appl (`Ident lwt_preemptive_detach,
                                 `Fun (["_"], exp)),
                          `Unit)

let let_bind : (ml_pat * ml_exp) list -> ml_exp -> ml_exp =
  fun binds e ->
    ListLabels.fold_left ~init:e binds
      ~f:(fun e' (x, e) -> `Let (x, e, e'))

let rec wrapper_body : type a. concurrency:concurrency_policy -> errno:errno_policy ->
  a fn -> ml_exp -> polarity -> (ml_pat * ml_exp) list -> wrapper_state =
  fun ~concurrency ~errno fn exp pol binds -> match fn with
  | Returns t ->
    let exp = run_exp ~concurrency exp in
    begin match pattern_and_exp_of_typ ~concurrency ~errno t exp (flip pol) binds with
      pat, None, binds -> { exp ; args = []; trivial = true; binds;
                            pat = local_con "Returns" [pat] }
    | pat, Some exp, binds -> { exp; args = []; trivial = false; binds;
                                pat = local_con "Returns" [pat] }
    end
  | Function (f, t) ->
    let x = fresh_var () in
    begin match pattern_and_exp_of_typ ~concurrency ~errno
                  f (`Ident (path_of_string x)) pol binds with
    | fpat, None, binds ->
      let { exp; args; trivial; pat = tpat; binds } =
        wrapper_body ~concurrency ~errno
          t (`Appl (exp, `Ident (path_of_string x))) pol binds  in
      { exp; args = x :: args; trivial; binds;
        pat = local_con "Function" [fpat; tpat] }
    | fpat, Some exp', binds ->
      let { exp; args = xs; trivial; pat = tpat; binds } =
        wrapper_body ~concurrency ~errno
          t (`Appl (exp, exp')) pol binds in
      { exp; args = x :: xs; trivial = false; binds;
        pat = local_con "Function" [fpat; tpat] }
    end

let lwt_bind = Ctypes_path.path_of_string "Lwt.bind"
let lwt_return = Ctypes_path.path_of_string "Lwt.return"
let box_lwt = Ctypes_path.path_of_string "box_lwt"
let use_value = Ctypes_path.path_of_string "CI.use_value"

let return_result : args:lident list -> ml_exp =
  fun ~args ->
    let x = fresh_var () in
    (* fun v -> CI.use_value (x1,x2,....xn); Lwt.return v *)
    `Fun ([x],
          `Seq
            (`Appl (`Ident use_value,
                    `Tuple
                      (ListLabels.map args
                         ~f:(fun x -> `Ident (Ctypes_path.path_of_string x)))),
             `Appl (`Ident lwt_return, `Ident (Ctypes_path.path_of_string x))))

(** Returns the variables bound in a pattern, in no particular order *)
let rec pat_bound_vars : ml_pat -> lident list = function
  | `Var x -> [x]
  | `Record (args, _) -> pats_bound_vars (List.map snd args)
  | `As (p, x) -> x :: pat_bound_vars p
  | `Underscore -> []
  | `Con (_, ps) -> pats_bound_vars ps
and pats_bound_vars : ml_pat list -> lident list =
  fun ps -> List.fold_left (fun xs p -> pat_bound_vars p @ xs) [] ps

let wrapper : type a. concurrency:concurrency_policy -> errno:errno_policy ->
  path -> a fn -> string -> polarity -> ml_pat * ml_exp =
  fun ~concurrency ~errno id fn f pol ->
    let p = wrapper_body ~concurrency ~errno fn (`Ident (path_of_string f)) pol [] in
    match p, concurrency with
      { trivial = true; pat; binds }, #non_lwt ->
      (pat, let_bind binds (run_exp ~concurrency (`Ident id)))
    | { exp; args; pat; binds }, #non_lwt ->
      (pat, `Fun (args, let_bind binds exp))
    | { trivial = true; pat; args; binds }, #lwt ->
      let exp : ml_exp = List.fold_left (fun f p -> `Appl (f, `Ident (path_of_string p))) (`Ident id) args in
      (pat, `Fun (args,
                  let_bind binds
                    (`Appl (`Ident box_lwt,
                            `Appl (`Appl (`Ident lwt_bind,
                                          run_exp ~concurrency exp),
                                   return_result ~args:(args
                                                        @ pats_bound_vars
                                                            (List.map fst binds)))))))
    | { exp; args; pat; binds }, #lwt ->
      (pat, `Fun (args,
                  let_bind binds
                    (`Appl (`Ident box_lwt,
                            `Appl (`Appl (`Ident lwt_bind, exp),
                                   return_result ~args:(args @
                                                          pats_bound_vars
                                                            (List.map fst binds)))))))

let case ~concurrency ~errno ~stub_name ~external_name fmt fn =
  let p, e = wrapper ~concurrency ~errno
      (path_of_string external_name) fn external_name In in
  Format.fprintf fmt "@[<hov 2>@[<h 2>|@ @[@[%a@],@ %S@]@ ->@]@ "
    Emit_ML.(ml_pat NoApplParens) p stub_name;
  Format.fprintf fmt "@[<hov 2>@[%a@]@]@]@." Emit_ML.(ml_exp ApplParens) e

let val_case ~stub_name ~external_name fmt typ =
  let x = fresh_var () in
  let p = `As (pattern_of_typ typ, x) in
  let app = `Appl (`Ident (path_of_string external_name), `Unit) in
  let rhs = `MakePtr (`Ident (path_of_string x), app) in
  Format.fprintf fmt "@[<hov 2>@[<h 2>|@ @[@[%a@],@ %S@]@ ->@]@ "
    Emit_ML.(ml_pat NoApplParens) p stub_name;
  Format.fprintf fmt "@[<hov 2>@[%a@]@]@]@."
    Emit_ML.(ml_exp (ApplParens)) rhs

let constructor_decl : type a. concurrency:concurrency_policy ->
  errno:errno_policy -> string -> a fn -> Format.formatter -> unit =
  fun ~concurrency ~errno name fn fmt ->
    Format.fprintf fmt "@[|@ %s@ : (@[%a@])@ name@]@\n" name
      Emit_ML.ml_external_type (ml_external_type_of_fn ~concurrency ~errno fn In)

let inverse_case ~register_name ~constructor name fmt fn : unit =
  let p, e = wrapper ~concurrency:`Sequential ~errno:`Ignore_errno
      (path_of_string "f") fn "f" Out in
  Format.fprintf fmt "|@[ @[%a, %S@] -> %s %s (%a)@]@\n"
    Emit_ML.(ml_pat NoApplParens) p name register_name constructor
    Emit_ML.(ml_exp ApplParens) 
    e
