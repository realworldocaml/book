include Base
include Ppxlib
include Ast_builder.Default

(* errors and error messages *)

let ( ^^ ) = Caml.( ^^ )
let error ~loc fmt = Location.raise_errorf ~loc ("ppx_quickcheck: " ^^ fmt)
let invalid ~loc fmt = error ~loc ("invalid syntax: " ^^ fmt)
let unsupported ~loc fmt = error ~loc ("unsupported: " ^^ fmt)
let internal_error ~loc fmt = error ~loc ("internal error: " ^^ fmt)

let short_string_of_core_type core_type =
  match core_type.ptyp_desc with
  | Ptyp_any -> "wildcard type"
  | Ptyp_var _ -> "type variable"
  | Ptyp_arrow _ -> "function type"
  | Ptyp_tuple _ -> "tuple type"
  | Ptyp_constr _ -> "type name"
  | Ptyp_object _ -> "object type"
  | Ptyp_class _ -> "class type"
  | Ptyp_alias _ -> "type variable alias"
  | Ptyp_variant _ -> "polymorphic variant"
  | Ptyp_poly _ -> "explicit polymorphic type"
  | Ptyp_package _ -> "first-class module type"
  | Ptyp_extension _ -> "ppx extension type"
;;

(* little syntax helpers *)

let loc_map { loc; txt } ~f = { loc; txt = f txt }
let lident_loc = loc_map ~f:lident

let prefixed_type_name prefix type_name =
  match type_name with
  | "t" -> prefix
  | _ -> prefix ^ "_" ^ type_name
;;

let generator_name type_name = prefixed_type_name "quickcheck_generator" type_name
let observer_name type_name = prefixed_type_name "quickcheck_observer" type_name
let shrinker_name type_name = prefixed_type_name "quickcheck_shrinker" type_name
let pname { loc; txt } ~f = pvar ~loc (f txt)
let ename { loc; txt } ~f = evar ~loc (f txt)
let pgenerator = pname ~f:generator_name
let pobserver = pname ~f:observer_name
let pshrinker = pname ~f:shrinker_name
let egenerator = ename ~f:generator_name
let eobserver = ename ~f:observer_name
let eshrinker = ename ~f:shrinker_name

let ptuple ~loc list =
  match list with
  | [] -> [%pat? ()]
  | [ pat ] -> pat
  | _ -> ppat_tuple ~loc list
;;

(* creating (probably-)unique symbols for generated code *)

let gensym prefix loc =
  let loc = { loc with loc_ghost = true } in
  let sym = gen_symbol ~prefix:("_" ^ prefix) () in
  pvar ~loc sym, evar ~loc sym
;;

let gensyms prefix loc_list = List.map loc_list ~f:(gensym prefix) |> List.unzip

let gensymss prefix loc_list_list =
  List.map loc_list_list ~f:(gensyms prefix) |> List.unzip
;;

(* expression to create a higher order function that maps from function with one kind of
   argument label to another *)

let fn_map_label ~loc ~from ~to_ =
  let f_pat, f_expr = gensym "f" loc in
  let x_pat, x_expr = gensym "x" loc in
  pexp_fun
    ~loc
    Nolabel
    None
    f_pat
    (pexp_fun ~loc to_ None x_pat (pexp_apply ~loc f_expr [ from, x_expr ]))
;;
