#if OCAML_VERSION < (4, 03, 0)
#define Pconst_string Const_string
#endif

open Asttypes
open Parsetree
open Ast_helper

let raise_errorf = Ppx_deriving.raise_errorf

let dynlink ?(loc=Location.none) filename =
  let filename = Dynlink.adapt_filename filename in
  try
    Dynlink.loadfile filename
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let init_findlib = lazy (
  Findlib.init ();
  Findlib.record_package Findlib.Record_core "ppx_deriving.api";
)

let load_ocamlfind_package ?loc pkg =
  Lazy.force init_findlib;
  try
    Fl_dynload.load_packages [pkg]
  with Dynlink.Error error ->
    raise_errorf ?loc "Cannot load %s: %s" pkg (Dynlink.error_message error)

let load_plugin ?loc plugin =
  let len = String.length plugin in
  let pkg_prefix = "package:" in
  let pkg_prefix_len = String.length pkg_prefix in
  if len >= pkg_prefix_len &&
     String.sub plugin 0 pkg_prefix_len = pkg_prefix then
    let pkg = String.sub plugin pkg_prefix_len (len - pkg_prefix_len) in
    load_ocamlfind_package ?loc pkg
  else
    dynlink ?loc plugin

let get_plugins () =
  match Ast_mapper.get_cookie "ppx_deriving" with
  | Some { pexp_desc = Pexp_tuple exprs } ->
    exprs |> List.map (fun expr ->
      match expr with
      | { pexp_desc = Pexp_constant (Pconst_string (file, None)) } -> file
      | _ -> assert false)
  | Some _ -> assert false
  | None -> []

let add_plugins plugins =
  let loaded  = get_plugins () in
  let plugins = List.filter (fun file -> not (List.mem file loaded)) plugins in
  List.iter load_plugin plugins;
  let loaded  = loaded @ plugins in
  Ast_mapper.set_cookie "ppx_deriving"
    (Exp.tuple (List.map (fun file -> Exp.constant (Pconst_string (file, None))) loaded))

let mapper argv =
  get_plugins () |> List.iter load_plugin;
  add_plugins argv;
  let omp_mapper = Migrate_parsetree.Driver.run_as_ast_mapper [] in
  let structure mapper = function
    | [%stri [@@@findlib.ppxopt [%e? { pexp_desc = Pexp_tuple (
          [%expr "ppx_deriving"] :: elems) }]]] :: rest ->
      elems |>
        List.map (fun elem ->
          match elem with
          | { pexp_desc = Pexp_constant (Pconst_string (file, None))} -> file
          | _ -> assert false) |>
        add_plugins;
        mapper.Ast_mapper.structure mapper rest
    | items -> omp_mapper.Ast_mapper.structure mapper items in
  { omp_mapper with Ast_mapper.structure }

let () =
  Ast_mapper.register "ppx_deriving" mapper

