(* Translation environment *)

type id = string
type ty_name = string

(* Java types *)
type ty =
  [ `Class of ty_name * (id * ty_name) list
      (* Class name and constructor parameters *)
  | `Interface of ty_name
      (* Interface name *)
  ]

type env_t = {
  module_items : (string * Atd.Ast.type_expr) list;
  package      : string;
  package_dir  : string;
  input_file   : string option;
}

let default_env = {
  module_items = [];
  package      = "out";
  package_dir  = "out";
  input_file   = None;
}
