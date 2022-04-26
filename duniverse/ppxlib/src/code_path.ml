open! Import

type t = {
  file_path : string;
  main_module_name : string;
  submodule_path : string loc list;
  value : string loc option;
  in_expr : bool;
}

let top_level ~file_path =
  let main_module_name =
    file_path |> Caml.Filename.basename |> Caml.Filename.remove_extension
    |> String.capitalize_ascii
  in
  {
    file_path;
    main_module_name;
    submodule_path = [];
    value = None;
    in_expr = false;
  }

let file_path t = t.file_path
let main_module_name t = t.main_module_name

let submodule_path t =
  List.rev_map ~f:(fun located -> located.txt) t.submodule_path

let value t = Option.map ~f:(fun located -> located.txt) t.value

let fully_qualified_path t =
  let value = value t in
  let submodule_path =
    List.rev_map ~f:(fun located -> Some located.txt) t.submodule_path
  in
  let names = (Some t.main_module_name :: submodule_path) @ [ value ] in
  String.concat ~sep:"." @@ List.filter_opt names

let enter_expr t = { t with in_expr = true }

let enter_module ~loc module_name t =
  if t.in_expr then t
  else
    { t with submodule_path = { txt = module_name; loc } :: t.submodule_path }

let enter_value ~loc value_name t =
  if t.in_expr then t else { t with value = Some { txt = value_name; loc } }

let to_string_path t = String.concat ~sep:"." (t.file_path :: submodule_path t)
let with_string_path f ~loc ~path = f ~loc ~path:(to_string_path path);;

let module M = struct
  let a = "lol"
end in
M.a
