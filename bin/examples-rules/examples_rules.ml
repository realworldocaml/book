module List = ListLabels

let failf fmt =
  Format.ksprintf (fun msg -> Format.eprintf "%s\n" msg; exit 1) fmt

module Config = struct
  type t =
    { packages : string list }

  let empty = { packages = [] }

  let parse_packages ~path packages =
    List.map packages
      ~f:(function
          | Sexplib.Sexp.Atom s -> s
          | _ -> failf "Invalid package field in %s" path)

  let parse path =
    let ic = open_in path in
    let sexps = Sexplib.Sexp.input_sexps ic in
    close_in ic;
    match sexps with
    | [] -> empty
    | [List ((Atom "packages") :: packages)] ->
      { packages = parse_packages ~path packages }
    | _ -> failf "Invalid example config at %s" path

  let path = ".rwo-example"
end

let packages_to_string packages =
  match packages with
  | [] -> ""
  | _ ->
    "\n" ^
    String.concat "\n" (List.map packages ~f:(Printf.sprintf "  (package %s)"))

let print_rule ~dir_name ~path (config : Config.t) =
  Printf.printf
    {|
(rule
 (alias %s)
 (deps
  (source_tree %s)%s)
 (action
  (run dune build @all @runtest --root %s)))

(alias
 (name runtest)
 (deps (alias %s)))
|}
    dir_name
    path
    (packages_to_string config.packages)
    path
    dir_name

let run examples_dir =
  let correct = Filename.concat examples_dir "correct" in
  let is_dir path = Filename.concat correct path |> Sys.is_directory in
  let sub_dirs =
    Sys.readdir correct
    |> Array.to_list
    |> List.sort ~cmp:String.compare
    |> List.filter ~f:is_dir
  in
  List.iter sub_dirs
    ~f:(fun dir_name ->
        let path = Filename.concat correct dir_name in
        let config_file = Filename.concat path Config.path in
        let config =
          if Sys.file_exists config_file then
            Config.parse config_file
          else
            Config.empty
        in
        print_rule ~dir_name ~path config)

let () =
  match Sys.argv.(1) with
  | example_dir -> run example_dir
  | exception Invalid_argument _ -> failf ""
