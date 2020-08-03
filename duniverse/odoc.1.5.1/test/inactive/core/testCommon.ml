
open DocOck
open Paths
open Types

class ident = object
  method root x = x
  inherit [string] Maps.paths
  inherit [string] Maps.types
end

exception Error of string * string

let module_name file =
  let base = Filename.basename file in
  let prefix =
    try
      let pos = String.index base '.' in
        String.sub base 0 pos
    with Not_found -> base
  in
    String.capitalize_ascii prefix

let to_root file =
  let name = module_name file in
  name, Paths.contains_double_underscore name

let check_identity_map file intf =
  let ident = new ident in
  let intf' = ident#unit intf in
    if intf != intf' then
      raise (Error(file, "deep identity map failed"))
    else ()

let lookup files =
  let names = List.map to_root files in
    fun file source name ->
      if (Identifier.name source.Unit.id) <> (module_name file) then
        raise (Error(file, "bad lookup during resolution"));
      match List.assoc name names with
      | hidden -> Found { root = name; hidden }
      | exception Not_found -> Not_found

let fetch intfs =
  let intfs =
    List.map (fun (file, intf) -> (module_name file, intf)) intfs
  in
    fun file name ->
      try
        List.assoc name intfs
      with Not_found ->
        let msg = "bad fetch of " ^ name ^ " during resolution" in
          raise (Error(file, msg))

let resolve_file lookup fetch file intf =
  let resolver = build_resolver (lookup file) (fetch file) in
  resolve resolver intf

let expand_file fetch file intf =
  let expander =
    build_expander (fun _ -> Not_found) (fun ~root:_ name -> fetch file name)
  in
  expand expander intf

let test read files =
  let intfs =
    List.map (fun file -> file, read file) files
  in
  List.iter (fun (file, intf) -> check_identity_map file intf) intfs;
  let lookup = lookup files in
  let fetch = fetch intfs in
  let intfs' =
    List.map
      (fun (file, intf) -> file, resolve_file lookup fetch file intf)
      intfs
  in
  ignore
    (List.map (fun (file, intf) -> file, expand_file fetch file intf) intfs')

let get_files kind =
  let files = ref [] in
  let add_file file =
    files := file :: !files
  in
    Arg.parse [] add_file ("Test doc-ock on " ^ kind ^ " files");
    !files
