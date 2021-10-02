module List = ListLabels
open MoreLabels
module Smap = Map.Make (String)

let () =
  let dir, oc =
    match Sys.argv with
    | [| _; "-dir"; dir; "-o"; fn |] -> (dir, open_out fn)
    | _ -> failwith "bad command line arguments"
  in

  let files =
    Sys.readdir dir |> Array.to_list |> List.sort ~cmp:String.compare
  in

  let all_exposed_modules =
    List.filter files ~f:(fun fn -> Filename.check_suffix fn ".cmi")
    |> List.map ~f:(fun fn ->
           String.capitalize_ascii (Filename.chop_extension fn))
  in

  let module_to_lib =
    List.filter files ~f:(fun fn -> Filename.check_suffix fn ".cma")
    |> List.fold_left ~init:Smap.empty ~f:(fun acc fn ->
           let lib_mod =
             try Scanf.sscanf fn "ocaml%s.cma" (fun s -> "Ocaml_" ^ s) with
             | _ -> String.capitalize_ascii (Filename.chop_extension fn)
           in
           let units = Read_cma.units (Filename.concat dir fn) in
           List.fold_left units ~init:acc ~f:(fun acc unit ->
               Smap.add acc ~key:unit ~data:lib_mod))
  in

  (* If we keep the alias, we can't use -linkall... *)
  Printf.fprintf oc "module Do_not_use_directly = struct end\n";
  List.sort all_exposed_modules ~cmp:String.compare
  |> List.iter ~f:(fun m ->
         let repl =
           match Smap.find m module_to_lib with
           | lib ->
             let lib =
               let prefix = "Ocaml" in
               let prefix_len = String.length prefix in
               let lib_len = String.length lib in
               if lib_len >= prefix_len && String.sub lib 0 prefix_len = prefix
               then
                 prefix ^ "_" ^ String.sub lib prefix_len (lib_len - prefix_len)
               else
                 lib
             in
             Printf.sprintf ", use %s.%s instead" lib m
           | exception Not_found -> ""
         in
         Printf.fprintf oc
           "module %s = Do_not_use_directly [@@deprecated \"Accessing this \
            module directly is deprecated%s\"]\n"
           m repl)
