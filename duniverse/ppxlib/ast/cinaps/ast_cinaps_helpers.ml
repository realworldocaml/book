(* -*- tuareg -*- *)

include StdLabels
include Printf

let nl () = printf "\n"

let qualified_types =
  [
    ( "Parsetree",
      [
        "structure";
        "signature";
        "toplevel_phrase";
        "core_type";
        "expression";
        "pattern";
        "case";
        "type_declaration";
        "type_extension";
        "extension_constructor";
      ] );
  ]

let all_types = List.concat (List.map ~f:snd qualified_types)

let foreach_module f =
  nl ();
  List.iter qualified_types ~f:(fun (m, types) -> f m types)

let foreach_type f = foreach_module (fun m -> List.iter ~f:(f m))

let foreach_version f =
  nl ();
  List.iter Supported_version.all ~f:(fun v ->
      f (Supported_version.to_int v) (Supported_version.to_string v))

let foreach_version_pair f =
  nl ();
  let rec aux = function
    | x :: (y :: _ as tail) ->
        f (Supported_version.to_int x) (Supported_version.to_int y);
        aux tail
    | [ _ ] | [] -> ()
  in
  aux Supported_version.all

let with_then_and () =
  let first = ref true in
  fun oc ->
    output_string oc (if !first then "with" else " and");
    first := false
