(* -*- tuareg -*- *)

open StdLabels
open Printf

let nl () = printf "\n"

let supported_versions =
  [
    ("402", "4.02");
    ("403", "4.03");
    ("404", "4.04");
    ("405", "4.05");
    ("406", "4.06");
    ("407", "4.07");
    ("408", "4.08");
    ("409", "4.09");
    ("410", "4.10");
    ("411", "4.11");
    ("412", "4.12");
    ("413", "4.13");
    ("414", "4.14");
  ]

let foreach_version f =
  nl ();
  List.iter supported_versions ~f:(fun (suffix, version) -> f suffix version)

let foreach_version_pair f =
  nl ();
  let rec aux = function
    | (x, _) :: ((y, _) :: _ as tail) ->
        f x y;
        aux tail
    | [ _ ] | [] -> ()
  in
  aux supported_versions
