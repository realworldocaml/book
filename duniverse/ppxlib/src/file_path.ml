open! Import

let chop_prefix ~prefix x =
  if String.is_prefix ~prefix x then
    Some (String.drop_prefix x (String.length prefix))
  else
    None
;;

let get_default_path (loc : Location.t) =
  let fname = loc.loc_start.pos_fname in
  match chop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None       -> fname
;;

let get_default_path_str : structure -> string = function
  | [] -> ""
  | { pstr_loc = loc; _ } :: _ -> get_default_path loc
;;

let get_default_path_sig : signature -> string = function
  | [] -> ""
  | { psig_loc = loc; _ } :: _ -> get_default_path loc
;;
