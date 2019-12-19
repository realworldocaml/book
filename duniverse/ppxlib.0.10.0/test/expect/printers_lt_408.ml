let print_loc ppf (loc : Location.t) =
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line _";
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@.";
;;

let warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs = _ } ->
    print_loc ppf loc;
    if is_error
    then
      Format.fprintf ppf "Error (warning %d): %s@." number message
    else Format.fprintf ppf "Warning %d: %s@." number message
;;

let rec error_reporter ppf {Location.loc; msg; sub; if_highlight=_} =
  print_loc ppf loc;
  Format.fprintf ppf "Error: %s" msg;
  List.iter (fun err ->
    Format.fprintf ppf "@\n@[<2>%a@]" error_reporter err)
    sub
;;

let setup ppf =
  Location.formatter_for_warnings := ppf;
  Location.warning_printer := warning_printer;
  Location.error_reporter := error_reporter;
;;
