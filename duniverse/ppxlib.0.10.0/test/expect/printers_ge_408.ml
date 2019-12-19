let print_loc _ _ ppf (loc : Location.t) =
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line _";
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@.";
;;

let report_printer () =
  let printer = Location.default_report_printer () in
  { printer with Location. pp_main_loc = print_loc; pp_submsg_loc = print_loc; }
;;

let setup ppf =
  Location.formatter_for_warnings := ppf;
  Location.warning_reporter := Location.default_warning_reporter;
  Location.report_printer   := report_printer;
  Location.alert_reporter   := Location.default_alert_reporter;
;;
