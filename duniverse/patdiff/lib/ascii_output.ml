open! Core
open! Import


module Rule = struct
  let apply s ~rule ~refined:_ =
    Ansi_output.Rule.apply s ~rule:(Patdiff_format.Rule.strip_styles rule) ~refined:false
  ;;
end

let print ~print_global_header ~file_names ~rules ~print ~location_style hunks =
  let rules = Patdiff_format.Rules.strip_styles rules in
  Ansi_output.print ~print_global_header ~file_names ~rules ~print ~location_style hunks
;;
