open Core
open Core_bench_internals
open Core_bench_internals.Display

let display ?libname ~display_config results =
  if display_config.Display_config.show_output_as_sexp
  then Simplified_benchmark.to_sexp ?libname results |> Sexp.to_string |> print_endline
  else (
    let cols = make_columns display_config results in
    print_endline
    @@ Ascii_table_kernel.to_string_noattr
         ~limit_width_to:(Display_config.limit_width_to display_config)
         ~bars:(if Display_config.ascii_table display_config then `Ascii else `Unicode)
         ~display:(Display_config.display display_config)
         cols
         results;
    Warnings.display ())
;;
