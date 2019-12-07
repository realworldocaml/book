let with_formatter ~path f =
  let chan = open_out path in
  f Format.(formatter_of_out_channel chan);
  close_out chan
;;

with_formatter
  ~path:"bench_micro_generated_stubs.c"
  (fun fmt ->
    Format.fprintf fmt "#include \"bench_micro_stubs.h\"\n\n";
    Cstubs.write_c fmt ~prefix:"bench_micro" (module Bench_micro_bindings.Make));

with_formatter
  ~path:"bench_micro_generated.ml"
  (fun fmt ->
    Cstubs.write_ml fmt ~prefix:"bench_micro" (module Bench_micro_bindings.Make))
