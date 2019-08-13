open Configurator.V1

let program =
  {|
int main(int argc, char ** argv)
{
  return __builtin_popcount(argc);
}
|}
;;

let () =
  let output = ref "" in
  main
    ~name:"discover"
    ~args:[ "-o", Set_string output, "FILENAME output file" ]
    (fun c ->
       let has_popcnt = c_test c ~c_flags:[ "-mpopcnt" ] program in
       Flags.write_sexp !output (if has_popcnt then [ "-mpopcnt" ] else []))
;;
