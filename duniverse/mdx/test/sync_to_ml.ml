let do_hash file =
   In_channel.with_file file ~f:(fun ic ->
     let open Cryptokit in
     hash_channel (Hash.md5 ()) ic
     |> transform_string (Hexa.encode ())
     |> print_endline
   )
[@@@part "1"];;
let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
[@@@part "toto"];;
let x = 34
let f = 42.3
let s = "toto"
let f x u = u x

let () =
  print_int x;
  print_float f
;;

[@@@part "zzz"];;
let () =
  print_string s
;;

[@@@part "42"];;

let () =
  f x print_int
