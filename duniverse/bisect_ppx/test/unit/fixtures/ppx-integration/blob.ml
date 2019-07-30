let me () = Printf.printf "ME: %s\n" [%blob "blob.ml"]

let me' () = print_endline "foo"; [%blob "blob.ml"]
