(* Basic. *)
let () =
  try
    print_endline "foo"
  with
  | Exit -> ()
  | Not_found -> print_endline "bar"

(* In tail position. *)
let f () =
  try
    print_endline "foo"
  with
  | Exit -> ()
  | Not_found -> print_endline "bar"

(* Or-pattern. *)
let () =
  try
    ()
  with
  | Exit | Not_found -> print_endline "bar"
