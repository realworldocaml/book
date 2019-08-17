let () =
  let ver, output =
    try
      match Sys.argv with
      | [|_; "-ocaml-version"; v; "-o"; fn|] ->
        (Scanf.sscanf v "%d.%d" (fun major minor -> (major, minor)),
         fn)
      | _ -> raise Exit
    with _ ->
      failwith "bad command line arguments"
  in
  let prefix =
    if ver >= (4, 04) then "bytes" else "string"
  in
  let oc = open_out output in
  Printf.fprintf oc {|
external set        : %s -> int -> char -> unit = "%%%s_safe_set"
external unsafe_set : %s -> int -> char -> unit = "%%%s_unsafe_set"
|} prefix prefix prefix prefix;
  close_out oc
