open Core.Std

let _ =
  (* Read JSON file into an OCaml string *)
  let buf = In_channel.read_all "book.json" in

  (* Use the string JSON constructor *)
  let json1 = Yojson.Basic.from_string buf in

  (* Use the file JSON constructor *)
  let json2 = Yojson.Basic.from_file "book.json" in

  print_endline (if json1 = json2 then "OK" else "FAIL");
  print_endline (if phys_equal json1 json2 then "FAIL" else "OK")
