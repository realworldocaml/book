open StdLabels

let () =
  let cmi_fn, oc =
    match Sys.argv with
    | [| _; "-caml-cmi"; cmi_fn; "-o"; fn |] -> cmi_fn, open_out fn
    | [| _; "-caml-cmi"; cmi_fn1; cmi_fn2; "-o"; fn |] ->
      let cmi_fn = if Sys.file_exists cmi_fn1 then cmi_fn1 else cmi_fn2 in
      cmi_fn, open_out fn
    | _ -> failwith "bad command line arguments"
  in
  try
    let cmi = Cmi_format.read_cmi cmi_fn in
    let buf = Buffer.create 512 in
    let pp = Format.formatter_of_buffer buf in
    Format.pp_set_margin pp max_int;
    (* so we can parse line by line below *)
    Format.fprintf pp "%a@." Printtyp.signature cmi.Cmi_format.cmi_sign;
    let s = Buffer.contents buf in
    let lines = Str.split (Str.regexp "\n") s in
    Printf.fprintf oc "[@@@warning \"-3\"]\n\n";
    List.iter lines ~f:(fun line ->
      let repl = Mapper.line (Lexing.from_string line) in
      if repl <> "" then Printf.fprintf oc "%s\n\n" repl);
    flush oc
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
;;
