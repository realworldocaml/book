let test_cases_dir = "test_cases"

let test ~accepted ~rejected ~dir_name file_name =
  try
    let json = Yojson.Basic.from_file @@ Filename.concat dir_name file_name in
    let pped = Yojson.Basic.show json in
    accepted file_name pped
  with
  | Yojson.Json_error msg ->
    rejected file_name msg

let pass = fun _ _ -> ()

let fail fmt filename v = Alcotest.failf fmt filename v

let test_parses =
  test
    ~accepted:pass
    ~rejected:(fail "%s is valid JSON but failed with Json_error %s")

let test_rejects =
  test
    ~accepted:(fail "%s is invalid JSON but parsed to %s")
    ~rejected:pass

let test_any =
  test
    ~accepted:pass
    ~rejected:pass

let test_file ~dir_name file_name =
  match file_name.[0] with
  | 'y' -> test_parses ~dir_name file_name
  | 'n' -> test_rejects ~dir_name file_name
  | 'i' -> test_any ~dir_name file_name
  | _ -> assert false

let basic =
  let test_files = Array.to_list @@ Sys.readdir test_cases_dir in
  let sorted = List.sort String.compare test_files in
  List.rev_map
    ( fun base_name ->
        (base_name, `Quick, fun () -> test_file ~dir_name:test_cases_dir base_name)
    )
    sorted

let () =
  Alcotest.run
    "RFC 8259 Compliance"
    [ "Yojson.Basic", basic
    ]
