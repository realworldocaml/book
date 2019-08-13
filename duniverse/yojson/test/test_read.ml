let from_string () =
  Alcotest.(check Testable.yojson)
    __LOC__
    Fixtures.json_value
    (Yojson.Safe.from_string Fixtures.json_string)

let from_file () =
  let input_file = Filename.temp_file "test_yojson_from_file" ".json" in
  let oc = open_out input_file in
  output_string oc Fixtures.json_string;
  close_out oc;
  Alcotest.(check Testable.yojson) __LOC__ Fixtures.json_value (Yojson.Safe.from_file input_file);
  Sys.remove input_file

let single_json = [
  "from_string", `Quick, from_string;
  "from_file", `Quick, from_file;
]
