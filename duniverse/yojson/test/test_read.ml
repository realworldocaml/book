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

let unquoted_from_string () =
  Alcotest.(check Testable.yojson)
    __LOC__
    Fixtures.unquoted_value
    (Yojson.Safe.from_string Fixtures.unquoted_json)

let map_ident_and_string () =
  let lexbuf = Lexing.from_string {|{foo:"hello"}|} in
  let lexer_state = Yojson.init_lexer () in

  let ident_expected expectation reference start len =
    let identifier = String.sub reference start len in
    Alcotest.(check string)
      (Format.asprintf "Reference '%s' start %d len %d matches '%s'" reference start len expectation)
      expectation
      identifier;
    ()
  in
  let skip_over f =
    f lexer_state lexbuf
  in
  let map_f mapper f =
    mapper lexer_state f lexbuf
  in
  let map_ident = map_f Yojson.Safe.map_ident in
  let map_string = map_f Yojson.Safe.map_string in

  skip_over Yojson.Safe.read_lcurl;
  map_ident (ident_expected "foo");
  skip_over Yojson.Safe.read_colon;

  let variant = skip_over Yojson.Safe.start_any_variant in
  Alcotest.(check Testable.variant_kind) "String starts with double quote" `Double_quote variant;

  map_string (ident_expected "hello");

  Alcotest.check_raises
    "Reading } raises End_of_object"
    Yojson.End_of_object
    (fun () -> Yojson.Safe.read_object_end lexbuf)

let single_json = [
  "from_string", `Quick, from_string;
  "from_file", `Quick, from_file;
  "unquoted_from_string", `Quick, unquoted_from_string;
  "map_ident/map_string", `Quick, map_ident_and_string;
]
