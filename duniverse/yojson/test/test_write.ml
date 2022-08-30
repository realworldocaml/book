let to_string_tests =
  let test ?suf expected = 
    Alcotest.(check string) __LOC__ expected (Yojson.Safe.to_string ?suf Fixtures.json_value)
  in
  [
    "to_string with default settings", `Quick, (fun () -> test Fixtures.json_string);
    "to_string with newline", `Quick, (fun () -> test ~suf:"\n" Fixtures.json_string_newline);
    "to_string without newline", `Quick, (fun () -> test ~suf:"" Fixtures.json_string);
  ]

let to_file_tests =
  let test ?suf expected =
    let output_file = Filename.temp_file "test_yojson_to_file" ".json" in
    Yojson.Safe.to_file ?suf output_file Fixtures.json_value;
    let file_content =
      let ic = open_in output_file in
      let length = in_channel_length ic in
      let s = really_input_string ic length in
      close_in ic;
      s
    in
    Sys.remove output_file;
    Alcotest.(check string) __LOC__ expected file_content
  in
  [
    "to_file with default settings", `Quick, (fun () -> test Fixtures.json_string_newline);
    "to_file with newline", `Quick, (fun () -> test ~suf:"\n" Fixtures.json_string_newline);
    "to_file without newline", `Quick, (fun () -> test ~suf:"" Fixtures.json_string);
  ]

(* List.to_seq is not available on old OCaml versions. *)
let rec list_to_seq = function
  | [] -> (fun () -> Seq.Nil)
  | x :: xs -> (fun () -> Seq.Cons (x, list_to_seq xs))

let seq_to_file_tests =
  let test ?suf () =
    let output_file = Filename.temp_file "test_yojson_seq_to_file" ".json" in
    let data = [`String "foo"; `String "bar"] in
    Yojson.Safe.seq_to_file ?suf output_file (list_to_seq data);
    let read_data =
      let seq = Yojson.Safe.seq_from_file output_file in
      let acc = ref [] in
      Seq.iter (fun v -> acc := v :: !acc) seq;
      List.rev !acc
    in
    Sys.remove output_file;
    Alcotest.(check (list Testable.yojson)) "seq_{to,from}_file roundtrip" data read_data
  in
  [
    "seq_to_file with default settings", `Quick, (fun () -> test ());
    "seq_to_file with newline", `Quick, (fun () -> test ~suf:"\n" ());
    "seq_to_file without newline", `Quick, (fun () -> test ~suf:"" ());
  ]

let single_json =
  List.flatten [
    to_file_tests;
    to_string_tests;
    seq_to_file_tests;
  ]
