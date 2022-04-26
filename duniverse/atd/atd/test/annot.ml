(*
   Unit tests for Atd.Annot
*)

open Printf

let schema : Atd.Annot.schema = [
  {
    section = "foo";
    fields = [
      Module_head, "a";
      Type_def, "b";
      Type_expr, "c";
      Variant, "d";
      Cell, "e";
      Field, "f";
      Field, "g";
    ]
  };
  {
    section = "bar";
    fields = [
      Module_head, "m";
      Type_def, "n";
      Type_expr, "o";
      Variant, "p";
      Cell, "q";
      Field, "r";
    ]
  }
]

let test_valid_input atd_input =
  let root, _orig_types = Atd.Util.load_string atd_input in
  Atd.Annot.validate schema (Atd.Ast.Full_module root)

let test_invalid_input atd_input =
  let root, _orig_types = Atd.Util.load_string atd_input in
  try
    Atd.Annot.validate schema (Atd.Ast.Full_module root);
    assert false
  with Atd.Ast.Atd_error msg ->
    printf "Error (expected): %s\n%!" msg

let test_valid () =
  test_valid_input {|
<foo a>
<bar m>

type record <foo b> = {
  field
    <buz x>
    <foo f>
    <bar r="x">
    <foo g>:
      int <foo c>
        list <foo c="">;
}

type sum = [
| Hello <bar p> of string <foo c>
| World <foo d>
| Tuple of (<foo e>: int * <bar q>: string * record option <bar o>) <foo c>
]
|}

let test_invalid1 () =
  test_invalid_input {|<foo hello>|}

let test_invalid2 () =
  test_invalid_input {|type t = int <foo f>|}

let test_invalid3 () =
  test_invalid_input {|type t = int <foo f> list list list|}

let test_invalid4 () =
  test_invalid_input {|type t <foo b c> = int|}

let test_invalid5 () =
  test_invalid_input {|
type t = {
  ?k: (int * int <foo f> list) option
}
|}

let test_invalid6 () =
  test_invalid_input {|
type t = {
  ?k <foo x>: int;
}
|}

let test_invalid7 () =
  test_invalid_input {| type t = [ A <foo f> ] |}

let test_invalid8 () =
  test_invalid_input {| type t = [ A <foo f> of int ] |}

let test_invalid9 () =
  test_invalid_input {|type t = (<foo f>: int * int)|}

let test = "Annot", [
  "valid", `Quick, test_valid;
  "invalid1", `Quick, test_invalid1;
  "invalid2", `Quick, test_invalid2;
  "invalid3", `Quick, test_invalid3;
  "invalid4", `Quick, test_invalid4;
  "invalid5", `Quick, test_invalid5;
  "invalid6", `Quick, test_invalid6;
  "invalid7", `Quick, test_invalid7;
  "invalid8", `Quick, test_invalid8;
  "invalid9", `Quick, test_invalid9;
]
