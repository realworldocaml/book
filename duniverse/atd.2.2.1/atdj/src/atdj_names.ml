open Atd.Import
(* Names *)

let to_camel_case (s : string) =
  let res    = Bytes.of_string s in
  let offset = ref 0 in
  let upper  = ref true in
  let f = function
    | '_' ->
        upper := true;
    | ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') as x ->
        upper := true;
        Bytes.set res !offset x;
        incr offset
    | _ as x ->
        if !upper then (
          Bytes.set res !offset (Char.uppercase_ascii x);
          upper := false
        ) else
          Bytes.set res !offset x;
        incr offset in
  String.iter f s;
  Bytes.to_string (Bytes.sub res 0 !offset)

(* Translate type names into idiomatic Java class names.  We special case
 * `string', `int' and `bool'  (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name str =
  match str with
    | "string" -> "String"
    | "int"    -> "Integer"
    | "bool"   -> "Boolean"
    | "float"  -> "Double"
    | _ -> to_camel_case str

let java_keywords = [
  "abstract";
  "assert";
  "boolean";
  "break";
  "byte";
  "case";
  "catch";
  "char";
  "class";
  "const";
  "continue";
  "default";
  "do";
  "double";
  "else";
  "enum";
  "extends";
  "final";
  "finally";
  "float";
  "for";
  "goto";
  "if";
  "implements";
  "import";
  "instanceof";
  "int";
  "interface";
  "long";
  "native";
  "new";
  "package";
  "private";
  "protected";
  "public";
  "return";
  "short";
  "static";
  "strictfp";
  "super";
  "switch";
  "synchronized";
  "this";
  "throw";
  "throws";
  "transient";
  "try";
  "void";
  "volatile";
  "while";
]

let is_java_keyword =
  let tbl = Hashtbl.create 200 in
  List.iter (fun k -> Hashtbl.add tbl k ()) java_keywords;
  fun k -> Hashtbl.mem tbl k

(*
   Automatically append an underscore to a field name if it is a Java keyword.
   Use the alternative provided as <java name ="..."> if available.

   ATD field                           Java name

   not_a_keyword                       not_a_keyword
   class                               class_
   class <java name="class_name">      class_name
   not_a_keyword <java name="class">   class

*)
let get_java_field_name field_name annot =
  let field_name =
    if is_java_keyword field_name then
      field_name ^ "_"
    else
      field_name
  in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["java"]
    ~field:"name"
    annot

let get_java_variant_names field_name annot =
  let lower_field_name = String.lowercase_ascii field_name in
  let field_name =
    if is_java_keyword lower_field_name then
      field_name ^ "_"
    else
      field_name
  in
  let field_name =
    Atd.Annot.get_field
      ~parse:(fun s -> Some s)
      ~default:field_name
      ~sections:["java"]
      ~field:"name"
      annot
  in
  let func_name = to_camel_case field_name in
  let enum_name = String.uppercase_ascii field_name in
  let private_field_name = String.lowercase_ascii field_name in
  func_name, enum_name, private_field_name

let get_json_field_name field_name annot =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["json"]
    ~field:"name"
    annot

let get_json_variant_name field_name annot =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["json"]
    ~field:"name"
    annot
