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

(* Translate type names into idiomatic Scala names.  We special case
 * `string', `int', `bool' and `float` (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name str =
  match str with
    | "string" -> "String"
    | "int"    -> "Int"
    | "bool"   -> "Boolean"
    | "float"  -> "Double"
    | _ -> to_camel_case str

(* Per https://scala-lang.org/files/archive/spec/2.12/01-lexical-syntax.html *)
let scala_keywords = [
  "abstract";
  "case";
  "catch";
  "class";
  "def";
  "do";
  "else";
  "extends";
  "false";
  "final";
  "finally";
  "for";
  "forSome";
  "if";
  "implicit";
  "import";
  "lazy";
  "macro";
  "match";
  "new";
  "null";
  "object";
  "override";
  "package";
  "private";
  "protected";
  "return";
  "sealed";
  "super";
  "this";
  "throw";
  "trait";
  "try";
  "true";
  "type";
  "val";
  "var";
  "while";
  "with";
  "yield";
]

let is_scala_keyword =
  let tbl = Hashtbl.create 200 in
  List.iter (fun k -> Hashtbl.add tbl k ()) scala_keywords;
  fun k -> Hashtbl.mem tbl k

(*
   Automatically append an underscore to a field name if it is a Scala keyword.
   Use the alternative provided as <scala name ="..."> if available.

   ATD field                           Scala name

   not_a_keyword                       not_a_keyword
   class                               class_
   class <scala name="class_name">     class_name
   not_a_keyword <scala name="class">  class

*)
let get_scala_field_name field_name annot =
  let field_name =
    if is_scala_keyword field_name then
      field_name ^ "_"
    else
      field_name
  in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["scala"]
    ~field:"name"
    annot

let get_scala_variant_name field_name annot =
  let lower_field_name = String.lowercase_ascii field_name in
  let field_name =
    if is_scala_keyword lower_field_name then
      field_name ^ "_"
    else
      field_name
  in
  let field_name =
    Atd.Annot.get_field
      ~parse:(fun s -> Some s)
      ~default:field_name
      ~sections:["scala"]
      ~field:"name"
      annot
  in
  to_camel_case field_name


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

(* Splits a package name into a prefix and last component.
   Eg: "com.example.foo.bar" -> ("com.example.foo", "bar")
*)
let split_package_name p =
  let dot = String.rindex p '.' in
  let prefix = String.sub p 0 dot in
  let suffix = String.sub p (dot + 1) (String.length p - dot - 1) in
  (prefix, suffix)
