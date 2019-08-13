#! /usr/bin/env ocamlscript
Ocaml.packs := ["json-wheel"; "easy-format"]
--
open Json_type
open Easy_format

(* JSON does not allow rendering floats with a trailing dot: that is,
   1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
   string_of_int result in a trailing dot *)
let jstring_of_float f =
  let s = string_of_float f in
  let s_len = String.length s in
  if s.[ s_len - 1 ] = '.' then
    s ^ "0"
  else
    s

let escape_json_string buf s =
  for i = 0 to String.length s - 1 do
    let c = String.unsafe_get s i in
    match c with
      | '"'    -> Buffer.add_string buf "\\\""
      | '\t'   -> Buffer.add_string buf "\\t"
      | '\r'   -> Buffer.add_string buf "\\r"
      | '\b'   -> Buffer.add_string buf "\\b"
      | '\n'   -> Buffer.add_string buf "\\n"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\'   -> Buffer.add_string buf "\\\\"
   (* | '/'    -> "\\/" *) (* Forward slash can be escaped
                              but doesn't have to *)
      | '\x00'..'\x1F' (* Control characters that must be escaped *)
      | '\x7F' (* DEL *) ->
          Printf.bprintf buf "\\u%04X" (int_of_char c)
      | _      ->
          (* Don't bother detecting or escaping multibyte chars *)
          Buffer.add_char buf c
  done

let jstring_of_string s =
  let buf = Buffer.create (String.length s) in
  Buffer.add_char buf '"';
  escape_json_string buf s;
  Buffer.add_char buf '"';
  Buffer.contents buf



let null = { atom_style = Some "null" }
let bool = { atom_style = Some "bool" }
let int = { atom_style = Some "int" }
let float = { atom_style = Some "float" }
let string = { atom_style = Some "string" }
let label_string = { atom_style = Some "label" }
let colon = { atom_style = Some "punct" }

let array =
  { list with
      opening_style = Some "punct";
      separator_style = Some "punct";
      closing_style = Some "punct" }

let label_with_colon =
  { list with
      space_after_opening = false;
      space_before_closing = false;
      space_after_separator = false;
      wrap_body = `No_breaks }

let rec format = function
    Null -> Atom ("null", null)
  | Bool b -> Atom (string_of_bool b, bool)
  | Int i -> Atom (string_of_int i, int)
  | Float f -> Atom (jstring_of_float f, float)
  | String s -> Atom (jstring_of_string s, string)
  | Array l -> List (("[", ",", "]", array), List.map format l)
  | Object l -> List (("{", ",", "}", array), List.map format_field l)

and format_field (s, x) =
  let lab =
    List (("", "", "", label_with_colon),
	  [ Atom (jstring_of_string s, label_string);
	    Atom (":", colon) ])
  in
  Label ((lab, label), format x)



let html_escape_string s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
	'&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let html_escape = `Escape_string html_escape_string
let html_style = [
  "null", { tag_open = "<span class=\"json-null\">";
	    tag_close = "</span>" };
  "bool", { tag_open = "<span class=\"json-bool\">";
	    tag_close = "</span>" };
  "int", { tag_open = "<span class=\"json-int\">";
	   tag_close = "</span>" };
  "float", { tag_open = "<span class=\"json-float\">";
	     tag_close = "</span>" };
  "string", { tag_open = "<span class=\"json-string\">";
	      tag_close = "</span>" };
  "label", { tag_open = "<span class=\"json-label\">";
	     tag_close = "</span>" };
  "punct", { tag_open = "<span class=\"json-punct\">";
	     tag_close = "</span>" };
]



let print_html json =
  print_string "\
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
<title>JSON</title>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<meta name=\"generator\" content=\"Easy-format\" />
<style type=\"text/css\">
body,code,pre { color:black;background-color:white }
.json-null { color: #808080; }
.json-bool { color: black; }
.json-int { color: black; }
.json-float { color: black; }
.json-string { color: black; }
.json-label { color: #0033cc; }
.json-punct { color: black; }
</style>
</head>
<body>
<pre>
";
  Pretty.to_stdout ~escape: html_escape ~styles: html_style (format json);
  print_string "\
</pre>
</body>
</html>
"

let () =
  let options = [] in
  let files = ref [] in
  let anon_fun s = files := s :: !files in
  let usage_msg = Printf.sprintf "Usage: %s <file>" Sys.argv.(0) in
  Arg.parse options anon_fun usage_msg;
  let file =
    match !files with
	[s] -> s
      | _ -> Arg.usage options usage_msg; exit 1
  in
  let json = Json_io.load_json ~allow_comments:true file in
  print_html json

