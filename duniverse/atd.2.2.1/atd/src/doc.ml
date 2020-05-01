open Import

type inline = Doc_types.inline =
  | Text of string
  | Code of string

type block = Doc_types.block =
  | Paragraph of inline list
  | Pre of string

type doc = block list

let parse_text loc s =
  try (Doc_lexer.parse_string s : block list)
  with e ->
    failwith (Printf.sprintf "%s:\nInvalid format for doc.text %S:\n%s"
                (Ast.string_of_loc loc) s (Printexc.to_string e))

let get_doc loc an : doc option =
  Annot.get_opt_field
    ~parse:(fun s -> Some (parse_text loc s))
    ~sections:["doc"]
    ~field:"text" an


(* Conversion to HTML *)

let html_escape buf s =
  String.iter (
    function
        '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c
  ) s

let print_inline buf = function
  | Text s -> html_escape buf s
  | Code s -> bprintf buf "<code>%a</code>" html_escape s

let html_of_doc blocks =
  let buf = Buffer.create 300 in
  bprintf buf "\n<div class=\"atd-doc\">\n";
  List.iter (function
    | Paragraph l ->
        Buffer.add_string buf "<p>\n";
        List.iter (print_inline buf) l;
        Buffer.add_string buf "\n</p>\n"
    | Pre s ->
        Buffer.add_string buf "<pre>\n";
        html_escape buf s;
        Buffer.add_string buf "</pre>\n"
  ) blocks;
  bprintf buf "\n</div>\n";
  Buffer.contents buf
