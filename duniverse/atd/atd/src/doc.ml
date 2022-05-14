open Import

type inline = Doc_types.inline =
  | Text of string
  | Code of string

type block = Doc_types.block =
  | Paragraph of inline list
  | Pre of string

type doc = block list

(* Parse and print ATD's own "text" format *)
module Text = struct

  let parse loc s =
    try (Doc_lexer.parse_string s : block list)
    with e ->
      failwith (Printf.sprintf "%s:\nInvalid format for doc.text %S:\n%s"
                  (Ast.string_of_loc loc) s (Printexc.to_string e))

  (*
     Escape as little as we can get away with depending on the context:
     - always: \ -> \\
     - normal text mode: {{ -> \{\{
     - code: }} -> \}\}
     - pre: }}} -> \}\}\}
  *)

  let escape_text_re =
    Re.Pcre.regexp {|\{\{\|\\|}

  let escape_code_re =
    Re.Pcre.regexp {|\}\}|\\|}

  let escape_pre_re =
    Re.Pcre.regexp {|\}\}\}|\\|}

  let escape_text s =
    Re.Pcre.substitute ~rex:escape_text_re ~subst:(function
      | "{{" -> {|\{\{|}
      | {|\|} -> {|\\|}
      | s -> s (* bug *)
    ) s

  let escape_code s =
    Re.Pcre.substitute ~rex:escape_code_re ~subst:(function
      | "}}" -> {|\}\}|}
      | {|\|} -> {|\\|}
      | s -> s (* bug *)
    ) s

  let escape_pre s =
    Re.Pcre.substitute ~rex:escape_pre_re ~subst:(function
      | "}}}" -> {|\}\}\}|}
      | {|\|} -> {|\\|}
      | s -> s (* bug *)
    ) s

  let compact_whitespace =
    let rex = Re.Pcre.regexp "(?: \t\r\n)+" in
    fun s ->
      Re.Pcre.substitute ~rex ~subst:(fun _ -> " ") s

  (* - remove leading and trailing whitespace
     - turn inner whitespace sequences into a single space *)
  let normalize_inline s =
    s
    |> String.trim
    |> compact_whitespace

  let concat_nonempty sep xs =
    xs
    |> List.filter ((<>) "")
    |> String.concat sep

  let print_inline (x : Doc_types.inline) =
    match x with
    | Text s -> s |> normalize_inline |> escape_text
    | Code s ->
        match s |> normalize_inline |> escape_code with
        | "" -> ""
        | s ->
            let first_space =
              if s.[0] = '{' then
                " "
              else
                ""
            in
            let last_space =
              if s.[String.length s - 1] = '}' then
                " "
              else
                ""
            in
            sprintf "{{%s%s%s}}"
              first_space s last_space

  let print_block (x : Doc_types.block) =
    match x with
    | Paragraph xs ->
        xs
        |> List.map print_inline
        |> concat_nonempty " "
    | Pre s ->
        let content = escape_pre s in
        match content with
        | "" -> ""
        | s ->
            let first_newline =
              if s.[0] <> '\n' then
                "\n"
              else
                ""
            in
            let last_newline =
              if s.[String.length s - 1] <> '\n' then
                "\n"
              else
                ""
            in
            sprintf "{{{%s%s%s}}}"
              first_newline s last_newline

  let print_blocks blocks =
    blocks
    |> List.map print_block
    |> String.concat "\n\n"
end

let parse_text = Text.parse
let print_text = Text.print_blocks

(*
   This must hold all the valid annotations of the form
   '<doc ...>'.
*)
let annot_schema : Annot.schema = [
  {
    section = "doc";
    fields = [
      Module_head, "text";
      Type_def, "text";
      Variant, "text";
      Field, "text";
      (* Tolerate but deprecate?
      Type_expr, "text"; *)
    ]
  }
]

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
