# odoc-parser 

Odoc-parser is a parser for odoc markup, which is an extension of the original markup
language parsed by [ocamldoc](https://ocaml.org/releases/4.12/htmlman/ocamldoc.html).

OCaml code can contain specially formatted comments that are used to document the
interfaces of modules. These comments are delimited by `(**` and `*)`. This parser
is intended to be used to parse the contents of these comments.

The parser is part of the [odoc](https://github.com/ocaml/odoc/) project.

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details of the development process.

## Example usage:

```ocaml
# #require "odoc-parser";;
# let location = {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};;
val location : Lexing.position =
  {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}
# let p = Odoc_parser.parse_comment ~location ~text:"{b Bold!}[unfinished";;
val p : Odoc_parser.t = <abstr>
# let w = Odoc_parser.warnings p;;
val w : Odoc_parser.Warning.t list =
  [{Odoc_parser.Warning.location =
     {Odoc_parser.Loc.file = "";
      start = {Odoc_parser.Loc.line = 1; column = 20};
      end_ = {Odoc_parser.Loc.line = 1; column = 20}};
    message = "End of text is not allowed in '[...]' (code)."}]
# Odoc_parser.ast p;;
- : Odoc_parser.Ast.t =
[{Odoc_parser__.Loc.location =
   {Odoc_parser__.Loc.file = "";
    start = {Odoc_parser__.Loc.line = 1; column = 0};
    end_ = {Odoc_parser__.Loc.line = 1; column = 20}};
  value =
   `Paragraph
     [{Odoc_parser__.Loc.location =
        {Odoc_parser__.Loc.file = "";
         start = {Odoc_parser__.Loc.line = 1; column = 0};
         end_ = {Odoc_parser__.Loc.line = 1; column = 9}};
       value =
        `Styled
          (`Bold,
           [{Odoc_parser__.Loc.location =
              {Odoc_parser__.Loc.file = "";
               start = {Odoc_parser__.Loc.line = 1; column = 3};
               end_ = {Odoc_parser__.Loc.line = 1; column = 8}};
             value = `Word "Bold!"}])};
      {Odoc_parser__.Loc.location =
        {Odoc_parser__.Loc.file = "";
         start = {Odoc_parser__.Loc.line = 1; column = 9};
         end_ = {Odoc_parser__.Loc.line = 1; column = 20}};
       value = `Code_span "unfinished"}]}]
```

