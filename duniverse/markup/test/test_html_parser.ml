(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common
module Error = Markup__Error

let doctype =
  `Doctype
    {doctype_name      = Some "html";
     public_identifier = None;
     system_identifier = None;
     raw_text          = None;
     force_quirks      = false}

let start_element name =
  `Start_element ((html_ns, name), [])

let expect ?prefix ?(context = Some `Document) text signals =
  let report, iterate, ended =
    expect_signals ?prefix signal_to_string text signals in

  text
  |> Markup__Stream_io.string
  |> Markup__Encoding.utf_8
  |> Markup__Input.preprocess is_valid_html_char Error.ignore_errors
  |> Markup__Html_tokenizer.tokenize Error.ignore_errors
  |> Markup__Html_parser.parse context report
  |> iter iterate;

  ended ()

let tests = [
  ("html.parser.basic" >:: fun _ ->
    expect "<!DOCTYPE html><html><head></head><body></body></html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 22, S (start_element "head");
        1, 28, S  `End_element;
        1, 35, S (start_element "body");
        1, 55, S  `End_element;
        1, 55, S  `End_element];

    expect ~prefix:true " <!--foo--> <!DOCTYPE html>"
      [ 1,  2, S (`Comment "foo");
        1, 13, S  doctype];

    expect ~prefix:true "<!DOCTYPE html> <!--foo--> <html></html>"
      [ 1,  1, S  doctype;
        1, 17, S (`Comment "foo");
        1, 28, S (start_element "html")];

    expect ~prefix:true "<html> <!--foo--> <head></head></html>"
      [ 1,  1, S (start_element "html");
        1,  8, S (`Comment "foo");
        1, 19, S (start_element "head")]);

  ("html.parser.implicit-top-level" >:: fun _ ->
    expect "<!DOCTYPE html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 16, S  `End_element;
        1, 16, S  `End_element];

    expect "<!DOCTYPE html><html></html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 22, S (start_element "head");
        1, 22, S  `End_element;
        1, 22, S (start_element "body");
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<!DOCTYPE html><head></head>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 22, S  `End_element;
        1, 29, S (start_element "body");
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<!DOCTYPE html><body></body>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<!DOCTYPE html><p></p>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 16, S (start_element "p");
        1, 19, S  `End_element;
        1, 23, S  `End_element;
        1, 23, S  `End_element];

    expect "<!DOCTYPE html><title></title>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S (start_element "title");
        1, 23, S  `End_element;
        1, 31, S  `End_element;
        1, 31, S (start_element "body");
        1, 31, S  `End_element;
        1, 31, S  `End_element]);

  ("html.parser.no-doctype" >:: fun _ ->
    expect ~prefix:true "<title>foo</title>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S (start_element "title");
        1,  8, S (`Text ["foo"])]);

  ("html.parser.double-doctype" >:: fun _ ->
    expect ~prefix:true "<!DOCTYPE html><!DOCTYPE html><html></html>"
      [ 1,  1, S  doctype;
        1, 16, E (`Bad_document "doctype should be first");
        1, 31, S (start_element "html")]);

  ("html.parser.end-before-html" >:: fun _ ->
    expect ~prefix:true "</p><html></html>"
      [ 1,  1, E (`Unmatched_end_tag "p");
        1,  5, S (start_element "html")]);

  ("html.parser.junk-before-head" >:: fun _ ->
    expect ~prefix:true "<html><!DOCTYPE html><html></p><head></head></html>"
      [ 1,  1, S (start_element "html");
        1,  7, E (`Bad_document "doctype should be first");
        1, 22, E (`Misnested_tag ("html", "html"));
        1, 28, E (`Unmatched_end_tag "p");
        1, 32, S (start_element "head")]);

  ("html.parser.head" >:: fun _ ->
    expect ~prefix:true "<head> <!--foo--><link><link/><meta><meta/></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (`Text [" "]);
        1,  8, S (`Comment "foo");
        1, 18, S (start_element "link");
        1, 18, S  `End_element;
        1, 24, S (start_element "link");
        1, 24, S  `End_element;
        1, 31, S (start_element "meta");
        1, 31, S  `End_element;
        1, 37, S (start_element "meta");
        1, 37, S  `End_element;
        1, 44, S  `End_element;
        1, 51, S (start_element "body")]);

  ("html.parser.style" >:: fun _ ->
    expect ~prefix:true "<head><style>foo</head>&lt;</style></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "style");
        1, 14, S (`Text ["foo</head>&lt;"]);
        1, 28, S  `End_element;
        1, 36, S  `End_element;
        1, 43, S (start_element "body")]);

  ("html.parser.title" >:: fun _ ->
    expect ~prefix:true "<head><title>foo</head>&lt;</title></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "title");
        1, 14, S (`Text ["foo</head><"]);
        1, 28, S  `End_element;
        1, 36, S  `End_element;
        1, 43, S (start_element "body")]);

  ("html.parser.script" >:: fun _ ->
    expect ~prefix:true "<head><script><!--foo</head>&lt;bar</script></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "script");
        1, 15, S (`Text ["<!--foo</head>&lt;bar"]);
        1, 36, S  `End_element;
        1, 45, S  `End_element;
        1, 52, S (start_element "body")]);

  ("html.parser.junk-in-head" >:: fun _ ->
    expect ~prefix:true "<head><!DOCTYPE html><html><head></p></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, E (`Bad_document "doctype should be first");
        1, 22, E (`Misnested_tag ("html", "head"));
        1, 28, E (`Misnested_tag ("head", "head"));
        1, 34, E (`Unmatched_end_tag "p");
        1, 38, S  `End_element;
        1, 45, S (start_element "body")]);

  ("html.parser.junk-after-head" >:: fun _ ->
    expect ~prefix:true
      "<head></head> <!--foo--><!DOCTYPE html><html><meta><head></p><body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S  `End_element;
        1, 14, S (`Text [" "]);
        1, 15, S (`Comment "foo");
        1, 25, E (`Bad_document "doctype should be first");
        1, 40, E (`Misnested_tag ("html", "html"));
        1, 46, E (`Misnested_tag ("meta", "html"));
        1, 46, S (start_element "meta");
        1, 46, S  `End_element;
        1, 52, E (`Bad_document "duplicate head element");
        1, 58, E (`Unmatched_end_tag "p");
        1, 62, S (start_element "body")]);

  ("html.parser.whitespace-after-head" >:: fun _ ->
    expect "<html><head></head> </html>"
      [ 1,  1, S (start_element "html");
        1,  7, S (start_element "head");
        1, 13, S  `End_element;
        1, 20, S (`Text [" "]);
        1, 21, S (start_element "body");
        1, 28, S  `End_element;
        1, 28, S  `End_element];

    expect "<html><head><title>foo</title></head> </html>"
      [ 1,  1, S (start_element "html");
        1,  7, S (start_element "head");
        1, 13, S (start_element "title");
        1, 20, S (`Text ["foo"]);
        1, 23, S  `End_element;
        1, 31, S  `End_element;
        1, 38, S (`Text [" "]);
        1, 39, S (start_element "body");
        1, 46, S  `End_element;
        1, 46, S  `End_element]);

  ("html.parser.body-content" >:: fun _ ->
    expect "<body><!--foo--> bar</body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S (`Comment "foo");
        1, 17, S (`Text [" bar"]);
        1, 28, S  `End_element;
        1, 28, S  `End_element]);

  ("html.parser.body.whitespace" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) " \n\r\t\x0c&#x0d;"
      [ 1,  1, S (`Text [" \n\n\t\x0c\r"])]);

  ("html.parser.paragraphs" >:: fun _ ->
    expect "<p>foo</p>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element;
        1, 11, S  `End_element;
        1, 11, S  `End_element];

    expect "<p>foo<p>bar<div>baz</div>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element;
        1,  7, S (start_element "p");
        1, 10, S (`Text ["bar"]);
        1, 13, S  `End_element;
        1, 13, S (start_element "div");
        1, 18, S (`Text ["baz"]);
        1, 21, S  `End_element;
        1, 27, S  `End_element;
        1, 27, S  `End_element]);

  ("html.parser.links" >:: fun _ ->
    expect
      {|<a href="foo.com?bar=on&acte=123">foo</a>|}
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1, 1, S (`Start_element ((html_ns, "a"), [(("", "href"), "foo.com?bar=on&acte=123")]));
        1,  35, S (`Text ["foo"]);
        1,  38, S  `End_element;
        1, 42, S  `End_element;
        1, 42, S  `End_element];

    expect
      {|<a href="foo.com?bar=on&image=on">foo</a>|}
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1, 1, S (`Start_element ((html_ns, "a"), [(("", "href"), "foo.com?bar=on&image=on")]));
        1,  35, S (`Text ["foo"]);
        1,  38, S  `End_element;
        1, 42, S  `End_element;
        1, 42, S  `End_element];

    expect
      {|<a href="foo.com?bar=on&image;">foo</a>|}
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1, 1, S (`Start_element ((html_ns, "a"), [(("", "href"), "foo.com?bar=onℑ")]));
        1,  33, S (`Text ["foo"]);
        1,  36, S  `End_element;
        1, 40, S  `End_element;
        1, 40, S  `End_element]);

  ("html.parser.headings" >:: fun _ ->
    expect "<p><h1><h2>foo</h2>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "h1");
        1,  8, E (`Misnested_tag ("h2", "h1"));
        1,  8, S  `End_element;
        1,  8, S (start_element "h2");
        1, 12, S (`Text ["foo"]);
        1, 15, S  `End_element;
        1, 20, S  `End_element;
        1, 20, S  `End_element]);

  ("html.parser.pre" >:: fun _ ->
    expect "<p><pre>foo</pre>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "pre");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element;
        1, 18, S  `End_element;
        1, 18, S  `End_element];

    expect "<p><pre>\n\nfoo</pre>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "pre");
        2,  1, S (`Text ["\nfoo"]);
        3,  4, S  `End_element;
        3, 10, S  `End_element;
        3, 10, S  `End_element]);

  ("html.parser.textarea" >:: fun _ ->
    expect "<textarea>foo</p></textarea>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "textarea");
        1, 11, S (`Text ["foo</p>"]);
        1, 18, S  `End_element;
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<textarea>\n\nfoo</p></textarea>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "textarea");
        2,  1, S (`Text ["\nfoo</p>"]);
        3,  8, S  `End_element;
        3, 19, S  `End_element;
        3, 19, S  `End_element];

    expect ~context:(Some (`Fragment "body")) "<textarea></textarea><p>foo</p>"
      [ 1,  1, S (start_element "textarea");
        1, 11, S  `End_element;
        1, 22, S (start_element "p");
        1, 25, S (`Text ["foo"]);
        1, 28, S  `End_element]);

  ("html.parser.list" >:: fun _ ->
    expect "<ul><li>foo<li>bar</ul>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "ul");
        1,  5, S (start_element "li");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element;
        1, 12, S (start_element "li");
        1, 16, S (`Text ["bar"]);
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 24, S  `End_element;
        1, 24, S  `End_element]);

  ("html.parser.definition" >:: fun _ ->
    expect "<p><dt>foo<dd>bar"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "dt");
        1,  8, S (`Text ["foo"]);
        1, 11, S  `End_element;
        1, 11, S (start_element "dd");
        1, 15, S (`Text ["bar"]);
        1, 18, S  `End_element;
        1, 18, S  `End_element;
        1, 18, S  `End_element]);

  ("html.parser.plaintext" >:: fun _ ->
    expect "<p><plaintext>foo</plaintext></p>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "plaintext");
        1,  4, E (`Unmatched_start_tag "plaintext");
        1, 15, S (`Text ["foo</plaintext></p>"]);
        1, 34, S  `End_element;
        1, 34, S  `End_element;
        1, 34, S  `End_element]);

  ("html.parser.table" >:: fun _ ->
    expect "<p><table><tr><td>foo</td><td>bar</td></tr></table>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "table");
        1, 11, S (start_element "tbody");
        1, 11, S (start_element "tr");
        1, 15, S (start_element "td");
        1, 19, S (`Text ["foo"]);
        1, 22, S  `End_element;
        1, 27, S (start_element "td");
        1, 31, S (`Text ["bar"]);
        1, 34, S  `End_element;
        1, 39, S  `End_element;
        1, 44, S  `End_element;
        1, 44, S  `End_element;
        1, 52, S  `End_element;
        1, 52, S  `End_element]);

  ("html.parser.select" >:: fun _ ->
    expect "<select><option>foo<option>bar</select>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "select");
        1,  9, S (start_element "option");
        1, 17, S (`Text ["foo"]);
        1, 20, S  `End_element;
        1, 20, S (start_element "option");
        1, 28, S (`Text ["bar"]);
        1, 31, S  `End_element;
        1, 31, S  `End_element;
        1, 40, S  `End_element;
        1, 40, S  `End_element]);

  ("html.parser.datalist" >:: fun _ ->
    expect "<datalist><option><option></datalist>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "datalist");
        1, 11, S (start_element "option");
        1, 19, S  `End_element;
        1, 19, S (start_element "option");
        1, 27, S  `End_element;
        1, 27, S  `End_element;
        1, 38, S  `End_element;
        1, 38, S  `End_element]);

  ("html.parser.datalist.whitespace" >:: fun _ ->
    expect "<datalist>\n<option>\n<option>\n</datalist>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "datalist");
        1, 11, S (`Text ["\n"]);
        2,  1, S (start_element "option");
        2,  9, S (`Text ["\n"]);
        3,  1, S  `End_element;
        3,  1, S (start_element "option");
        3,  9, S (`Text ["\n"]);
        4,  1, S  `End_element;
        4,  1, S  `End_element;
        4, 12, S  `End_element;
        4, 12, S  `End_element]);

  ("html.parser.truncated-body" >:: fun _ ->
    expect "<body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S  `End_element;
        1,  7, S  `End_element];

    expect "<body></html>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1, 14, S  `End_element;
        1, 14, S  `End_element]);

  ("html.parser.junk-in-body" >:: fun _ ->
    expect "<body>\x00<!DOCTYPE html><html><meta><body></body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, E (`Bad_token ("U+0000", "body", "null"));
        1,  8, E (`Bad_document "doctype should be first");
        1, 23, E (`Misnested_tag ("html", "body"));
        1, 29, S (start_element "meta");
        1, 29, S  `End_element;
        1, 35, E (`Misnested_tag ("body", "body"));
        1, 48, S  `End_element;
        1, 48, S  `End_element]);

  ("html.parser.nested-html-in-body" >:: fun _ ->
    expect "<div><html></html>foo</div><div>bar</div>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "div");
        1,  6, E (`Misnested_tag ("html", "body"));
        1,  1, E (`Unmatched_start_tag "div");
        1, 19, E (`Bad_content "html");
        1, 19, S (`Text ["foo"]);
        1, 22, S  `End_element;
        1, 28, S (start_element "div");
        1, 33, S (`Text ["bar"]);
        1, 36, S  `End_element;
        1, 42, S  `End_element;
        1, 42, S  `End_element]);

  ("html.parser.nested-html-with-body-in-body" >:: fun _ ->
    expect "<p><html><body><p></body><br><p>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, E (`Misnested_tag ("html", "body"));
        1, 10, E (`Misnested_tag ("body", "body"));
        1, 16, S  `End_element;
        1, 16, S (start_element "p");
        1, 26, E (`Bad_document ("content after body"));
        1, 26, S (start_element "br");
        1, 26, S  `End_element;
        1, 30, S  `End_element;
        1, 30, S (start_element "p");
        1, 33, S  `End_element;
        1, 33, S  `End_element;
        1, 33, S  `End_element]
  );

  ("html.parser.whitespace-at-end" >:: fun _ ->
    expect "<html><body></body></html> "
      [ 1,  1, S (start_element "html");
        1,  7, S (start_element "head");
        1,  7, S  `End_element;
        1,  7, S (start_element "body");
        1, 27, S (`Text [" "]);
        1, 28, S  `End_element;
        1, 28, S  `End_element]);

  ("html.parser.foreign" >:: fun _ ->
    expect "<body><svg><g/></svg></body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S (`Start_element ((svg_ns, "svg"), []));
        1, 12, S (`Start_element ((svg_ns, "g"), []));
        1, 12, S  `End_element;
        1, 16, S  `End_element;
        1, 29, S  `End_element;
        1, 29, S  `End_element]);

  ("html.parser.foreign.svg-followed-by-html" >:: fun _ ->
    expect ~context:(Some (`Fragment "body"))
      "<svg><feTile></feTile></svg><b></b>"
      [ 1,  1, S (`Start_element ((svg_ns, "svg"), []));
        1,  6, S (`Start_element ((svg_ns, "feTile"), []));
        1, 14, S  `End_element;
        1, 23, S  `End_element;
        1, 29, S (start_element "b");
        1, 32, S  `End_element]);

  ("html.parser.reconstruct-active-formatting-elements" >:: fun _ ->
    expect "<p><em><strong>foo<p>bar"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  8, E (`Unmatched_start_tag "strong");
        1,  4, S (start_element "em");
        1,  8, S (start_element "strong");
        1, 16, S (`Text ["foo"]);
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 19, S (start_element "p");
        1,  8, E (`Unmatched_start_tag "strong");
        1,  4, E (`Unmatched_start_tag "em");
        1,  4, S (start_element "em");
        1,  8, S (start_element "strong");
        1, 22, S (`Text ["bar"]);
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element]);

  ("html.parser.close-formatting-elements" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<a>fo</a>o"
      [ 1,  1, S (start_element "a");
        1,  4, S (`Text ["fo"]);
        1,  6, S  `End_element;
        1, 10, S (`Text ["o"])]);

  ("html.parser.reset-mode" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<table></table><table></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S  `End_element;
        1, 16, S (start_element "table");
        1, 23, S  `End_element]);

  ("html.parser.fragment" >:: fun _ ->
    expect ~context:(Some (`Fragment "title")) "</p>"
      [ 1,  1, S (`Text ["</p>"])];

    expect ~context:(Some (`Fragment "textarea")) "</p>"
      [ 1,  1, S (`Text ["</p>"])];

    expect ~context:(Some (`Fragment "body")) "</p>"
      [ 1,  1, E (`Unmatched_end_tag "p");
        1,  1, S (start_element "p");
        1,  1, S  `End_element];

    expect ~context:(Some (`Fragment "body")) "<!DOCTYPE html>"
      [ 1,  1, E (`Bad_document "doctype should be first")]);

  ("html.parser.fragment.rawtext" >:: fun _ ->
    expect ~context:(Some (`Fragment "style")) "&nbsp;</p>"
      [ 1,  1, S (`Text ["&nbsp;</p>"])]);

  ("html.parser.fragment.script" >:: fun _ ->
    expect ~context:(Some (`Fragment "script")) "&nbsp;</p>"
      [ 1,  1, S (`Text ["&nbsp;</p>"])]);

  ("html.parser.fragment.plaintext" >:: fun _ ->
    expect ~context:(Some (`Fragment "plaintext")) "&nbsp;</p></plaintext>"
      [ 1,  1, S (`Text ["&nbsp;</p></plaintext>"])]);

  ("html.parser.context-detection" >:: fun _ ->
    expect ~context:None "<p>foo</p>"
      [ 1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element];

    expect ~context:None "<html></html>"
      [ 1,  1, S (start_element "html");
        1,  7, S (start_element "head");
        1,  7, S  `End_element;
        1,  7, S (start_element "body");
        1, 14, S  `End_element;
        1, 14, S  `End_element]);

  ("html.parser.foreign-context" >:: fun _ ->
    expect ~context:None "<g/>"
      [ 1,  1, S (`Start_element ((svg_ns, "g"), []));
        1,  1, S  `End_element]);

  ("html.parser.context-disambiguation" >:: fun _ ->
    expect ~context:(Some (`Fragment "svg")) "<a></a>"
      [ 1,  1, S (`Start_element ((svg_ns, "a"), []));
        1,  4, S  `End_element]);

  ("html.parser.context-case-insensitivity" >:: fun _ ->
    expect ~context:(Some (`Fragment "SVG")) "<a></a>"
      [ 1,  1, S (`Start_element ((svg_ns, "a"), []));
        1,  4, S  `End_element]);

  ("html.parser.bad-self-closing-tag" >:: fun _ ->
    expect "<p/>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, E (`Bad_token ("/>", "tag", "should not be self-closing"));
        1,  1, S (start_element "p");
        1,  5, S  `End_element;
        1,  5, S  `End_element;
        1,  5, S  `End_element]);

  ("html.parser.image-tag" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<image/>"
      [ 1,  1, E (`Bad_token ("image", "tag", "should be 'img'"));
        1,  1, S (start_element "img");
        1,  1, S  `End_element]);

  ("html.parser.nulls" >:: fun _ ->
    expect ~context:(Some (`Fragment "svg")) "\x00foo"
      [ 1,  1, E (`Bad_token ("U+0000", "foreign content", "null"));
        1,  1, S (`Text ["\xef\xbf\xbdfoo"])];

    expect ~context:(Some (`Fragment "body")) "<table>\x00foo</table>"
      [ 1,  1, S (start_element "table");
        1,  8, E (`Bad_token ("U+0000", "table", "null"));
        1,  9, E (`Bad_content "table");
        1, 10, E (`Bad_content "table");
        1, 11, E (`Bad_content "table");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element];

    expect ~context:(Some (`Fragment "select")) "\x00foo"
      [ 1,  1, E (`Bad_token ("U+0000", "select", "null"));
        1,  2, S (`Text ["foo"])]);

  ("html.parser.foreign.cdata" >:: fun _ ->
    expect ~context:None "<svg><![CDATA[foo]]></svg>"
      [ 1,  1, S (`Start_element ((svg_ns, "svg"), []));
        1, 15, S (`Text ["foo"]);
        1, 21, S  `End_element]);

  ("html.parser.large-text" >:: fun _ ->
    with_text_limit 8 begin fun () ->
      expect ~context:None "foobar" [ 1,  1, S (`Text ["foobar"])];
      expect ~context:None "foobarbaz" [ 1,  1, S (`Text ["foobarba"; "z"])]
    end);

  ("html.parser.adoption-agency.simple" >:: fun _ ->
    expect ~context:None "foo<b>bar</b>baz"
      [ 1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"]);
        1, 10, S  `End_element;
        1, 14, S (`Text ["baz"])]);

  ("html.parser.adoption-agency.stray" >:: fun _ ->
    expect ~context:None "foo</b>bar"
      [ 1,  4, E (`Unmatched_end_tag "b");
        1,  1, S (`Text ["foo"; "bar"])]);

  ("html.parser.adoption-agency.nested" >:: fun _ ->
    expect ~context:None "foo<b>bar<em>baz</em>quux</b>lulz"
      [ 1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"]);
        1, 10, S (start_element "em");
        1, 14, S (`Text ["baz"]);
        1, 17, S  `End_element;
        1, 22, S (`Text ["quux"]);
        1, 26, S  `End_element;
        1, 30, S (`Text ["lulz"])]);

  ("html.parser.adoption-agency.nested.stray" >:: fun _ ->
    expect ~context:None "foo<b>bar</em>baz</b>quux"
      [ 1, 10, E (`Unmatched_end_tag "em");
        1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"; "baz"]);
        1, 18, S  `End_element;
        1, 22, S (`Text ["quux"])]);

  ("html.parser.adoption-agency.interleaved" >:: fun _ ->
    expect ~context:None "foo<b>bar<em>baz</b>quux</em>"
      [ 1, 17, E (`Unmatched_end_tag "b");
        1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"]);
        1, 10, S (start_element "em");
        1, 14, S (`Text ["baz"]);
        1, 17, S  `End_element;
        1, 17, S  `End_element;
        1, 10, S (start_element "em");
        1, 21, S (`Text ["quux"]);
        1, 25, S  `End_element]);

  ("html.parser.adoption-agency.block" >:: fun _ ->
    expect ~context:None "foo<b>bar<p>baz</b>quux"
      [ 1, 16, E (`Unmatched_end_tag "b");
        1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"]);
        1, 16, S  `End_element;
        1, 10, S (start_element "p");
        1,  4, S (start_element "b");
        1, 13, S (`Text ["baz"]);
        1, 16, S  `End_element;
        1, 20, S (`Text ["quux"]);
        1, 24, S  `End_element]);

  ("html.parser.adoption-agency.block.nested" >:: fun _ ->
    expect ~context:None "foo<b>bar<em>baz<strong>quux<p>blah</b>lulz"
      [ 1, 36, E (`Unmatched_end_tag "b");
        1, 17, E (`Unmatched_start_tag "strong");
        1, 10, E (`Unmatched_start_tag "em");
        1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "b");
        1,  7, S (`Text ["bar"]);
        1, 10, S (start_element "em");
        1, 14, S (`Text ["baz"]);
        1, 17, S (start_element "strong");
        1, 25, S (`Text ["quux"]);
        1, 36, S  `End_element;
        1, 36, S  `End_element;
        1, 36, S  `End_element;
        1, 10, S (start_element "em");
        1, 17, S (start_element "strong");
        1, 29, S (start_element "p");
        1,  4, S (start_element "b");
        1, 32, S (`Text ["blah"]);
        1, 36, S  `End_element;
        1, 40, S (`Text ["lulz"]);
        1, 44, S  `End_element;
        1, 44, S  `End_element;
        1, 44, S  `End_element]);

  ("html.parser.adoption-agency.reconstructed" >:: fun _ ->
    expect ~context:None "<p><b>foo<p>bar<em>baz</b>quux"
      [ 1,  1, S (start_element "p");
        1,  4, E (`Unmatched_start_tag "b");
        1,  4, S (start_element "b");
        1,  7, S (`Text ["foo"]);
        1, 10, S  `End_element;
        1, 10, S  `End_element;
        1, 10, S (start_element "p");
        1, 23, E (`Unmatched_end_tag "b");
        1, 16, E (`Unmatched_start_tag "em");
        1,  4, S (start_element "b");
        1, 13, S (`Text ["bar"]);
        1, 16, S (start_element "em");
        1, 20, S (`Text ["baz"]);
        1, 23, S  `End_element;
        1, 23, S  `End_element;
        1, 16, S (start_element "em");
        1, 27, S (`Text ["quux"]);
        1, 31, S  `End_element;
        1, 31, S  `End_element]);

  ("html.parser.noscript" >:: fun _ ->
    expect ~context:None "<head><noscript><meta></noscript></head>"
      [ 1,  1, S (start_element "head");
        1,  7, S (start_element "noscript");
        1, 17, S (start_element "meta");
        1, 17, S  `End_element;
        1, 23, S  `End_element;
        1, 34, S  `End_element]);

  ("html.parser.noscript.bad" >:: fun _ ->
    expect ~context:None
      "<head><noscript><!DOCTYPE html><html><head><noscript></head></noscript>"
      [ 1,  1, S (start_element "head");
        1,  7, S (start_element "noscript");
        1, 17, E (`Bad_document "doctype should be first");
        1, 32, E (`Misnested_tag ("html", "noscript"));
        1, 38, E (`Misnested_tag ("head", "noscript"));
        1, 44, E (`Misnested_tag ("noscript", "noscript"));
        1, 54, E (`Unmatched_end_tag "head");
        1, 61, S  `End_element;
        1, 72, S  `End_element]);

  ("html.parser.noscript.content" >:: fun _ ->
    expect ~context:None "<noscript> \t\n<!--foo--> foo</noscript>"
      [ 1,  1, S (start_element "noscript");
        1, 11, S (`Text [" \t\n"]);
        2,  1, S (`Comment "foo");
        2, 12, E (`Bad_content "noscript");
        2, 11, S (`Text [" "]);
        2, 12, S  `End_element;
        2, 12, S (start_element "body");
        2, 15, E (`Unmatched_end_tag "noscript");
        2, 12, S (`Text ["foo"]);
        2, 26, S  `End_element]);

  ("html.parser.head.fragment" >:: fun _ ->
    expect ~context:None "<base>"
      [ 1,  1, S (start_element "base");
        1,  1, S  `End_element];

    expect ~context:None "<basefont>"
      [ 1,  1, S (start_element "basefont");
        1,  1, S  `End_element];

    expect ~context:None "<bgsound>"
      [ 1,  1, S (start_element "bgsound");
        1,  1, S  `End_element];

    expect ~context:None "<link>"
      [ 1,  1, S (start_element "link");
        1,  1, S  `End_element];

    expect ~context:None "<meta>"
      [ 1,  1, S (start_element "meta");
        1,  1, S  `End_element];

    expect ~context:None "<noframes></noframes>"
      [ 1,  1, S (start_element "noframes");
        1, 11, S  `End_element];

    expect ~context:None "<style></style>"
      [ 1,  1, S (start_element "style");
        1,  8, S  `End_element]);

  ("html.parser.body.fragment" >:: fun _ ->
    expect ~context:None "<body></body>"
      [ 1,  1, S (start_element "body");
        1, 14, S  `End_element]);

  ("html.parser.body.content-truncated" >:: fun _ ->
    expect ~context:None "<p></body></html>foo"
      [ 1,  1, S (start_element "p");
        1,  4, E (`Unmatched_end_tag "body");
        1, 11, E (`Unmatched_end_tag "html");
        1, 18, S (`Text ["foo"]);
        1, 21, S  `End_element]);

  ("html.parser.nested-button" >:: fun _ ->
    expect ~context:None "<button><button>submit</button></button>"
      [ 1,  1, S (start_element "button");
        1,  9, E (`Misnested_tag ("button", "button"));
        1,  9, S  `End_element;
        1,  9, S (start_element "button");
        1, 17, S (`Text ["submit"]);
        1, 23, S  `End_element;
        1, 32, E (`Unmatched_end_tag "button")]);

  ("html.parser.nested-list" >:: fun _ ->
    expect ~context:None "<ul><li><ul></li></ul></li></ul>"
      [ 1,  1, S (start_element "ul");
        1,  5, S (start_element "li");
        1,  9, S (start_element "ul");
        1, 13, E (`Unmatched_end_tag "li");
        1, 18, S  `End_element;
        1, 23, S  `End_element;
        1, 28, S  `End_element]);

  ("html.parser.definitions" >:: fun _ ->
    expect ~context:None "</dd><dd></dd>"
      [ 1,  1, E (`Unmatched_end_tag "dd");
        1,  6, S (start_element "dd");
        1, 10, S  `End_element]);

  ("html.parser.nested-achor" >:: fun _ ->
    expect ~context:None "<a><a></a></a>"
      [ 1,  4, E (`Misnested_tag ("a", "a"));
        1, 11, E (`Unmatched_end_tag "a");
        1,  1, S (start_element "a");
        1,  4, S  `End_element;
        1,  4, S (start_element "a");
        1,  7, S  `End_element]);

  ("html.parser.nested-anchor.reconstruct" >:: fun _ ->
    expect ~context:None "<p><a>foo<a>bar<p>baz"
      [ 1,  1, S (start_element "p");
        1, 10, E (`Misnested_tag ("a", "a"));
        1, 10, E (`Unmatched_start_tag "a");
        1,  4, S (start_element "a");
        1,  7, S (`Text ["foo"]);
        1, 10, S  `End_element;
        1, 10, S (start_element "a");
        1, 13, S (`Text ["bar"]);
        1, 16, S  `End_element;
        1, 16, S  `End_element;
        1, 16, S (start_element "p");
        1, 10, E (`Unmatched_start_tag "a");
        1, 10, S (start_element "a");
        1, 19, S (`Text ["baz"]);
        1, 22, S  `End_element;
        1, 22, S  `End_element]);

  ("html.parser.nested-nobr" >:: fun _ ->
    expect ~context:None "foo<nobr>bar<nobr>baz</nobr>quux</nobr>blah"
      [ 1, 13, E (`Misnested_tag ("nobr", "nobr"));
        1, 33, E (`Unmatched_end_tag "nobr");
        1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "nobr");
        1, 10, S (`Text ["bar"]);
        1, 13, S  `End_element;
        1, 13, S (start_element "nobr");
        1, 19, S (`Text ["baz"]);
        1, 22, S  `End_element;
        1, 29, S (`Text ["quux"; "blah"])]);

  ("html.parser.end-br" >:: fun _ ->
    expect ~context:None "<br></br>"
      [ 1,  1, S (start_element "br");
        1,  1, S  `End_element;
        1,  5, E (`Unmatched_end_tag "br");
        1,  5, S (start_element "br");
        1,  5, S  `End_element]);

  ("html.parser.hr" >:: fun _ ->
    expect ~context:None "<p><hr>"
      [ 1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "hr");
        1,  4, S  `End_element]);

  ("html.parser.input" >:: fun _ ->
    expect ~context:None "<input type='text'>"
      [ 1,  1, S (`Start_element ((html_ns, "input"), [("", "type"), "text"]));
        1,  1, S  `End_element]);

  ("html.parser.iframe" >:: fun _ ->
    expect ~context:None "<iframe><p>foo&amp;</p></iframe>"
      [ 1,  1, S (start_element "iframe");
        1,  9, S (`Text ["<p>foo&amp;</p>"]);
        1, 24, S  `End_element]);

  ("html.parser.noembed" >:: fun _ ->
    expect ~context:None "<noembed><p>foo&amp;</p></noembed>"
      [ 1,  1, S (start_element "noembed");
        1, 10, S (`Text ["<p>foo&amp;</p>"]);
        1, 25, S  `End_element]);

  ("html.parser.generic-tag" >:: fun _ ->
    expect ~context:None "<foo></foo>"
      [ 1,  1, S (start_element "foo");
        1,  6, S  `End_element]);

  ("html.parser.option.body" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<option><optgroup></optgroup>"
      [ 1,  1, S (start_element "option");
        1,  9, S  `End_element;
        1,  9, S (start_element "optgroup");
        1, 19, S  `End_element]);

  ("html.parser.table-content-in-body" >:: fun _ ->
    expect ~context:(Some (`Fragment "body"))
      "<caption><col><colgroup><tbody><td><tfoot><th><thead><tr>"
      [ 1,  1, E (`Misnested_tag ("caption", "body"));
        1, 10, E (`Misnested_tag ("col", "body"));
        1, 15, E (`Misnested_tag ("colgroup", "body"));
        1, 25, E (`Misnested_tag ("tbody", "body"));
        1, 32, E (`Misnested_tag ("td", "body"));
        1, 36, E (`Misnested_tag ("tfoot", "body"));
        1, 43, E (`Misnested_tag ("th", "body"));
        1, 47, E (`Misnested_tag ("thead", "body"));
        1, 54, E (`Misnested_tag ("tr", "body"))]);

  ("html.parser.caption" >:: fun _ ->
    expect ~context:None "<table><caption>foo<p>bar</caption></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "caption");
        1, 17, S (`Text ["foo"]);
        1, 20, S (start_element "p");
        1, 23, S (`Text ["bar"]);
        1, 26, S  `End_element;
        1, 26, S  `End_element;
        1, 36, S  `End_element]);

  ("html.parser.colgroup" >:: fun _ ->
    expect ~context:None "<table><colgroup><col></colgroup></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "colgroup");
        1, 18, S (start_element "col");
        1, 18, S  `End_element;
        1, 23, S  `End_element;
        1, 34, S  `End_element]);

  ("html.parser.colgroup.implicit" >:: fun _ ->
    expect ~context:None "<table><col></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "colgroup");
        1,  8, S (start_element "col");
        1,  8, S  `End_element;
        1, 13, S  `End_element;
        1, 13, S  `End_element]);

  ("html.parser.td.direct" >:: fun _ ->
    expect ~context:None "<table><td></td></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "tbody");
        1,  8, E (`Misnested_tag ("td", "table"));
        1,  8, S (start_element "tr");
        1,  8, S (start_element "td");
        1, 12, S  `End_element;
        1, 17, S  `End_element;
        1, 17, S  `End_element;
        1, 17, S  `End_element]);

  ("html.parser.tbody" >:: fun _ ->
    expect ~context:None "<table><tbody></tbody></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "tbody");
        1, 15, S  `End_element;
        1, 23, S  `End_element]);

  ("html.parser.nested-table" >:: fun _ ->
    expect ~context:None "<table><table></table>"
      [ 1,  1, S (start_element "table");
        1,  8, E (`Misnested_tag ("table", "table"));
        1,  8, S  `End_element;
        1,  8, S (start_element "table");
        1, 15, S  `End_element]);

  ("html.parser.nested-caption" >:: fun _ ->
    expect ~context:None "<table><caption><caption></caption></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "caption");
        1, 17, E (`Misnested_tag ("caption", "caption"));
        1, 17, S  `End_element;
        1, 17, S (start_element "caption");
        1, 26, S  `End_element;
        1, 36, S  `End_element]);

  ("html.parser.truncated-caption" >:: fun _ ->
    expect ~context:None "<table><caption></table>"
      [ 1,  1, S (start_element "table");
        1,  8, S (start_element "caption");
        1, 17, E (`Unmatched_end_tag "table");
        1, 17, S  `End_element;
        1, 17, S  `End_element]);

  ("html.parser.nested-tbody" >:: fun _ ->
    expect ~context:None "<tbody><tbody></tbody>"
      [ 1,  1, S (start_element "tbody");
        1,  8, S  `End_element;
        1,  8, S (start_element "tbody");
        1, 15, S  `End_element]);

  ("html.parser.option" >:: fun _ ->
    expect ~context:None "<option></option>"
      [ 1,  1, S (start_element "option");
        1,  9, S  `End_element]);

  ("html.parser.optgroup" >:: fun _ ->
    expect ~context:None
      "<select><optgroup><option><optgroup><option></optgroup></select>"
      [ 1,  1, S (start_element "select");
        1,  9, S (start_element "optgroup");
        1, 19, S (start_element "option");
        1, 27, S  `End_element;
        1, 27, S  `End_element;
        1, 27, S (start_element "optgroup");
        1, 37, S (start_element "option");
        1, 45, S  `End_element;
        1, 45, S  `End_element;
        1, 56, S  `End_element]);

  ("html.parser.form" >:: fun _ ->
    expect ~context:None "<form></form>"
      [ 1,  1, S (start_element "form");
        1,  7, S  `End_element]);

  ("html.parser.form.nested" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<form><form></form>"
      [ 1,  1, S (start_element "form");
        1,  7, E (`Misnested_tag ("form", "form"));
        1, 13, S  `End_element]
  );

  ("html.parser.form.unopened" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "</form>"
      [ 1,  1, E (`Unmatched_end_tag "form")]);

  ("html.parser.noframes" >:: fun _ ->
    expect ~context:None "<noframes>foo&amp;bar</a></noframes>"
      [ 1,  1, S (start_element "noframes");
        1, 11, S (`Text ["foo&amp;bar</a>"]);
        1, 26, S  `End_element]);

  ("html.parser.frameset" >:: fun _ ->
    expect "<frameset><frame></frameset>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "frameset");
        1, 11, S (start_element "frame");
        1, 11, S  `End_element;
        1, 18, S  `End_element;
        1, 29, S  `End_element]);

  ("html.parser.frameset.fragment" >:: fun _ ->
    expect ~context:None "<frameset></frameset>"
      [ 1,  1, S (start_element "frameset");
        1, 11, S  `End_element];

    expect ~context:None "<frame>"
      [ 1,  1, S (start_element "frame");
        1,  1, S  `End_element]);

  ("html.parser.frameset.content" >:: fun _ ->
    expect ~context:None
      ("<frameset> \t\n<!--foo--><noframes></noframes><frameset></frameset>" ^
       "</frameset>")
      [ 1,  1, S (start_element "frameset");
        1, 11, S (`Text [" \t\n"]);
        2,  1, S (`Comment "foo");
        2, 11, S (start_element "noframes");
        2, 21, S  `End_element;
        2, 32, S (start_element "frameset");
        2, 42, S  `End_element;
        2, 53, S  `End_element]);

  ("html.parser.frameset.bad" >:: fun _ ->
    expect ~context:None "<frameset><!DOCTYPE html><html>f"
      [ 1,  1, S (start_element "frameset");
        1, 11, E (`Bad_document "doctype should be first");
        1, 26, E (`Misnested_tag ("html", "frameset"));
        1, 32, E (`Bad_content "frameset");
        1, 33, E (`Unexpected_eoi "frameset");
        1, 33, S  `End_element]);

  ("html.parser.after-frameset.content" >:: fun _ ->
    expect ~context:None
      "<frameset></frameset> \t\n<!--foo--><noframes></noframes></html>"
      [ 1,  1, S (start_element "frameset");
        1, 11, S  `End_element;
        1, 22, S (`Text [" \t\n"]);
        2,  1, S (`Comment "foo");
        2, 11, S (start_element "noframes");
        2, 21, S  `End_element]);

  ("html.parser.after-frameset.bad" >:: fun _ ->
    expect ~context:None "<frameset></frameset><!DOCTYPE html><html>f"
      [ 1,  1, S (start_element "frameset");
        1, 11, S  `End_element;
        1, 22, E (`Bad_document "doctype should be first");
        1, 37, E (`Misnested_tag ("html", "html"));
        1, 43, E (`Bad_content "html")]);

  ("html.parser.frameset-in-body" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<frameset><p>"
      [ 1,  1, E (`Misnested_tag ("frameset", "body"));
        1, 11, S (start_element "p");
        1, 14, S  `End_element];

    expect ~context:None "<body><p><frameset><p></body>"
      [ 1,  1, S (start_element "body");
        1,  7, S (start_element "p");
        1, 10, E (`Misnested_tag ("frameset", "body"));
        1, 20, S  `End_element;
        1, 20, S (start_element "p");
        1, 30, S  `End_element;
        1, 30, S  `End_element];

    expect ~context:None "<p><frameset><p>"
      [ 1,  1, S (start_element "p");
        1,  4, E (`Misnested_tag ("frameset", "body"));
        1, 14, S  `End_element;
        1, 14, S (start_element "p");
        1, 17, S  `End_element];

    expect "<p><frameset><frame></frameset>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, E (`Misnested_tag ("frameset", "body"));
        1,  4, S  `End_element;
        1,  4, S  `End_element;
        1,  4, S (start_element "frameset");
        1, 14, S (start_element "frame");
        1, 14, S  `End_element;
        1, 21, S  `End_element;
        1, 32, S  `End_element])
]
