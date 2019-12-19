(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common
module Error = Markup__Error

let xml_decl = Test_xml_tokenizer.xml_decl
let raw_doctype = Test_xml_tokenizer.raw_doctype

let start_element name =
  `Start_element (("", name), [])

let no_custom_entities = fun _ -> None
let no_top_level_namespaces = fun _ -> None

let expect ?context ?(namespace = no_top_level_namespaces) text signals =
  let report, iterate, ended =
    expect_signals signal_to_string text signals in

  text
  |> Markup__Stream_io.string
  |> Markup__Encoding.utf_8
  |> Markup__Input.preprocess is_valid_xml_char Error.ignore_errors
  |> Markup__Xml_tokenizer.tokenize Error.ignore_errors no_custom_entities
  |> Markup__Xml_parser.parse context namespace report
  |> iter iterate;

  ended ()

let tests = [
  ("xml.parser.empty" >:: fun _ ->
    expect "" []);

  ("xml.parser.document" >:: fun _ ->
    expect "<root>foo</root>"
      [ 1,  1, S (start_element "root");
        1,  7, S (`Text ["foo"]);
        1, 10, S  `End_element];

    expect "  <root > foo </root >  "
      [ 1,  3, S (start_element "root");
        1, 10, S (`Text [" foo "]);
        1, 15, S  `End_element];

    expect "<!DOCTYPE html><root>foo</root>"
      [ 1,  1, S (raw_doctype "html");
        1, 16, S (start_element "root");
        1, 22, S (`Text ["foo"]);
        1, 25, S  `End_element];

    expect "<?xml version='1.0'?><root>foo</root>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 22, S (start_element "root");
        1, 28, S (`Text ["foo"]);
        1, 31, S  `End_element];

    expect "<?xml version='1.0'?>  <!DOCTYPE html>  <root>foo</root>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 24, S (raw_doctype "html");
        1, 41, S (start_element "root");
        1, 47, S (`Text ["foo"]);
        1, 50, S  `End_element]);

  ("xml.parser.leading-comments" >:: fun _ ->
    expect
      "<?xml version='1.0'?> <!--foo--><!DOCTYPE html> <!--bar--><root></root>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 23, S (`Comment "foo");
        1, 33, S (raw_doctype "html");
        1, 49, S (`Comment "bar");
        1, 59, S (start_element "root");
        1, 65, S  `End_element];

    expect " <!-- foo --> <root></root>"
      [ 1,  2, S (`Comment " foo ");
        1, 15, S (start_element "root");
        1, 21, S  `End_element]);

  ("xml.parser.trailing-comment" >:: fun _ ->
    expect "<root></root> <!-- foo --> "
      [ 1,  1, S (start_element "root");
        1,  7, S  `End_element;
        1, 15, S (`Comment " foo ")]);

  ("xml.parser.leading-processing-instructions" >:: fun _ ->
    expect
      "<?xml version='1.0'?> <?foo bar?><!DOCTYPE html> <?bar foo?><a></a>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 23, S (`PI ("foo", "bar"));
        1, 34, S (raw_doctype "html");
        1, 50, S (`PI ("bar", "foo"));
        1, 61, S (start_element "a");
        1, 64, S  `End_element];

    expect " <?foo bar?> <root></root>"
      [ 1,  2, S (`PI ("foo", "bar"));
        1, 14, S (start_element "root");
        1, 20, S  `End_element]);

  ("xml.parser.trailing-processing-instruction" >:: fun _ ->
    expect "<root></root> <?foo bar?>"
      [ 1,  1, S (start_element "root");
        1,  7, S  `End_element;
        1, 15, S (`PI ("foo", "bar"))]);

  ("xml.parser.junk-before-xml-declaration" >:: fun _ ->
    expect " <?xml version='1.0'?><root></root>"
      [ 1,  2, E (`Bad_document "XML declaration must be first");
        1, 23, S (start_element "root");
        1, 29, S  `End_element];

    expect " <?xml version='1.0'?><!DOCTYPE html><root></root>"
      [ 1,  2, E (`Bad_document "XML declaration must be first");
        1, 23, S (raw_doctype "html");
        1, 38, S (start_element "root");
        1, 44, S  `End_element];

    expect "<!-- foo --><?xml version='1.0'?><!DOCTYPE html><root></root>"
      [ 1,  1, S (`Comment " foo ");
        1, 13, E (`Bad_document "XML declaration must be first");
        1, 34, S (raw_doctype "html");
        1, 49, S (start_element "root");
        1, 55, S  `End_element]);

  ("xml.parser.junk-before-doctype" >:: fun _ ->
    expect "<?xml version='1.0'?>foo<!DOCTYPE html><root></root>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 22, E (`Bad_document "text at top level");
        1, 25, S (raw_doctype "html");
        1, 40, S (start_element "root");
        1, 46, S  `End_element]);

  ("xml.parser.junk-before-root" >:: fun _ ->
    expect "<?xml version='1.0'?><!DOCTYPE html>foo<root></root>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 22, S (raw_doctype "html");
        1, 37, E (`Bad_document "expected root element");
        1, 40, S (start_element "root");
        1, 46, S  `End_element]);

  ("xml.parser.junk-after-root" >:: fun _ ->
    expect "<root></root>foo"
      [ 1,  1, S (start_element "root");
        1,  7, S  `End_element;
        1, 14, S (`Text ["foo"])];

    expect "<root></root> <foo></foo> <bar></bar>"
      [ 1,  1, S (start_element "root");
        1,  7, S  `End_element;
        1, 15, S (start_element "foo");
        1, 20, S  `End_element;
        1, 26, S (`Text [" "]);
        1, 27, S (start_element "bar");
        1, 32, S  `End_element];

    expect "<?xml version='1.0'?><root/>foo"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 22, S (start_element "root");
        1, 22, S  `End_element;
        1, 29, E (`Bad_document "not allowed after root element");
        1, 29, S (`Text ["foo"])]);

  ("xml.parser.self-closing-root" >:: fun _ ->
    expect "<root/>"
      [ 1,  1, S (start_element "root");
        1,  1, S  `End_element]);

  ("xml.parser.content" >:: fun _ ->
    expect "<root>foo<!--bar--><?baz quux?>&lt;<![CDATA[&gt;]]></root>"
      [ 1,  1, S (start_element "root");
        1,  7, S (`Text ["foo"]);
        1, 10, S (`Comment "bar");
        1, 20, S (`PI ("baz", "quux"));
        1, 32, S (`Text ["<&gt;"]);
        1, 52, S  `End_element];

    expect "<root><nested><more>foo</more></nested><a>bar</a><blah/></root>"
      [ 1,  1, S (start_element "root");
        1,  7, S (start_element "nested");
        1, 15, S (start_element "more");
        1, 21, S (`Text ["foo"]);
        1, 24, S  `End_element;
        1, 31, S  `End_element;
        1, 40, S (start_element "a");
        1, 43, S (`Text ["bar"]);
        1, 46, S  `End_element;
        1, 50, S (start_element "blah");
        1, 50, S  `End_element;
        1, 57, S  `End_element]);

  ("xml.parser.prolog-in-content" >:: fun _ ->
    expect "<root><?xml version='1.0'?><!DOCTYPE html></root>"
      [ 1,  1, S (start_element "root");
        1,  7, E (`Bad_document "XML declaration should be at top level");
        1, 28, E (`Bad_document "doctype should be at top level");
        1, 43, S  `End_element]);

  ("xml.parser.attributes" >:: fun _ ->
    expect "<root foo='bar'/>"
      [ 1,  1, S (`Start_element (("", "root"), [("", "foo"), "bar"]));
        1,  1, S  `End_element]);

  ("xml.parser.bad-attributes" >:: fun _ ->
    expect "<root foo='bar' foo='baz'/>"
      [ 1,  1, E (`Bad_token ("foo", "tag", "duplicate attribute"));
        1,  1, S (`Start_element (("", "root"), [("", "foo"), "bar"]));
        1,  1, S  `End_element];

    expect "<root xmlns:a='some_ns' xmlns:b='some_ns' a:foo='' b:foo=''/>"
      [ 1,  1, E (`Bad_token ("foo", "tag", "duplicate attribute"));
        1,  1, S (`Start_element (("", "root"),
                    [(xmlns_ns, "a"), "some_ns";
                     (xmlns_ns, "b"), "some_ns";
                     ("some_ns", "foo"), ""]));
        1,  1, S  `End_element]);

  ("xml.parser.misnested-tags" >:: fun _ ->
    expect "<foo>"
      [ 1,  1, S (start_element "foo");
        1,  1, E (`Unmatched_start_tag "foo");
        1,  6, S  `End_element];

    expect "<foo></bar></foo>"
      [ 1,  1, S (start_element "foo");
        1,  6, E (`Unmatched_end_tag "bar");
        1, 12, S  `End_element];

    expect "<foo><bar><baz></foo>"
      [ 1,  1, S (start_element "foo");
        1,  6, S (start_element "bar");
        1, 11, S (start_element "baz");
        1, 11, E (`Unmatched_start_tag "baz");
        1, 16, S  `End_element;
        1,  6, E (`Unmatched_start_tag "bar");
        1, 16, S  `End_element;
        1, 16, S  `End_element]);

  ("xml.parser.fragment" >:: fun _ ->
    expect "foo<bar/>"
      [ 1,  1, S (`Text ["foo"]);
        1,  4, S (start_element "bar");
        1,  4, S  `End_element];

    expect " <!-- foo --> bar <baz></baz> <quux/>"
      [ 1,  1, S (`Text [" "]);
        1,  2, S (`Comment " foo ");
        1, 14, S (`Text [" bar "]);
        1, 19, S (start_element "baz");
        1, 24, S  `End_element;
        1, 30, S (`Text [" "]);
        1, 31, S (start_element "quux");
        1, 31, S  `End_element];

    expect "foo"
      [ 1,  1, S (`Text ["foo"])]);

  ("xml.parser.namespaces" >:: fun _ ->
    expect
      "<root xmlns='some_ns' xmlns:a='other_ns' a:foo='bar' baz='quux'></root>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "xmlns"), "some_ns";
                     (xmlns_ns, "a"), "other_ns";
                     ("other_ns", "foo"), "bar";
                     ("", "baz"), "quux"]));
        1, 65, S  `End_element];

    expect "<a:root xmlns:a='some_ns' xmlns:b='other_ns'></b:root>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "a"), "some_ns";
                     (xmlns_ns, "b"), "other_ns"]));
        1, 46, E (`Unmatched_end_tag "b:root");
        1,  1, E (`Unmatched_start_tag "a:root");
        1, 55, S  `End_element];

    expect "<a:root xmlns:a='some_ns' xmlns:b='some_ns'></b:root>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "a"), "some_ns";
                     (xmlns_ns, "b"), "some_ns"]));
        1, 45, S  `End_element];

    expect "<root xmlns='some_ns'><foo bar='baz'/></root>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "xmlns"), "some_ns"]));
        1, 23, S (`Start_element (("some_ns", "foo"),
                    [("", "bar"), "baz"]));
        1, 23, S  `End_element;
        1, 39, S  `End_element];

    expect "<root xmlns:a='some_ns'><a:foo bar='baz'/><quux/></root>"
      [ 1,  1, S (`Start_element (("", "root"),
                    [(xmlns_ns, "a"), "some_ns"]));
        1, 25, S (`Start_element (("some_ns", "foo"),
                    [("", "bar"), "baz"]));
        1, 25, S  `End_element;
        1, 43, S (`Start_element (("", "quux"), []));
        1, 43, S  `End_element;
        1, 50, S  `End_element];

    expect
      ("<root xmlns:a='some_ns' xmlns:b='other_ns'><foo xmlns:a='another_ns'>" ^
       "<a:bar/><b:baz/></foo></root>")
      [ 1,  1, S (`Start_element (("", "root"),
                    [(xmlns_ns, "a"), "some_ns";
                     (xmlns_ns, "b"), "other_ns"]));
        1, 44, S (`Start_element (("", "foo"),
                    [(xmlns_ns, "a"), "another_ns"]));
        1, 70, S (`Start_element (("another_ns", "bar"), []));
        1, 70, S  `End_element;
        1, 78, S (`Start_element (("other_ns", "baz"), []));
        1, 78, S  `End_element;
        1, 86, S  `End_element;
        1, 92, S  `End_element];

    expect "<root xmlns='some_ns'><foo xmlns='other_ns'><bar/></foo></root>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "xmlns"), "some_ns"]));
        1, 23, S (`Start_element (("other_ns", "foo"),
                    [(xmlns_ns, "xmlns"), "other_ns"]));
        1, 45, S (`Start_element (("other_ns", "bar"), []));
        1, 45, S  `End_element;
        1, 51, S  `End_element;
        1, 57, S  `End_element];

    expect "<root xmlns='some_ns'></root><foo/>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                    [(xmlns_ns, "xmlns"), "some_ns"]));
        1, 23, S  `End_element;
        1, 30, S (start_element "foo");
        1, 30, S  `End_element];

    expect
      ("<root xmlns:a='some_ns'><foo xmlns:a='other_ns'><a:bar/></foo>" ^
        "<a:baz/></root>")
      [ 1,  1, S (`Start_element (("", "root"),
                    [(xmlns_ns, "a"), "some_ns"]));
        1, 25, S (`Start_element (("", "foo"),
                    [(xmlns_ns, "a"), "other_ns"]));
        1, 49, S (`Start_element (("other_ns", "bar"), []));
        1, 49, S  `End_element;
        1, 57, S  `End_element;
        1, 63, S (`Start_element (("some_ns", "baz"), []));
        1, 63, S  `End_element;
        1, 71, S  `End_element]);

  ("xml.parser.bad-namespaces" >:: fun _ ->
    expect "<a:root><foo b:bar=''/></a:root>"
      [ 1,  1, E (`Bad_namespace "a");
        1,  1, S (`Start_element (("a", "root"), []));
        1,  9, E (`Bad_namespace "b");
        1,  9, S (`Start_element (("", "foo"),
                    [("b", "bar"), ""]));
        1,  9, S  `End_element;
        1, 24, E (`Bad_namespace "a");
        1, 24, S  `End_element]);

  ("xml.parser.custom-namespaces" >:: fun _ ->
    let namespace = function
      | "a" -> Some "some_ns"
      | "b" -> Some "other_ns"
      | "xmlns" -> Some "bad"
      | _ -> None
    in

    expect ~namespace "<a:root b:foo='bar' xmlns:c='baz'/>"
      [ 1,  1, S (`Start_element (("some_ns", "root"),
                  [("other_ns", "foo"), "bar";
                   (xmlns_ns, "c"), "baz"]));
        1,  1, S  `End_element])
]
