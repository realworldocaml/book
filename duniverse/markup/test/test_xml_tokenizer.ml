(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common
module Error = Markup__Error

let xml_decl version encoding standalone =
  `Xml {version; encoding; standalone}

let raw_doctype s =
  `Doctype
    {doctype_name      = None;
     public_identifier = None;
     system_identifier = None;
     raw_text          = Some s;
     force_quirks      = false}

let tag ?(self_closing = false) name attributes =
  {Token_tag.name; attributes; self_closing}

let no_custom_entities = fun _ -> None

let expect ?(entity = no_custom_entities) text signals =
  let report, iterate, ended = expect_signals token_to_string text signals in

  text
  |> Markup__Stream_io.string
  |> Markup__Encoding.utf_8
  |> Markup__Input.preprocess is_valid_xml_char Error.ignore_errors
  |> Markup__Xml_tokenizer.tokenize report entity
  |> iter iterate;

  ended ()

let xml = "xml declaration"
let pi = "processing instruction"

let tests = [
  ("xml.tokenizer.empty" >:: fun _ ->
    expect ""
      [ 1,  1, S  `EOF]);

  ("xml.tokenizer.whitespace" >:: fun _ ->
    expect " \t \n \x09 \x0d \x0d\x0a "
      [ 1,  1, S (`Chars [" \t \n \x09 \x0a \x0a "]);
        4,  2, S  `EOF]);

  ("xml.tokenizer.text" >:: fun _ ->
    expect "foo bar"
      [ 1,  1, S (`Chars ["foo bar"]);
        1,  8, S  `EOF];

    expect "foo > bar"
      [ 1,  1, S (`Chars ["foo > bar"]);
        1, 10, S  `EOF]);

  ("xml.tokenizer.spurious-cdata-end" >:: fun _ ->
    expect "foo]]>bar"
      [ 1,  4, E (`Bad_token ("]]>", "text", "must end a CDATA section"));
        1,  1, S (`Chars ["foo]]>bar"]);
        1, 10, S  `EOF];

    expect "foo]]]>bar"
      [ 1,  5, E (`Bad_token ("]]>", "text", "must end a CDATA section"));
        1,  1, S (`Chars ["foo]]]>bar"]);
        1, 11, S  `EOF];

    expect "foo]]bar"
      [ 1,  1, S (`Chars ["foo]]bar"]);
        1,  9, S  `EOF];

    expect "foo]>bar"
      [ 1,  1, S (`Chars ["foo]>bar"]);
        1,  9, S  `EOF]);

  ("xml.tokenizer.comment" >:: fun _ ->
    expect "text<!-- foo -->text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (`Comment " foo ");
        1, 17, S (`Chars ["text"]);
        1, 21, S  `EOF]);

  ("xml.tokenizer.bad-comment-start" >:: fun _ ->
    expect "text<!foo -->"
      [ 1,  5, E (`Bad_token ("<!", "comment", "should start with '<!--'"));
        1,  5, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["text<!foo -->"]);
        1, 14, S  `EOF];

    expect "<!<!-- foo -->"
      [ 1,  1, E (`Bad_token ("<!", "comment", "should start with '<!--'"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!"]);
        1,  3, S (`Comment " foo ");
        1, 15, S  `EOF];

    expect "<!"
      [ 1,  1, E (`Bad_token ("<!", "comment", "should start with '<!--'"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!"]);
        1,  3, S  `EOF];

    expect "text<!-foo -->"
      [ 1,  5, E (`Bad_token ("<!-", "comment", "should start with '<!--'"));
        1,  5, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["text<!-foo -->"]);
        1, 15, S  `EOF];

    expect "<!-<!-- foo -->"
      [ 1,  1, E (`Bad_token ("<!-", "comment", "should start with '<!--'"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!-"]);
        1,  4, S (`Comment " foo ");
        1, 16, S  `EOF];

    expect "<!-"
      [ 1,  1, E (`Bad_token ("<!-", "comment", "should start with '<!--'"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!-"]);
        1,  4, S  `EOF]);

  ("xml.tokenizer.comment-end" >:: fun _ ->
    expect "<!-- foo - -->"
      [ 1,  1, S (`Comment " foo - ");
        1, 15, S  `EOF];

    expect "<!-- foo -- -->"
      [ 1, 10, E (`Bad_token ("--", "comment", "should be followed by '>'"));
        1,  1, S (`Comment " foo -- ");
        1, 16, S  `EOF];

    expect "<!-- foo --->"
      [ 1, 10, E (`Bad_token ("--", "comment", "should be followed by '>'"));
        1,  1, S (`Comment " foo -");
        1, 14, S  `EOF]);

  ("xml.tokenizer.unterminated-comment" >:: fun _ ->
    expect "<!--"
      [ 1,  1, S (`Comment "");
        1,  5, E (`Unexpected_eoi "comment");
        1,  5, S  `EOF];

    expect "<!-- foo"
      [ 1,  1, S (`Comment " foo");
        1,  9, E (`Unexpected_eoi "comment");
        1,  9, S  `EOF];

    expect "<!-- foo -"
      [ 1,  1, S (`Comment " foo ");
        1, 11, E (`Unexpected_eoi "comment");
        1, 11, S  `EOF];

    expect "<!-- foo --"
      [ 1,  1, S (`Comment " foo ");
        1, 12, E (`Unexpected_eoi "comment");
        1, 12, S  `EOF]);

  ("xml.tokenizer.markup-in-comment" >:: fun _ ->
    expect "<!-- <foo> -->"
      [ 1,  1, S (`Comment " <foo> ");
        1, 15, S  `EOF];

    expect "<!-- &gt; -->"
      [ 1,  1, S (`Comment " &gt; ");
        1, 14, S  `EOF]);

  ("xml.tokenizer.cdata" >:: fun _ ->
    expect "text<![CDATA[foo<bar>&amp;]]baz]]]quux]]]>text"
      [ 1,  1, S (`Chars ["textfoo<bar>&amp;]]baz]]]quux]text"]);
        1, 47, S  `EOF]);

  ("xml.tokenizer.bad-cdata-start" >:: fun _ ->
    expect "<![foo"
      [ 1,  1, E (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<![foo"]);
        1,  7, S  `EOF];

    expect "<![cdata"
      [ 1,  1, E (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<![cdata"]);
        1,  9, S  `EOF];

    expect "<![<![CDATA[bar]]>"
      [ 1,  1, E (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<![bar"]);
        1, 19, S  `EOF];

    expect "<!["
      [ 1,  1, E (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!["]);
        1,  4, S  `EOF]);

  ("xml.tokenizer.unterminated-cdata" >:: fun _ ->
    expect "<![CDATA[foo"
      [ 1,  1, S (`Chars ["foo"]);
        1, 13, E (`Unexpected_eoi "cdata");
        1, 13, S  `EOF];

    expect "<![CDATA[foo]"
      [ 1,  1, S (`Chars ["foo"]);
        1, 14, E (`Unexpected_eoi "cdata");
        1, 14, S  `EOF];

    expect "<![CDATA[foo]]"
      [ 1,  1, S (`Chars ["foo"]);
        1, 15, E (`Unexpected_eoi "cdata");
        1, 15, S  `EOF]);

  ("xml.tokenizer.doctype" >:: fun _ ->
    expect "text<!DOCTYPE html [ <!ELEMENT foo (#PCDATA)> ]>text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (raw_doctype "html [ <!ELEMENT foo (#PCDATA)> ]");
        1, 49, S (`Chars ["text"]);
        1, 53, S  `EOF];

    expect "text<!DOCTYPE html SYSTEM \"html.dtd\">text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (raw_doctype "html SYSTEM \"html.dtd\"");
        1, 38, S (`Chars ["text"]);
        1, 42, S  `EOF];

    expect "text<!DOCTYPE html SYSTEM \"<!ELEMENT\">text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (raw_doctype "html SYSTEM \"<!ELEMENT\"");
        1, 39, S (`Chars ["text"]);
        1, 43, S  `EOF];

    expect "text<!DOCTYPE html SYSTEM 'html.dtd'>text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (raw_doctype "html SYSTEM 'html.dtd'");
        1, 38, S (`Chars ["text"]);
        1, 42, S  `EOF];

    expect "text<!DOCTYPE html SYSTEM '<!ELEMENT'>text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (raw_doctype "html SYSTEM '<!ELEMENT'");
        1, 39, S (`Chars ["text"]);
        1, 43, S  `EOF]);

  ("xml.tokenizer.bad-doctype-start" >:: fun _ ->
    let error =
      `Bad_token ("<!D", "doctype", "should start with '<!DOCTYPE '") in

    expect "<!Doctype html>"
      [ 1,  1, E  error;
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!Doctype html>"]);
        1, 16, S  `EOF];

    expect "<!D<!DOCTYPE html>"
      [ 1,  1, E  error;
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!D"]);
        1,  4, S (raw_doctype "html");
        1, 19, S `EOF];

    expect "<!DOC"
      [ 1,  1, E  error;
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<!DOC"]);
        1,  6, S  `EOF]);

  ("xml.tokenizer.unterminated-doctype" >:: fun _ ->
    expect "<!DOCTYPE html"
      [ 1,  1, S (raw_doctype "html");
        1, 15, E (`Unexpected_eoi "doctype");
        1, 15, S  `EOF];

    expect "<!DOCTYPE html SYSTEM \"foo>"
      [ 1,  1, S (raw_doctype "html SYSTEM \"foo>");
        1, 28, E (`Unexpected_eoi "doctype");
        1, 28, S  `EOF];

    expect "<!DOCTYPE html SYSTEM 'foo>"
      [ 1,  1, S (raw_doctype "html SYSTEM 'foo>");
        1, 28, E (`Unexpected_eoi "doctype");
        1, 28, S  `EOF];

    expect "<!DOCTYPE html [ <!ELEMENT "
      [ 1,  1, S (raw_doctype "html [ <!ELEMENT ");
        1, 28, E (`Unexpected_eoi "doctype");
        1, 28, S  `EOF]);

  ("xml.tokenizer.pi-in-doctype" >:: fun _ ->
    expect "<!DOCTYPE html [ <?foo bar?> ]>"
      [ 1,  1, S (raw_doctype "html [ <?foo bar?> ]");
        1, 32, S  `EOF];

    expect "<!DOCTYPE html [ <?foo bar ]>"
      [ 1, 30, E (`Unexpected_eoi "processing instruction");
        1,  1, S (raw_doctype "html [ <?foo bar ]>");
        1, 30, E (`Unexpected_eoi "doctype");
        1, 30, S  `EOF]);

  ("xml.tokenizer.comment-in-doctype" >:: fun _ ->
    expect "<!DOCTYPE html [ <!-- foo --> ]>"
      [ 1,  1, S (raw_doctype "html [ <!-- foo --> ]");
        1, 33, S  `EOF];

    expect "<!DOCTYPE html [ <!-- foo ]>"
      [ 1,  1, S (raw_doctype "html [ <!-- foo ]>");
        1, 29, E (`Unexpected_eoi "doctype");
        1, 29, S  `EOF]);

  ("xml.tokenizer.reference" >:: fun _ ->
    expect "foo&lt;bar&gt;&amp;&quot;&apos;baz&#48;&#x31;quux"
      [ 1,  1, S (`Chars ["foo<bar>&\"'baz01quux"]);
        1, 50, S  `EOF]);

  ("xml.tokenizer.bad-reference" >:: fun _ ->
    expect "&"
      [ 1,  2, E (`Unexpected_eoi "reference");
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&"]);
        1,  2, S  `EOF];

    expect "&lt"
      [ 1,  4, E (`Unexpected_eoi "reference");
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&lt"]);
        1,  4, S  `EOF];

    expect "&;"
      [ 1,  1, E (`Bad_token ("&;", "reference", "empty reference"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&;"]);
        1,  3, S  `EOF];

    expect "&<!-- foo -->"
      [ 1,  2, E (`Bad_token ("<", "reference", "invalid start character"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&"]);
        1,  2, S (`Comment " foo ");
        1, 14, S  `EOF];

    expect "&lt<!-- foo -->"
      [ 1,  4, E (`Bad_token ("<", "reference", "invalid name character"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&lt"]);
        1,  4, S (`Comment " foo ");
        1, 16, S  `EOF];

    expect "&#<!-- foo -->"
      [ 1,  3, E (`Bad_token ("<", "reference", "expected digit"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&#"]);
        1,  3, S (`Comment " foo ");
        1, 15, S  `EOF];

    expect "&#;"
      [ 1,  1, E (`Bad_token ("&#;", "reference", "empty character reference"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&#;"]);
        1,  4, S  `EOF];

    expect "&#x<!-- foo -->"
      [ 1,  4, E (`Bad_token ("<", "reference", "expected digit"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&#x"]);
        1,  4, S (`Comment " foo ");
        1, 16, S  `EOF];

    let empty_character_reference = "empty character reference" in
    expect "&#x;"
      [ 1,  1, E (`Bad_token ("&#x;", "reference", empty_character_reference));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&#x;"]);
        1,  5, S  `EOF];

    expect "&#6a;"
      [ 1,  4, E (`Bad_token ("a", "reference", "expected digit"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&#6a;"]);
        1,  6, S  `EOF];

    let absurd = "&#x1000000000000000000000000000000000000000;" in
    expect absurd
      [ 1,  1, E (`Bad_token (absurd, "reference", "number out of range"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars [absurd]);
        1, 45, S  `EOF]);

  ("xml.tokenizer.custom-reference" >:: fun _ ->
    let entity = function
      | "test" -> Some "custom"
      | "lt" -> Some "foobar"
      | _ -> None
    in

    expect ~entity "&test; &lt;"
      [ 1,  1, S (`Chars ["custom <"]);
        1, 12, S  `EOF];

    expect ~entity "&other;"
      [ 1,  1, E (`Bad_token ("other", "reference", "unknown entity"));
        1,  1, E (`Bad_token ("&", "text", "replace with '&amp;'"));
        1,  1, S (`Chars ["&other;"]);
        1,  8, S  `EOF]);

  ("xml.tokenizer.faux-markup" >:: fun _ ->
    expect "&lt;!-- foo -->"
      [ 1,  1, S (`Chars ["<!-- foo -->"]);
        1, 16, S  `EOF]);

  ("xml.tokenizer.start-tag" >:: fun _ ->
    expect "text<foo>text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (`Start (tag "foo" []));
        1, 10, S (`Chars ["text"]);
        1, 14, S  `EOF];

    expect "<foo  >"
      [ 1,  1, S (`Start (tag "foo" []));
        1,  8, S  `EOF]);

  ("xml.tokenizer.self-closing-tag" >:: fun _ ->
    expect "<foo/>"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1,  7, S  `EOF];

    expect "<foo  />"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1,  9, S  `EOF]);

  ("xml.tokenizer.end-tag" >:: fun _ ->
    expect "</foo>"
      [ 1,  1, S (`End (tag "foo" []));
        1,  7, S  `EOF];

    expect "</foo   >"
      [ 1,  1, S (`End (tag "foo" []));
        1, 10, S  `EOF]);

  ("xml.tokenizer.bad-tag-names" >:: fun _ ->
    expect "<<foo>"
      [ 1,  2, E (`Bad_token ("<", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<"]);
        1,  2, S (`Start (tag "foo" []));
        1,  7, S  `EOF];

    expect "</<foo>"
      [ 1,  3, E (`Bad_token ("<", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</"]);
        1,  3, S (`Start (tag "foo" []));
        1,  8, S  `EOF];

    expect "<abc<foo>"
      [ 1,  5, E (`Bad_token ("<", "tag", "invalid name character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<abc"]);
        1,  5, S (`Start (tag "foo" []));
        1, 10, S  `EOF];

    expect "</abc<foo>"
      [ 1,  6, E (`Bad_token ("<", "tag", "invalid name character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</abc"]);
        1,  6, S (`Start (tag "foo" []));
        1, 11, S  `EOF];

    expect "< foo>"
      [ 1,  2, E (`Bad_token (" ", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["< foo>"]);
        1,  7, S  `EOF];

    expect "</ foo>"
      [ 1,  3, E (`Bad_token (" ", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</ foo>"]);
        1,  8, S  `EOF];

    expect "<>"
      [ 1,  2, E (`Bad_token (">", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<>"]);
        1,  3, S  `EOF];

    expect "</>"
      [ 1,  3, E (`Bad_token (">", "tag", "invalid start character"));
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</>"]);
        1,  4, S  `EOF]);

  ("xml.tokenizer.junk-in-end-tag" >:: fun _ ->
    expect "</foo bar>"
      [ 1,  7, E (`Bad_token ("b", "tag", "attribute in end tag"));
        1,  1, S (`End (tag "foo" []));
        1, 11, S  `EOF]);

  ("xml.tokenizer.stray-slash-in-tag" >:: fun _ ->
    expect "<foo / />"
      [ 1,  6, E (`Bad_token ("/", "tag", "should be part of '/>'"));
        1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1, 10, S  `EOF]);

  ("xml.tokenizer.unterminated-tag" >:: fun _ ->
    expect "<foo"
      [ 1,  5, E (`Unexpected_eoi "tag");
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["<foo"]);
        1,  5, S  `EOF];

    expect "foo<"
      [ 1,  5, E (`Unexpected_eoi "tag");
        1,  4, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["foo<"]);
        1,  5, S  `EOF];

    expect "</"
      [ 1,  3, E (`Unexpected_eoi "tag");
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</"]);
        1,  3, S  `EOF];

    expect "</foo"
      [ 1,  6, E (`Unexpected_eoi "tag");
        1,  1, E (`Bad_token ("<", "text", "replace with '&lt;'"));
        1,  1, S (`Chars ["</foo"]);
        1,  6, S  `EOF];

    expect "</foo "
      [ 1,  1, S (`End (tag "foo" []));
        1,  7, E (`Unexpected_eoi "tag");
        1,  7, S  `EOF];

    expect "<foo /"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1,  7, E (`Unexpected_eoi "tag");
        1,  7, S  `EOF];

    expect "<foo bar='' "
      [ 1,  1, S (`Start (tag "foo" ["bar", ""]));
        1, 13, E (`Unexpected_eoi "tag");
        1, 13, S  `EOF]);

  ("xml.tokenizer.attributes" >:: fun _ ->
    expect "<foo bar='baz' blah='blahh'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "baz"; "blah", "blahh"]));
        1, 29, S  `EOF];

    let attributes = ["bar", "baz"; "blah", "blahh"] in
    expect "<foo bar='baz' blah='blahh'/>"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" attributes));
        1, 30, S  `EOF];

    expect "<foo   bar=''    baz=\"'\"  blah='\"'  >"
      [ 1,  1, S (`Start (tag "foo" ["bar", ""; "baz", "'"; "blah", "\""]));
        1, 38, S  `EOF];

    expect "<foo bar='>' baz='/'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", ">"; "baz", "/"]));
        1, 22, S  `EOF]);

  ("xml.tokenizer.references-in-attributes" >:: fun _ ->
    expect "<foo bar='&lt;'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "<"]));
        1, 17, S  `EOF];

    expect "<foo bar=\"&lt;\">"
      [ 1,  1, S (`Start (tag "foo" ["bar", "<"]));
        1, 17, S  `EOF];

    expect "<foo bar='&#48;&#x31;'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "01"]));
        1, 24, S  `EOF];

    expect "<foo bar='&'>text"
      [ 1, 12, E (`Bad_token ("'", "reference", "invalid start character"));
        1, 11, E (`Bad_token ("&", "attribute", "replace with '&amp;'"));
        1,  1, S (`Start (tag "foo" ["bar", "&"]));
        1, 14, S (`Chars ["text"]);
        1, 18, S  `EOF]);

  ("xml.tokenizer.bad-attribute-name" >:: fun _ ->
    expect "<foo !bar=''>"
      [ 1,  6, E (`Bad_token ("!", "attribute", "invalid start character"));
        1,  1, S (`Start (tag "foo" ["!bar", ""]));
        1, 14, S  `EOF];

    expect "<foo b!ar=''>"
      [ 1,  7, E (`Bad_token ("!", "attribute", "invalid name character"));
        1,  1, S (`Start (tag "foo" ["b!ar", ""]));
        1, 14, S  `EOF]);

  ("xml.tokenizer.good-attribute-name" >:: fun _ ->
    expect "<foo -bar=''>"
      [ 1,  6, E (`Bad_token ("-", "attribute", "invalid start character"));
        1,  1, S (`Start (tag "foo" ["-bar", ""]));
        1, 14, S  `EOF];

    expect "<foo b-ar=''>"
      [ 1,  1, S (`Start (tag "foo" ["b-ar", ""]));
        1, 14, S  `EOF]);

  ("xml.tokenizer.bad-attribute-whitespace" >:: fun _ ->
    expect "<foo bar  =  'baz'>"
      [ 1,  9, E (`Bad_token (" ", "attribute", "whitespace not allowed here"));
        1, 12, E (`Bad_token (" ", "attribute", "whitespace not allowed here"));
        1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 20, S  `EOF]);

  ("xml.tokenizer.bad-attribute-value" >:: fun _ ->
    expect "<foo bar>"
      [ 1,  6, E (`Bad_token ("bar", "attribute", "has no value"));
        1,  1, S (`Start (tag "foo" ["bar", ""]));
        1, 10, S  `EOF];

    expect "<foo bar=>"
      [ 1,  6, E (`Bad_token ("bar", "attribute", "has no value"));
        1,  1, S (`Start (tag "foo" ["bar", ""]));
        1, 11, S  `EOF];

    expect "<foo bar=baz blah=''>"
      [ 1, 10, E (`Bad_token ("b", "attribute", "unquoted value"));
        1,  1, S (`Start (tag "foo" ["bar", "baz"; "blah", ""]));
        1, 22, S  `EOF];

    expect "<foo bar= baz>"
      [ 1, 10, E (`Bad_token (" ", "attribute", "whitespace not allowed here"));
        1, 11, E (`Bad_token ("b", "attribute", "unquoted value"));
        1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 15, S  `EOF];

    expect "<foo bar=&amp;>"
      [ 1, 10, E (`Bad_token ("&", "attribute", "unquoted value"));
        1,  1, S (`Start (tag "foo" ["bar", "&"]));
        1, 16, S  `EOF];

    expect "<foo bar=&>"
      [ 1, 10, E (`Bad_token ("&", "attribute", "unquoted value"));
        1, 11, E (`Bad_token (">", "reference", "invalid start character"));
        1, 10, E (`Bad_token ("&", "attribute", "replace with '&amp;'"));
        1,  1, S (`Start (tag "foo" ["bar", "&"]));
        1, 12, S  `EOF];

    expect "<foo bar='<' baz=<>"
      [ 1, 11, E (`Bad_token ("<", "attribute", "replace with '&lt;'"));
        1, 18, E (`Bad_token ("<", "attribute", "unquoted value"));
        1, 18, E (`Bad_token ("<", "attribute", "replace with '&lt;'"));
        1,  1, S (`Start (tag "foo" ["bar", "<"; "baz", "<"]));
        1, 20, S  `EOF];

    expect "<foo bar='baz"
      [ 1, 14, E (`Unexpected_eoi "attribute value");
        1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 14, E (`Unexpected_eoi "tag");
        1, 14, S  `EOF]);

  ("xml.tokenizer.processing-instruction" >:: fun _ ->
    expect "text<?target content?>text"
      [ 1,  1, S (`Chars ["text"]);
        1,  5, S (`PI ("target", "content"));
        1, 23, S (`Chars ["text"]);
        1, 27, S  `EOF];

    (* Disabled to avoid trigraph warning from the C compiler. *)
    (*
    expect "<?target  content ? ??>"
      [ 1,  1, S (`PI ("target", " content ? ?"));
        1, 24, S  `EOF];
    *)

    expect "<?target &amp;<?>"
      [ 1,  1, S (`PI ("target", "&amp;<"));
        1, 18, S  `EOF]);

  ("xml.tokenizer.bad-processing-instruction" >:: fun _ ->
    expect "<? target content?>"
      [ 1,  3, E (`Bad_token (" ", pi, "whitespace not allowed here"));
        1,  1, S (`PI ("target", "content"));
        1, 20, S  `EOF];

    expect "<?&amp; content?>"
      [ 1,  3, E (`Bad_token ("&", pi, "invalid start character"));
        1,  7, E (`Bad_token (";", pi, "invalid name character"));
        1,  1, S (`PI ("&amp;", "content"));
        1, 18, S  `EOF];

    (* Disabled to avoid trigraph warning from the C compiler. *)
    (*
    expect "<??>"
      [ 1,  1, E (`Bad_token ("<?...", pi, "empty"));
        1,  5, S  `EOF]
    *));

  ("xml.tokenizer.unterminated-processing-instruction" >:: fun _ ->
    expect "<?"
      [ 1,  3, E (`Unexpected_eoi pi);
        1,  1, E (`Bad_token ("<?...", pi, "empty"));
        1,  3, S  `EOF];

    expect "<?target"
      [ 1,  9, E (`Unexpected_eoi pi);
        1,  1, S (`PI ("target", ""));
        1,  9, S  `EOF];

    expect "<?target content"
      [ 1, 17, E (`Unexpected_eoi pi);
        1,  1, S (`PI ("target", "content"));
        1, 17, S  `EOF];

    expect "<?target content?"
      [ 1, 18, E (`Unexpected_eoi pi);
        1,  1, S (`PI ("target", "content"));
        1, 18, S  `EOF]);

  ("xml.tokenizer.xml-declaration" >:: fun _ ->
    expect "<?xml version='1.0'?>"
      [ 1,  1, S (xml_decl "1.0" None None);
        1, 22, S  `EOF];

    expect "<?xml  version='1.1'  encoding=\"utf-8\" standalone='yes'  ?>"
      [ 1,  1, S (xml_decl "1.1" (Some "utf-8") (Some true));
        1, 60, S  `EOF];

    expect "<?xml version='1.2' standalone='no'?>"
      [ 1,  1, S (xml_decl "1.2" None (Some false));
        1, 38, S  `EOF]);

  ("xml.tokenizer.case-mismatch-in-xml-declaration" >:: fun _ ->
    expect "<?XmL VeRsIoN='1.0' ENCODING='utf-8' sTaNdAlOnE='YeS'?>"
      [ 1,  1, E (`Bad_token ("XmL", xml, "must be 'xml'"));
        1,  7, E (`Bad_token ("VeRsIoN", xml, "must be 'version'"));
        1, 21, E (`Bad_token ("ENCODING", xml, "must be 'encoding'"));
        1, 38, E (`Bad_token ("sTaNdAlOnE", xml, "must be 'standalone'"));
        1, 38, E (`Bad_token ("YeS", xml, "must be 'yes' or 'no'"));
        1,  1, S (xml_decl "1.0" (Some "utf-8") (Some true));
        1, 56, S  `EOF]);

  ("xml.tokenizer.missing-xml-version" >:: fun _ ->
    expect "<?xml?>"
      [ 1,  1, E (`Bad_token ("<?xml...", xml, "missing version"));
        1,  1, S (xml_decl "1.0" None None);
        1,  8, S  `EOF];

    expect "<?xml ?>"
      [ 1,  1, E (`Bad_token ("<?xml...", xml, "missing version"));
        1,  1, S (xml_decl "1.0" None None);
        1,  9, S  `EOF];

    expect "<?XmL?>"
      [ 1,  1, E (`Bad_token ("XmL", xml, "must be 'xml'"));
        1,  1, E (`Bad_token ("<?xml...", xml, "missing version"));
        1,  1, S (xml_decl "1.0" None None);
        1,  8, S  `EOF];

    expect "<?xml standalone='yes'?>"
      [ 1,  1, E (`Bad_token ("<?xml...", xml, "missing version"));
        1,  1, S (xml_decl "1.0" None (Some true));
        1, 25, S  `EOF]);

  ("xml.tokenizer.version-not-first" >:: fun _ ->
    expect "<?xml standalone='yes' version='1.0'?>"
      [ 1, 24, E (`Bad_token ("version", xml, "must be first"));
        1,  1, S (xml_decl "1.0" None (Some true));
        1, 39, S  `EOF]);

  ("xml.tokenizer.bad-version" >:: fun _ ->
    expect "<?xml version='2.0'?>"
      [ 1,  7, E (`Bad_token ("2.0", xml, "must match 1.x"));
        1,  1, S (xml_decl "2.0" None None);
        1, 22, S  `EOF]);

  ("xml.tokenizer.bad-standalone" >:: fun _ ->
    expect "<?xml version='1.0' standalone='maybe'?>"
      [ 1, 21, E (`Bad_token ("maybe", xml, "must be 'yes' or 'no'"));
        1,  1, S (xml_decl "1.0" None None);
        1, 41, S  `EOF];

    expect "<?xml version='1.0' standalone='yes' encoding='utf-8'?>"
      [ 1, 21, E (`Bad_token ("standalone", xml, "must come after 'encoding'"));
        1,  1, S (xml_decl "1.0" (Some "utf-8") (Some true));
        1, 56, S  `EOF]);

  ("xml.tokenizer.junk-in-xml-declaration" >:: fun _ ->
    expect "<?xml version='1.0' foo='bar'?>"
      [ 1, 21, E (`Bad_token ("foo", xml, "not allowed here"));
        1,  1, S (xml_decl "1.0" None None);
        1, 32, S  `EOF]);

  ("xml.tokenizer.unterminated-xml-declaration" >:: fun _ ->
    expect "<?xml "
      [ 1,  7, E (`Unexpected_eoi xml);
        1,  1, E (`Bad_token ("<?xml...", xml, "missing version"));
        1,  1, S (xml_decl "1.0" None None);
        1,  7, S  `EOF];

    expect "<?xml version='1.0' "
      [ 1, 21, E (`Unexpected_eoi xml);
        1,  1, S (xml_decl "1.0" None None);
        1, 21, S  `EOF]);

  ("xml.tokenizer.large-text" >:: fun _ ->
    with_text_limit 8 begin fun () ->
      expect "foobar"
        [ 1,  1, S (`Chars ["foobar"]);
          1,  7, S  `EOF];

      expect "foobarbaz"
        [ 1,  1, S (`Chars ["foobarba"; "z"]);
          1, 10, S  `EOF]
    end)
]
