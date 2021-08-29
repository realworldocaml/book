(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common

let expect id signals strings =
  let _, iterate, ended = expect_strings id strings in

  signals
  |> Markup__Kstream.of_list
  |> Markup__Html_writer.write
  |> iter iterate;

  ended ()

let tests = [
  ("html.writer.empty" >:: fun _ ->
    expect "empty" [] []);

  ("html.writer.text" >:: fun _ ->
    expect "text" [`Text ["foo"]] [S "foo"];
    expect "adjacent text" [`Text ["foo"]; `Text ["bar"]] [S "foo"; S "bar"];
    expect "empty text" [`Text [""]] []);

  ("html.writer.text-escaping" >:: fun _ ->
    expect "text escaping" [`Text ["<foo&bar>\xc2\xa0baz"]]
      [S "&lt;foo&amp;bar&gt;&nbsp;baz"]);

  ("html.writer.doctype" >:: fun _ ->
    let doctype =
      {doctype_name      = Some "html";
       public_identifier = None;
       system_identifier = None;
       raw_text          = None;
       force_quirks      = false}
    in
    expect "doctype" [`Doctype doctype] [S "<!DOCTYPE html>"];

    let doctype = {doctype with doctype_name = None} in
    expect "bad doctype" [`Doctype doctype] [S "<!DOCTYPE>"]);

  ("html.writer.comment" >:: fun _ ->
    expect "comment" [`Comment "foo"] [S "<!--"; S "foo"; S "-->"]);

  ("html.writer.processing-instruction" >:: fun _ ->
    expect "processing instruction" [`PI ("foo", "bar")]
      [S "<?"; S "foo"; S " "; S "bar"; S ">"]);

  ("html.writer.xml-declaration" >:: fun _ ->
    let xml = {version = "1.0"; encoding = None; standalone = None} in
    expect "xml declaration" [`Xml xml] []);

  ("html.writer.element" >:: fun _ ->
    expect "element"
      [`Start_element ((html_ns, "p"), []);
       `End_element]
      [S "<"; S "p"; S ">"; S "</"; S "p"; S ">"]);

  ("html.writer.void-element" >:: fun _ ->
    expect "void element"
      [`Start_element ((html_ns, "head"), []);
       `Start_element ((html_ns, "meta"), []);
       `End_element;
       `Start_element ((html_ns, "meta"), []);
       `End_element;
       `End_element]
      [S "<"; S "head"; S ">";
       S "<"; S "meta"; S ">";
       S "<"; S "meta"; S ">";
       S "</"; S "head"; S ">"]);

  ("html.writer.void-element-with-content" >:: fun _ ->
    expect "void element with content"
      [`Start_element ((html_ns, "head"), []);
       `Start_element ((html_ns, "meta"), []);
       `Text ["foo"];
       `End_element;
       `Start_element ((html_ns, "meta"), []);
       `End_element;
       `End_element]
      [S "<"; S "head"; S ">";
       S "<"; S "meta"; S ">";
       S "foo";
       S "</"; S "meta"; S ">";
       S "<"; S "meta"; S ">";
       S "</"; S "head"; S ">"]);

  ("html.writer.pre" >:: fun _ ->
    expect "pre"
      [`Start_element ((html_ns, "pre"), []);
       `Text ["\nfoo"];
       `End_element]
      [S "<"; S "pre"; S ">"; S "\n"; S "\nfoo"; S "</"; S "pre"; S ">"]);

  ("html.writer.attributes" >:: fun _ ->
    expect "attributes"
      [`Start_element
        ((html_ns, "p"), [("", "id"), "foo"; ("", "class"), "bar"]);
       `End_element]
      [S "<"; S "p"; S " ";
       S "id"; S "=\""; S "foo"; S "\""; S " ";
       S "class"; S "=\""; S "bar"; S "\""; S ">";
       S "</"; S "p"; S ">"]);

  ("html.writer.attribute-escaping" >:: fun _ ->
    expect "attribute escaping"
      [`Start_element ((html_ns, "p"), [("", "id"), "foo<>\"&\xc2\xa0"]);
       `End_element]
      [S "<"; S "p"; S " ";
       S "id"; S "=\""; S "foo<>&quot;&amp;&nbsp;"; S "\""; S ">";
       S "</"; S "p"; S ">"]);

  ("html.writer.foreign-element" >:: fun _ ->
    expect "foreign element"
      [`Start_element ((svg_ns, "use"), [(xlink_ns, "href"), "#foo"]);
       `End_element]
      [S "<"; S "use"; S " "; S "xlink:href"; S "=\""; S "#foo"; S "\""; S ">";
       S "</"; S "use"; S ">"]);

  ("html.writer.script-element" >:: fun _ ->
     expect "script element"
     [ `Start_element ((html_ns, "script"), []);
       `Text ["true && false"];
       `End_element ]
     [ S "<"; S "script"; S ">";
       S "true && false";
       S "</"; S "script"; S ">" ]);
]
