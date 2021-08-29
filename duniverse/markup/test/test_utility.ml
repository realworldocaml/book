(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

open Markup__Common
open! Markup
module Kstream = Markup__Kstream

let doctype =
  `Doctype
    {Markup.doctype_name = Some "html";
     public_identifier   = None;
     system_identifier   = None;
     raw_text            = None;
     force_quirks        = false}

let start_element name = `Start_element ((Markup.Ns.html, name), [])

let ok = wrong_k "failed"

type dom =
  | Text of string
  | Element of string * dom list

let tests = [
  ("utility.content" >:: fun _ ->
    "<?xml version='1.0'?><!DOCTYPE html><!--blah--><p>foo</p><?bar baz?>"
    |> string
    |> parse_xml
    |> signals
    |> content
    |> write_xml
    |> to_string
    |> assert_equal "<p>foo</p>");

  ("utility.strings_to_bytes" >:: fun _ ->
    ["foo"; "bar"]
    |> Kstream.of_list
    |> Markup__Utility.strings_to_bytes
    |> fun s ->
      Kstream.to_list s ok (assert_equal ['f'; 'o'; 'o'; 'b'; 'a'; 'r']));

  ("utility.tree" >:: fun _ ->
    [start_element "a";
     `Comment "blah";
     `Text ["foo"];
     start_element "b";
     `Text ["bar"];
     `End_element;
     `Text ["baz"];
     `End_element]
    |> of_list
    |> tree
      ~text:(fun ss -> Text (String.concat "" ss))
      ~element:(fun (_, name) _ children -> Element (name, children))
    |> assert_equal
      (Some
        (Element ("a",
          [Text "foo"; Element ("b", [Text "bar"]); Text "baz"]))));

  ("utility.tree.empty" >:: fun _ ->
    []
    |> of_list
    |> tree ~text:ignore ~element:(fun _ _ _ -> ())
    |> assert_equal None);

  ("utility.tree.reread" >:: fun _ ->
    let signals =
      [start_element "p";
       `End_element;
       start_element "p";
       `End_element]
      |> of_list
    in

    tree signals ~text:ignore ~element:(fun _ _ _ -> ()) |> ignore;

    signals |> to_list |> List.length |> assert_equal 2);

  ("utility.from_tree" >:: fun _ ->
    let dom =
      Element ("p", [Text "foo"; Element ("em", [Text "bar"]); Text "baz"]) in
    dom
    |> from_tree (function
      | Element (name, children) ->
        `Element ((Markup.Ns.html, name), [], children)
      | Text s ->
        `Text s)
    |> to_list
    |> assert_equal [
      start_element "p";
      `Text ["foo"];
      start_element "em";
      `Text ["bar"];
      `End_element;
      `Text ["baz"];
      `End_element]);

  ("utility.text" >:: fun _ ->
    [`Xml {Markup.version = "1.0"; encoding = None; standalone = None};
     `Comment "blah";
     `Text ["foo"];
     start_element "a";
     `Text ["bar"; "baz"];
     `End_element]
    |> of_list
    |> text
    |> to_string
    |> assert_equal "foobarbaz");

  ("utility.trim" >:: fun _ ->
    [start_element "div";
     `Text ["\n "];
     start_element "p";
     `Text ["\n  "];
     start_element "em";
     `Text ["foo"];
     `End_element;
     `Text [" bar\n "];
     `End_element;
     `Text ["\n "];
     start_element "pre";
     `Text ["\n baz \n "];
     `End_element;
     `Text ["\n"];
     `End_element]
    |> of_list
    |> trim
    |> to_list
    |> assert_equal
    [start_element "div";
     start_element "p";
     start_element "em";
     `Text ["foo"];
     `End_element;
     `Text [" bar"];
     `End_element;
     start_element "pre";
     `Text ["\n baz \n "];
     `End_element;
     `End_element]);

  ("utility.trim.doctype" >:: fun _ ->
    [doctype;
     `Text ["\n"];
     start_element "div";
     `End_element]
    |> of_list
    |> trim
    |> to_list
    |> assert_equal [
      doctype;
      start_element "div";
      `End_element]);

  ("utility.normalize_text" >:: fun _ ->
    [`Text [""];
     start_element "a";
     `Text ["foo"];
     `Text ["bar"];
     `End_element;
     `Text ["foo"; "bar"];
     `Text ["baz"; ""; "quux"]]
    |> of_list
    |> normalize_text
    |> to_list
    |> assert_equal [
      start_element "a";
      `Text ["foo"; "bar"];
      `End_element;
      `Text ["foo"; "bar"; "baz"; "quux"]]);

  ("utility.pretty_print" >:: fun _ ->
    [start_element "div";
     start_element "p";
     start_element "em";
     `Text ["foo"];
     `End_element;
     `Text ["bar"];
     `End_element;
     start_element "pre";
     `Text ["\n baz \n "];
     `End_element;
     `End_element]
    |> of_list
    |> pretty_print
    |> to_list
    |> assert_equal [
     start_element "div";
     `Text ["\n"; " "];
     start_element "p";
     `Text ["\n"; "  "];
     start_element "em";
     `Text ["foo"];
     `End_element;
     `Text ["bar"; "\n"; " "];
     `End_element;
     `Text ["\n"; " "];
     start_element "pre";
     `Text ["\n baz \n "];
     `End_element;
     `Text ["\n"];
     `End_element;
     `Text ["\n"]]);

  ("utility.pretty_print.doctype" >:: fun _ ->
    [doctype;
     start_element "div";
     start_element "p";
     `End_element;
     `End_element]
    |> of_list
    |> pretty_print
    |> to_list
    |> assert_equal [
      doctype;
      `Text ["\n"];
      start_element "div";
      `Text ["\n"; " "];
      start_element "p";
      `End_element;
      `Text ["\n"];
      `End_element;
      `Text ["\n"]]);

  ("utility.pretty_print.doctype.existing-newline" >:: fun _ ->
    [doctype;
     `Text ["\n"];
     start_element "div";
     `End_element]
    |> of_list
    |> pretty_print
    |> to_list
    |> assert_equal [
      doctype;
      `Text ["\n"];
      start_element "div";
      `End_element;
      `Text ["\n"]]);

  ("utility.html5" >:: fun _ ->
    [doctype;
     doctype;
     `PI ("foo", "bar");
     `Xml {Markup.version = "1.0"; encoding = Some "utf-8"; standalone = None};
     `Comment "foo";
     start_element "p";
     `Text ["foo"];
     `End_element]
    |> of_list
    |> html5
    |> to_list
    |> assert_equal [
      doctype;
      `Comment "foo";
      start_element "p";
      `Text ["foo"];
      `End_element]);

  ("utility.xhtml" >:: fun _ ->
    [doctype;
     doctype;
     `PI ("foo", "bar");
     `Xml {Markup.version = "1.0"; encoding = Some "utf-8"; standalone = None};
     `Comment "foo";
     start_element "p";
     `Text ["foo"];
     `End_element]
    |> of_list
    |> xhtml
    |> to_list
    |> assert_equal [
      `Xml {Markup.version = "1.0"; encoding = Some "utf-8"; standalone = None};
      `Doctype {
        Markup.doctype_name = None;
        public_identifier = None;
        system_identifier = None;
        raw_text = Some
          ("html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" " ^
           "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"");
        force_quirks = false};
      `PI ("foo", "bar");
      `Comment "foo";
     start_element "p";
     `Text ["foo"];
     `End_element]);
]
