(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common

let no_prefixes = fun _ -> None

let expect ?(prefix = no_prefixes) id signals strings =
  let report, iterate, ended = expect_strings id strings in

  signals
  |> Markup__Kstream.of_list
  |> Markup__Xml_writer.write report prefix
  |> iter iterate;

  ended ()

let tests = [
  ("xml.writer.empty" >:: fun _ ->
    expect "empty" [] []);

  ("xml.writer.text" >:: fun _ ->
    expect "text" [`Text ["foo"]] [S "foo"];
    expect "adjacent text" [`Text ["foo"]; `Text ["bar"]] [S "foo"; S "bar"];
    expect "empty text" [`Text [""]] []);

  ("xml.writer.text-escaping" >:: fun _ ->
    expect "text escaping" [`Text ["<foo&bar>"]] [S "&lt;foo&amp;bar&gt;"]);

  ("xml.writer.xml-declaration" >:: fun _ ->
    expect "version only"
      [`Xml {version = "1.0"; encoding = None; standalone = None}]
      [S "<?xml"; S " "; S "version"; S "=\""; S "1.0"; S "\""; S "?>"];

    expect "encoding"
      [`Xml {version = "1.0"; encoding = Some "utf-8"; standalone = None}]
      [S "<?xml"; S " "; S "version"; S "=\""; S "1.0"; S "\""; S " ";
       S "encoding"; S "=\""; S "utf-8"; S "\""; S "?>"];

    expect "standalone: yes"
      [`Xml {version = "1.0"; encoding = None; standalone = Some true}]
      [S "<?xml"; S " "; S "version"; S "=\""; S "1.0"; S "\""; S " ";
       S "standalone"; S "=\""; S "yes"; S "\""; S "?>"];

    expect "standalone: no"
      [`Xml {version = "1.0"; encoding = None; standalone = Some false}]
      [S "<?xml"; S " "; S "version"; S "=\""; S "1.0"; S "\""; S " ";
       S "standalone"; S "=\""; S "no"; S "\""; S "?>"];

    expect "encoding and standalone"
      [`Xml {version = "1.0"; encoding = Some "utf-8"; standalone = Some false}]
      [S "<?xml"; S " "; S "version"; S "=\""; S "1.0"; S "\""; S " ";
       S "encoding"; S "=\""; S "utf-8"; S "\""; S " ";
       S "standalone"; S "=\""; S "no"; S "\""; S "?>"]);

  ("xml.writer.doctype" >:: fun _ ->
    let doctype =
      {doctype_name      = None;
       public_identifier = None;
       system_identifier = None;
       raw_text          = Some "html";
       force_quirks      = false}
    in

    expect "doctype"
      [`Doctype doctype] [S "<!DOCTYPE "; S "html"; S ">"]);

  ("xml.writer.processing-instruction" >:: fun _ ->
    expect "processing instruction"
      [`PI ("foo", "bar")]
      [S "<?"; S "foo"; S " "; S "bar"; S "?>"]);

  ("xml.writer.comment" >:: fun _ ->
    expect "comment" [`Comment "foo"] [S "<!--"; S "foo"; S "-->"]);

  ("xml.writer.element" >:: fun _ ->
    expect "self-closing element"
      [`Start_element (("", "foo"), []); `End_element]
      [S "<"; S "foo"; S "/>"];

    expect "element with text"
      [`Start_element (("", "foo"), []); `Text ["bar"]; `End_element]
      [S "<"; S "foo"; S ">"; S "bar"; S "</"; S "foo"; S ">"];

    expect "nested elements"
      [`Start_element (("", "foo"), []);
       `Start_element (("", "bar"), []);
       `Start_element (("", "baz"), []);
       `End_element;
       `End_element;
       `End_element]
      [S "<"; S "foo"; S ">"; S "<"; S "bar"; S ">"; S "<"; S "baz"; S "/>";
       S "</"; S "bar"; S ">"; S "</"; S "foo"; S ">"]);

    ("xml.writer.attribute" >:: fun _ ->
      expect "attribute"
        [`Start_element (("", "foo"),
          [("", "bar"), "baz"; ("", "blah"), "lulz"]);
         `End_element]
        [S "<"; S "foo"; S " "; S "bar"; S "=\""; S "baz"; S "\""; S " ";
         S "blah"; S "=\""; S "lulz"; S "\""; S "/>"]);

    ("xml.writer.attribute-escaping" >:: fun _ ->
      expect "attribute escaping"
        [`Start_element (("", "foo"), [("", "bar"), "<baz>&'\""]);
         `End_element]
        [S "<"; S "foo"; S " "; S "bar"; S "=\"";
         S "&lt;baz&gt;&amp;&apos;&quot;"; S "\""; S "/>"]);

    ("xml.writer.namespace" >:: fun _ ->
      expect "default namespace"
        [`Start_element (("some_ns", "foo"),
          [(xmlns_ns, "xmlns"), "some_ns";
           ("", "bar"), "baz"]);
         `Start_element (("some_ns", "quux"), []);
         `End_element;
         `End_element]
        [S "<"; S "foo"; S " ";
         S "xmlns"; S "=\""; S "some_ns"; S "\""; S " ";
         S "bar"; S "=\""; S "baz"; S "\""; S ">";
         S "<"; S "quux"; S "/>";
         S "</"; S "foo"; S ">"];

      expect "prefix"
        [`Start_element (("some_ns", "foo"),
          [(xmlns_ns, "a"), "some_ns";
           ("some_ns", "bar"), "baz"]);
         `Start_element (("some_ns", "quux"), []);
         `End_element;
         `End_element]
        [S "<"; S "a:foo"; S " ";
         S "xmlns:a"; S "=\""; S "some_ns"; S "\""; S " ";
         S "a:bar"; S "=\""; S "baz"; S "\""; S ">";
         S "<"; S "a:quux"; S "/>";
         S "</"; S "a:foo"; S ">"];

      expect "shadowing"
        [`Start_element (("", "foo"),
          [(xmlns_ns, "a"), "some_ns"]);
         `Start_element (("some_ns", "bar"),
          [(xmlns_ns, "a"), "other_ns"]);
         `End_element;
         `End_element]
        [S "<"; S "foo"; S " ";
         S "xmlns:a"; S "=\""; S "some_ns"; S "\""; S ">";
         E (`Bad_namespace "some_ns");
         S "<"; S "bar"; S " ";
         S "xmlns:a"; S "=\""; S "other_ns"; S "\""; S "/>";
         S "</"; S "foo"; S ">"];

      expect "shadowing resolution"
        [`Start_element (("", "foo"),
          [(xmlns_ns, "a"), "some_ns";
           (xmlns_ns, "b"), "some_ns"]);
         `Start_element (("some_ns", "bar"),
          [(xmlns_ns, "a"), "other_ns"]);
         `End_element;
         `End_element]
        [S "<"; S "foo"; S " ";
         S "xmlns:a"; S "=\""; S "some_ns"; S "\""; S " ";
         S "xmlns:b"; S "=\""; S "some_ns"; S "\""; S ">";
         S "<"; S "b:bar"; S " ";
         S "xmlns:a"; S "=\""; S "other_ns"; S "\""; S "/>";
         S "</"; S "foo"; S ">"]);

    ("xml.writer.top-level-namespace" >:: fun _ ->
      let prefix = function
        | "some_ns" -> Some "a"
        | _ -> None
      in

      expect ~prefix "top-level namespace"
        [`Start_element (("some_ns", "foo"), []);
         `End_element]
        [S "<"; S "a:foo"; S "/>"];

      expect ~prefix "top-level namespace shadowed"
        [`Start_element (("some_ns", "foo"),
          [(xmlns_ns, "a"), "other_ns"]);
         `End_element]
        [E (`Bad_namespace "some_ns");
         S "<"; S "foo"; S " ";
         S "xmlns:a"; S "=\""; S "other_ns"; S "\""; S "/>"])
]
