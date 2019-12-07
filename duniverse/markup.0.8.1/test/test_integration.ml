(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2

open Markup__Common
open Markup

let tests = [
  ("integration.xml" >:: fun _ ->
    "<?xml version='1.0' encoding='windows-1252'?><root>\xa0</root><a></a>"
    |> string
    |> parse_xml
    |> signals
    |> write_xml
    |> to_string
    |> assert_equal
      ("<?xml version=\"1.0\" encoding=\"windows-1252\"?>" ^
       "<root>\xc2\xa0</root><a/>");

    "\xfe\xff\x00f\x00o\x00o"
    |> string
    |> parse_xml
    |> signals
    |> write_xml
    |> to_string
    |> assert_equal "foo");

  ("integration.html" >:: fun _ ->
    "<!DOCTYPE html><html><body><p><em>foo<p>bar"
    |> string
    |> parse_html
    |> signals
    |> write_html
    |> to_string
    |> assert_equal
      ("<!DOCTYPE html><html><head></head><body><p><em>foo</em></p>" ^
       "<p><em>bar</em></p></body></html>"));

  ("integration.html.encoding" >:: fun _ ->
    ("<!DOCTYPE html><html><head><meta http-equiv='content-type' " ^
     "content='text/html' charset='iso-8859-15'></head>" ^
     "<body><p><em>\xA0\xA4foo<p>bar")
    |> string
    |> parse_html
    |> signals
    |> write_html
    |> to_string
    |> assert_equal
      ("<!DOCTYPE html><html><head><meta http-equiv=\"content-type\" " ^
       "content=\"text/html\" charset=\"iso-8859-15\"></head>" ^
       "<body><p><em>&nbsp;\xE2\x82\xACfoo</em></p>" ^
       "<p><em>bar</em></p></body></html>"));

  ("integration.html.context-disambiguation" >:: fun _ ->
    "<a></a>"
    |> string
    |> parse_html ~context:(`Fragment "svg")
    |> signals
    |> to_list
    |> assert_equal [`Start_element ((Ns.svg, "a"), []); `End_element]);

  ("integration.pretty_print" >:: fun _ ->
    "<root>foo<nested>bar</nested><nested>baz</nested></root>"
    |> string
    |> parse_xml
    |> signals
    |> pretty_print
    |> write_xml
    |> to_string
    |> assert_equal
      ("<root>\n foo\n <nested>\n  bar\n </nested>\n" ^
       " <nested>\n  baz\n </nested>\n</root>\n"));

  ("integration.locations" >:: fun _ ->
    let parser = "<root>foo</root>" |> string |> parse_xml in

    assert_equal (location parser) (1, 1);
    parser |> signals |> next |> ignore;
    assert_equal (location parser) (1, 1);
    parser |> signals |> next |> ignore;
    assert_equal (location parser) (1, 7);
    parser |> signals |> next |> ignore;
    assert_equal (location parser) (1, 10);
    parser |> signals |> next |> ignore;
    assert_equal (location parser) (1, 10));

  ("integration.reread-html-tree" >:: fun _ ->
    let stream = "<p></p>" |> string |> parse_html |> signals in

    let assemble () =
      stream |> tree ~text:(fun _ -> ()) ~element:(fun _ _ _ -> ()) in

    assert_equal ~msg:"fi" (assemble ()) (Some ());
    assert_equal ~msg:"fi" (assemble ()) None)
]
