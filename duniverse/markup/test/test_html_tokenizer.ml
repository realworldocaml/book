(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Common
module Error = Markup__Error
module Kstream = Markup__Kstream

let doctype
    ?name ?public_identifier ?system_identifier ?(force_quirks = false) () =
  {doctype_name = name;
   public_identifier;
   system_identifier;
   raw_text     = None;
   force_quirks}

let tag ?(self_closing = false) name attributes =
  {Token_tag.name; attributes; self_closing}

let expect ?state ?(foreign = false) text signals =
  let report, iterate, ended = expect_signals token_to_string text signals in

  let stream, set_state, set_foreign =
    text
    |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8
    |> Markup__Input.preprocess is_valid_html_char Error.ignore_errors
    |> Markup__Html_tokenizer.tokenize report
  in

  set_foreign (fun () -> foreign);

  let stream =
    match state with
    | None -> stream
    | Some state ->
      let open Kstream in
      let switched = ref false in
      (fun throw e k ->
        next stream throw e (fun t ->
        if not !switched then begin
          switched := true;
          set_state state;
        end;
        k t))
      |> make
  in

  iter iterate stream;

  ended ()

let char_sequence ?(start = 1) ?(no_eof = false) s =
  let rec assemble acc index =
    if index >= String.length s then
      let acc = if no_eof then acc else (1, index + start, S `EOF)::acc in
      List.rev acc
    else
      assemble
        ((1, index + start, S (`Char (Char.code s.[index])))::acc) (index + 1)
  in
  assemble [] 0

let tests = [
  ("html.tokenizer.empty" >:: fun _ ->
    expect "" [ 1,  1, S  `EOF]);

  ("html.tokenizer.text" >:: fun _ ->
    expect "foo" (char_sequence "foo");

    expect "f\x00oo"
      ([ 1,  1, S (`Char 0x66);
         1,  2, E (`Bad_token ("U+0000", "content", "null"));
         1,  2, S (`Char 0x00)] @
       (char_sequence ~start:3 "oo")));

  ("html.tokenizer.reference" >:: fun _ ->
    expect "&lt;&nbsp;&#48;&#x31;&#X32;&acE;"
      [ 1,  1, S (`Char 0x3C);
        1,  5, S (`Char 0xA0);
        1, 11, S (`Char 0x30);
        1, 16, S (`Char 0x31);
        1, 22, S (`Char 0x32);
        1, 28, S (`Char 0x223E);
        1, 28, S (`Char 0x0333);
        1, 33, S  `EOF];

    expect "&\t" (char_sequence "&\t");
    expect "&\n"
      [ 1,  1, S (`Char 0x26);
        1,  2, S (`Char 0x0A);
        2,  1, S  `EOF];
    expect "& " (char_sequence "& ");
    expect "&<"
      [ 1,  1, S (`Char 0x26);
        1,  3, E (`Unexpected_eoi "tag");
        1,  2, S (`Char 0x3C);
        1,  3, S  `EOF];
    expect "&&" (char_sequence "&&");
    expect "&" (char_sequence "&"));

  ("html.tokenizer.bad-numeric-reference" >:: fun _ ->
    let reference = "character reference" in

    expect "&#z"
      ([ 1,  1, E (`Bad_token ("&#", reference, "expected digits"))] @
       (char_sequence "&#z"));

    expect "&#xz"
      ([ 1,  1, E (`Bad_token ("&#x", reference, "expected digits"))] @
       (char_sequence "&#xz"));

    expect "&#Xz"
      ([ 1,  1, E (`Bad_token ("&#X", reference, "expected digits"))] @
       (char_sequence "&#Xz"));

    expect "&#48z"
      [ 1,  1, E (`Bad_token ("&#48", reference, "missing ';' at end"));
        1,  1, S (`Char 0x30);
        1,  5, S (`Char 0x7A);
        1,  6, S  `EOF];

    expect "&#x30z"
      [ 1,  1, E (`Bad_token ("&#x30", reference, "missing ';' at end"));
        1,  1, S (`Char 0x30);
        1,  6, S (`Char 0x7A);
        1,  7, S  `EOF];

    expect "&#X30z"
      [ 1,  1, E (`Bad_token ("&#X30", reference, "missing ';' at end"));
        1,  1, S (`Char 0x30);
        1,  6, S (`Char 0x7A);
        1,  7, S  `EOF];

    expect "&#1000000000000000000000000000000;"
      [ 1,  1, E (`Bad_token ("&#1000000000000000000000000000000;",
                              reference, "out of range"));
        1,  1, S (`Char u_rep);
        1, 35, S  `EOF];

    expect "&#1000000000000000000000000000000"
      [ 1,  1, E (`Bad_token ("&#1000000000000000000000000000000",
                              reference, "missing ';' at end"));
        1,  1, E (`Bad_token ("&#1000000000000000000000000000000",
                              reference, "out of range"));
        1,  1, S (`Char u_rep);
        1, 34, S  `EOF];

    expect "&#xD800;"
      [ 1,  1, E (`Bad_token ("&#xD800;", reference, "out of range"));
        1,  1, S (`Char u_rep);
        1,  9, S  `EOF];

    expect "&#x110000;"
      [ 1,  1, E (`Bad_token ("&#x110000;", reference, "out of range"));
        1,  1, S (`Char u_rep);
        1, 11, S  `EOF];

    expect "&#0;"
      [ 1,  1, E (`Bad_token ("&#0;", reference, "out of range"));
        1,  1, S (`Char u_rep);
        1,  5, S  `EOF];

    expect "&#x01;"
      [ 1,  1, E (`Bad_token ("&#x01;", reference, "invalid HTML character"));
        1,  1, S (`Char 0x01);
        1,  7, S  `EOF]);

  ("html.tokenizer.windows-1252-reference" >:: fun _ ->
    let sequence =
      let rec generate acc position = function
        | [] -> List.rev ((1, position, S `EOF)::acc)
        | (reference, translation)::rest ->
          let error =
            1, position, E (`Bad_token
              (Printf.sprintf "&#x%02X;" reference, "character reference",
               "Windows-1252 character"))
          in
          let character = 1, position, S (`Char translation) in
          generate (character::error::acc) (position + 6) rest
      in
      generate [] 1
        [0x80, 0x20AC;
         0x82, 0x201A;
         0x83, 0x0192;
         0x84, 0x201E;
         0x85, 0x2026;
         0x86, 0x2020;
         0x87, 0x2021;
         0x88, 0x02C6;
         0x89, 0x2030;
         0x8A, 0x0160;
         0x8B, 0x2039;
         0x8C, 0x0152;
         0x8E, 0x017D;
         0x91, 0x2018;
         0x92, 0x2019;
         0x93, 0x201C;
         0x94, 0x201D;
         0x95, 0x2022;
         0x96, 0x2013;
         0x97, 0x2014;
         0x98, 0x02DC;
         0x99, 0x2122;
         0x9A, 0x0161;
         0x9B, 0x203A;
         0x9C, 0x0153;
         0x9E, 0x017E;
         0x9F, 0x0178]
    in

    expect ("&#x80;&#x82;&#x83;&#x84;&#x85;&#x86;&#x87;&#x88;&#x89;&#x8A;" ^
            "&#x8B;&#x8C;&#x8E;&#x91;&#x92;&#x93;&#x94;&#x95;&#x96;&#x97;" ^
            "&#x98;&#x99;&#x9A;&#x9B;&#x9C;&#x9E;&#x9F;")
      sequence);

  ("html.tokenizer.bad-entity-reference" >:: fun _ ->
    let reference = "entity reference" in

    expect "&unknown" (char_sequence "&unknown");

    expect "&unknown;"
      ([ 1,  1, E (`Bad_token ("&unknown;", reference, "no such entity"))] @
       (char_sequence "&unknown;"));

    expect "&NBSP" (char_sequence "&NBSP");

    expect "&nbsp"
      ([ 1,  1, E (`Bad_token ("&nbsp", reference, "missing ';' at end"));
         1,  1, S (`Char 0xA0);
         1,  6, S  `EOF]);

    expect "&ltz"
      ([ 1,  1, E (`Bad_token ("&lt", reference, "missing ';' at end"));
         1,  1, S (`Char 0x3C);
         1,  4, S (`Char 0x7A);
         1,  5, S  `EOF]);

    expect "&ltz;"
      ([ 1,  1, E (`Bad_token ("&lt", reference, "missing ';' at end"));
         1,  1, S (`Char 0x3C);
         1,  4, S (`Char 0x7A);
         1,  5, S (`Char 0x3B);
         1,  6, S  `EOF]);

    expect "&a" (char_sequence "&a");

    expect "&\xc2\xa0" (char_sequence "&\xa0"));

  ("html.tokenizer.rcdata" >:: fun _ ->
    expect ~state:`RCDATA "f&lt;"
      [ 1,  1, S (`Char 0x66);
        1,  2, S (`Char 0x3C);
        1,  6, S  `EOF];

    expect ~state:`RCDATA "fo<</</FoO>" (char_sequence "fo<</</FoO>");

    expect ~state:`RCDATA "<title>foo</bar>&lt;</titlE><a>"
      ([ 1,  1, S (`Start (tag "title" []))] @
       (char_sequence ~start:8 ~no_eof:true "foo</bar><") @
       [ 1, 21, S (`End (tag "title" []));
         1, 29, S (`Start (tag "a" []));
         1, 32, S  `EOF]);

    expect ~state:`RCDATA "f\x00</foo>"
      ([ 1,  1, S (`Char 0x66);
         1,  2, E (`Bad_token ("U+0000", "content", "null"));
         1,  2, S (`Char u_rep)] @
       (char_sequence ~start:3 "</foo>"));

    expect ~state:`RCDATA "<title>f</title >"
      [ 1,  1, S (`Start (tag "title" []));
        1,  8, S (`Char 0x66);
        1,  9, S (`End (tag "title" []));
        1, 18, S  `EOF];

    expect ~state:`RCDATA "<title>f</title foo='bar'>"
      [ 1,  1, S (`Start (tag "title" []));
        1,  8, S (`Char 0x66);
        1,  9, E (`Bad_token ("foo", "tag", "end tag with attributes"));
        1,  9, S (`End (tag "title" ["foo", "bar"]));
        1, 27, S  `EOF];

    expect ~state:`RCDATA "<title>f</title/>"
      [ 1,  1, S (`Start (tag "title" []));
        1,  8, S (`Char 0x66);
        1,  9, E (`Bad_token ("/>", "tag", "end tag cannot be self-closing"));
        1,  9, S (`End (tag ~self_closing:true "title" []));
        1, 18, S  `EOF]);

  ("html.tokenizer.rawtext" >:: fun _ ->
    expect ~state:`RAWTEXT "f&lt;" (char_sequence "f&lt;");

    expect ~state:`RAWTEXT "f<</</FoO>" (char_sequence "f<</</FoO>");

    expect ~state:`RAWTEXT "<style>foo</bar>&lt;</style><a>"
      ([ 1,  1, S (`Start (tag "style" []))] @
       (char_sequence ~start:8 ~no_eof:true "foo</bar>&lt;") @
       [ 1, 21, S (`End (tag "style" []));
         1, 29, S (`Start (tag "a" []));
         1, 32, S  `EOF]);

    expect ~state:`RAWTEXT "f\x00</foo>"
      ([ 1,  1, S (`Char 0x66);
         1,  2, E (`Bad_token ("U+0000", "content", "null"));
         1,  2, S (`Char u_rep)] @
       (char_sequence ~start:3 "</foo>")));

  ("html.tokenizer.script-data" >:: fun _ ->
    expect ~state:`Script_data "f<</</FoO>" (char_sequence "f<</</FoO>");

    expect ~state:`Script_data "f<!a" (char_sequence "f<!a");

    expect ~state:`Script_data "f<!-a" (char_sequence "f<!-a");

    expect ~state:`Script_data "f<!-->" (char_sequence "f<!-->");

    expect ~state:`Script_data "f<!--->" (char_sequence "f<!--->");

    expect ~state:`Script_data "f<!--a-->" (char_sequence "f<!--a-->");

    expect ~state:`Script_data "f<!--<a-->" (char_sequence "f<!--<a-->");

    expect ~state:`Script_data "<script><!--a</script><a>"
      ([ 1,  1, S (`Start (tag "script" []))] @
       (char_sequence ~start:9 ~no_eof:true "<!--a") @
       [ 1, 14, S (`End (tag "script" []));
         1, 23, S (`Start (tag "a" []));
         1, 26, S  `EOF]);

    expect ~state:`Script_data "f<!--o\x00o"
      ((char_sequence ~no_eof:true "f<!--o") @
       [1,  7, E (`Bad_token ("U+0000", "script", "null"));
        1,  7, S (`Char u_rep);
        1,  8, S (`Char 0x6F);
        1,  9, E (`Unexpected_eoi "script");
        1,  9, S  `EOF]);

    expect ~state:`Script_data "f<!--a-a-->" (char_sequence "f<!--a-a-->");

    expect ~state:`Script_data "f<!--a-<a-->" (char_sequence "f<!--a-<a-->");

    expect ~state:`Script_data "f<!--a-<scRipt-->"
      (char_sequence "f<!--a-<scRipt-->");

    expect ~state:`Script_data "f<!--a-<scRipt>-->"
      (char_sequence "f<!--a-<scRipt>-->");

    expect ~state:`Script_data "f<!--a-<scRipt>a</scripT>-->"
      (char_sequence "f<!--a-<scRipt>a</scripT>-->");

    expect ~state:`Script_data "f<!--a-<script>-a-</script>-->"
      (char_sequence "f<!--a-<script>-a-</script>-->");

    expect ~state:`Script_data "f<!--a-<script>--a---<--</script>-->"
      (char_sequence "f<!--a-<script>--a---<--</script>-->");

    expect ~state:`Script_data "f<!--a-<script>a</a></0-->"
      (char_sequence "f<!--a-<script>a</a></0-->");

    expect ~state:`Script_data "f<!--a-<a>a-->"
      (char_sequence "f<!--a-<a>a-->");

    expect ~state:`Script_data "f<!--a-\x00-"
      ((char_sequence ~no_eof:true "f<!--a-") @
       [ 1,  8, E (`Bad_token ("U+0000", "script", "null"));
         1,  8, S (`Char u_rep);
         1,  9, S (`Char 0x02D);
         1, 10, E (`Unexpected_eoi "script");
         1, 10, S  `EOF]);

    expect ~state:`Script_data "f<!--a--\x00--"
      ((char_sequence ~no_eof:true "f<!--a--") @
       [ 1,  9, E (`Bad_token ("U+0000", "script", "null"));
         1,  9, S (`Char u_rep);
         1, 10, S (`Char 0x02D);
         1, 11, S (`Char 0x02D);
         1, 12, E (`Unexpected_eoi "script");
         1, 12, S  `EOF]);

    expect ~state:`Script_data "f<!--<script>\x00"
      ((char_sequence ~no_eof:true "f<!--<script>") @
       [ 1, 14, E (`Bad_token ("U+0000", "script", "null"));
         1, 14, S (`Char u_rep);
         1, 15, E (`Unexpected_eoi "script");
         1, 15, S  `EOF]);

    expect ~state:`Script_data "f<!--<script>-\x00-"
      ((char_sequence ~no_eof:true "f<!--<script>-") @
       [ 1, 15, E (`Bad_token ("U+0000", "script", "null"));
         1, 15, S (`Char u_rep);
         1, 16, S (`Char 0x2D);
         1, 17, E (`Unexpected_eoi "script");
         1, 17, S  `EOF]);

    expect ~state:`Script_data "f<!--<script>--\x00--"
      ((char_sequence ~no_eof:true "f<!--<script>--") @
       [ 1, 16, E (`Bad_token ("U+0000", "script", "null"));
         1, 16, S (`Char u_rep);
         1, 17, S (`Char 0x2D);
         1, 18, S (`Char 0x2D);
         1, 19, E (`Unexpected_eoi "script");
         1, 19, S  `EOF]);

    expect ~state:`Script_data "f<!--a< -->" (char_sequence "f<!--a< -->");

    expect ~state:`Script_data "<script>foo</bar>&lt;</script><a>"
      ([ 1,  1, S (`Start (tag "script" []))] @
       (char_sequence ~start:9 ~no_eof:true "foo</bar>&lt;") @
       [ 1, 22, S (`End (tag "script" []));
         1, 31, S (`Start (tag "a" []));
         1, 34, S  `EOF]);

    expect ~state:`Script_data "f\x00</foo>"
      ([ 1,  1, S (`Char 0x66);
         1,  2, E (`Bad_token ("U+0000", "content", "null"));
         1,  2, S (`Char u_rep)] @
       (char_sequence ~start:3 "</foo>")));

  ("html.tokenizer.plaintext" >:: fun _ ->
    expect ~state:`PLAINTEXT "<plaintext>foo&lt;</plaintext>"
      ([ 1,  1, S (`Start (tag "plaintext" []))] @
       (char_sequence ~start:12 "foo&lt;</plaintext>"));

    expect ~state:`PLAINTEXT "f\x00</foo>"
      ([ 1,  1, S (`Char 0x66);
         1,  2, E (`Bad_token ("U+0000", "content", "null"));
         1,  2, S (`Char u_rep)] @
       (char_sequence ~start:3 "</foo>")));

  ("html.tokenizer.comment" >:: fun _ ->
    expect "<!--foo-bar-->"
      [ 1,  1, S (`Comment "foo-bar");
        1, 15, S  `EOF];

    expect "<!---->"
      [ 1,  1, S (`Comment "");
        1,  8, S  `EOF];

    expect "<!---a-->"
      [ 1,  1, S (`Comment "-a");
        1, 10, S  `EOF]);

  ("html.tokenizer.bad-comment" >:: fun _ ->
    expect "<!foo>"
      [ 1,  1, E (`Bad_token ("<!", "comment", "should begin with '<!--'"));
        1,  1, S (`Comment "foo");
        1,  7, S  `EOF];

    expect "<!--\x00foo-->"
      [ 1,  5, E (`Bad_token ("U+0000", "comment", "null"));
        1,  1, S (`Comment "\xef\xbf\xbdfoo");
        1, 12, S  `EOF];

    expect "<!-->"
      [ 1,  1, E (`Bad_token ("<!-->", "comment", "'-->' overlaps '<!--'"));
        1,  1, S (`Comment "");
        1,  6, S  `EOF];

    expect "<!--"
      [ 1,  5, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "");
        1,  5, S  `EOF];

    expect "<!--->"
      [ 1,  1, E (`Bad_token ("<!--->", "comment", "'-->' overlaps '<!--'"));
        1,  1, S (`Comment "");
        1,  7, S  `EOF];

    expect "<!---\x00-->"
      [ 1,  6, E (`Bad_token ("U+0000", "comment", "null"));
        1,  1, S (`Comment "-\xef\xbf\xbd");
        1, 10, S  `EOF];

    expect "<!---"
      [ 1,  6, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "");
        1,  6, S  `EOF];

    expect "<!--a\x00-->"
      [ 1,  6, E (`Bad_token ("U+0000", "comment", "null"));
        1,  1, S (`Comment "a\xef\xbf\xbd");
        1, 10, S  `EOF];

    expect "<!--a"
      [ 1,  6, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "a");
        1,  6, S  `EOF];

    expect "<!--a-\x00-"
      [ 1,  7, E (`Bad_token ("U+0000", "comment", "null"));
        1,  9, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "a-\xef\xbf\xbd");
        1,  9, S  `EOF];

    expect "<!--a--\x00-->"
      [ 1,  8, E (`Bad_token ("U+0000", "comment", "null"));
        1,  1, S (`Comment "a--\xef\xbf\xbd");
        1, 12, S  `EOF];

    expect "<!--a--!>"
      [ 1,  8, E (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
        1,  1, S (`Comment "a");
        1, 10, S  `EOF];

    expect "<!--a--->"
      [ 1,  8, E (`Bad_token ("---", "comment", "'--' should be in '-->'"));
        1,  1, S (`Comment "a-");
        1, 10, S  `EOF];

    expect "<!--a--"
      [ 1,  8, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "a");
        1,  8, S  `EOF];

    expect "<!--a--a-->"
      [ 1,  8, E (`Bad_token ("--a", "comment", "'--' should be in '-->'"));
        1,  1, S (`Comment "a--a");
        1, 12, S  `EOF];

    expect "<!--a--!-->"
      [ 1,  8, E (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
        1,  1, S (`Comment "a--!");
        1, 12, S  `EOF];

    expect "<!--a--!\x00a-->"
      [ 1,  8, E (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
        1,  9, E (`Bad_token ("U+0000", "comment", "null"));
        1,  1, S (`Comment "a--!\xef\xbf\xbda");
        1, 14, S  `EOF];

    expect "<!--a--!a-->"
      [ 1,  8, E (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
        1,  1, S (`Comment "a--!a");
        1, 13, S  `EOF];

    expect "<!--a--!"
      [ 1,  8, E (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
        1,  9, E (`Unexpected_eoi "comment");
        1,  1, S (`Comment "a");
        1,  9, S  `EOF]);

  ("html.tokenizer.doctype" >:: fun _ ->
    expect "<!DOCTYPE html>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ()));
        1, 16, S  `EOF];

    expect "<!DOCTYPE  html>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ()));
        1, 17, S  `EOF];

    expect "<!DOCTYPE hTmL>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ()));
        1, 16, S  `EOF];

    expect "<!DOCTYPE html  >"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ()));
        1, 18, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo'>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo" ()));
        1, 29, S  `EOF];

    expect "<!DOCTYPE html SYSTEM 'bar'>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"bar" ()));
        1, 29, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo'  'bar'>"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                                 ~system_identifier:"bar" ()));
        1, 36, S  `EOF];

    expect "<!DOCTYPE html PuBlIc  \"foo\" >"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo" ()));
        1, 31, S  `EOF];

    expect "<!DOCTYPE html sYsTeM  \"bar\" >"
      [ 1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"bar" ()));
        1, 31, S  `EOF]);

  ("html.tokenizer.bad-doctype" >:: fun _ ->
    expect "<!DOCTYPEhtml>"
      [ 1, 10, E (`Bad_token ("h", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ()));
        1, 15, S  `EOF];

    expect "<!DOCTYPE"
      [ 1, 10, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~force_quirks:true ()));
        1, 10, S  `EOF];

    expect "<!DOCTYPE \x00html>"
      [ 1, 11, E (`Bad_token ("U+0000", "doctype", "null"));
        1,  1, S (`Doctype (doctype ~name:"\xef\xbf\xbdhtml" ()));
        1, 17, S  `EOF];

    expect "<!DOCTYPE >"
      [ 1, 11, E (`Bad_token (">", "doctype", "expected name"));
        1,  1, S (`Doctype (doctype ~force_quirks:true ()));
        1, 12, S  `EOF];

    expect "<!DOCTYPE "
      [ 1, 11, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~force_quirks:true ()));
        1, 11, S  `EOF];

    expect "<!DOCTYPE html\x00>"
      [ 1, 15, E (`Bad_token ("U+0000", "doctype", "null"));
        1,  1, S (`Doctype (doctype ~name:"html\xef\xbf\xbd" ()));
        1, 17, S  `EOF];

    expect "<!DOCTYPE html"
      [ 1, 15, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 15, S  `EOF];

    expect "<!DOCTYPE html "
      [ 1, 16, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 16, S  `EOF];

    expect "<!DOCTYPE html P>f"
      [ 1, 16, E (`Bad_token ("P", "doctype", "expected 'PUBLIC' or 'SYSTEM'"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 18, S (`Char 0x66);
        1, 19, S  `EOF];

    expect "<!DOCTYPE html PUBLIC'foo'>"
      [ 1, 22, E (`Bad_token ("'", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo" ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html PUBLIC\"foo\">"
      [ 1, 22, E (`Bad_token ("\"", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo" ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html PUBLIC>"
      [ 1, 22, E (`Bad_token (">", "doctype", "expected public identifier"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 23, S  `EOF];

    expect "<!DOCTYPE html PUBLIC"
      [ 1, 22, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 22, S  `EOF];

    expect "<!DOCTYPE html PUBLICfoo 'bar'>"
      [ 1, 22, E (`Bad_token ("f", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 32, S  `EOF];

    expect "<!DOCTYPE html PUBLIC >"
      [ 1, 23, E (`Bad_token (">", "doctype", "expected public identifier"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 24, S  `EOF];

    expect "<!DOCTYPE html PUBLIC "
      [ 1, 23, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 23, S  `EOF];

    expect "<!DOCTYPE html PUBLIC foo>"
      [ 1, 23, E (`Bad_token ("f", "doctype",
                              "public identifier must be quoted"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 27, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'f\x00oo>f"
      [ 1, 25, E (`Bad_token ("U+0000", "doctype", "null"));
        1, 28, E (`Bad_token (">", "doctype", "'>' in identifier"));
        1,  1, S (`Doctype (doctype ~name:"html"
                                    ~public_identifier:"f\xef\xbf\xbdoo"
                                    ~force_quirks:true ()));
        1, 29, S (`Char 0x66);
        1, 30, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo"
      [ 1, 27, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 27, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo''bar'>"
      [ 1, 28, E (`Bad_token ("'", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~system_identifier:"bar" ()));
        1, 34, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo'"
      [ 1, 28, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo'bar>"
      [ 1, 28, E (`Bad_token ("b", "doctype",
                              "system identifier must be quoted"));
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 32, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo' "
      [ 1, 29, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 29, S  `EOF];

    expect "<!DOCTYPE html PUBLIC 'foo' bar>"
      [ 1, 29, E (`Bad_token ("b", "doctype",
                              "system identifier must be quoted"));
        1,  1, S (`Doctype (doctype ~name:"html" ~public_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 33, S  `EOF];

    expect "<!DOCTYPE html SYSTEM'foo'>"
      [ 1, 22, E (`Bad_token ("'", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"foo" ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html SYSTEM\"foo\">"
      [ 1, 22, E (`Bad_token ("\"", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"foo" ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html SYSTEM>"
      [ 1, 22, E (`Bad_token (">", "doctype", "expected system identifier"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 23, S  `EOF];

    expect "<!DOCTYPE html SYSTEM"
      [ 1, 22, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 22, S  `EOF];

    expect "<!DOCTYPE html SYSTEMfoo 'bar'>"
      [ 1, 22, E (`Bad_token ("f", "doctype", "expected whitespace"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 32, S  `EOF];

    expect "<!DOCTYPE html SYSTEM >"
      [ 1, 23, E (`Bad_token (">", "doctype", "expected system identifier"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 24, S  `EOF];

    expect "<!DOCTYPE html SYSTEM "
      [ 1, 23, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 23, S  `EOF];

    expect "<!DOCTYPE html SYSTEM foo>"
      [ 1, 23, E (`Bad_token ("f", "doctype",
                              "system identifier must be quoted"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 27, S  `EOF];

    expect "<!DOCTYPE html SYSTEM 'foo'"
      [ 1, 28, E (`Unexpected_eoi "doctype");
        1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"foo"
                                    ~force_quirks:true ()));
        1, 28, S  `EOF];

    expect "<!DOCTYPE html SYSTEM 'foo' and stuff>"
      [ 1, 29, E (`Bad_token ("a", "doctype", "junk after system identifier"));
        1,  1, S (`Doctype (doctype ~name:"html" ~system_identifier:"foo" ()));
        1, 39, S  `EOF];

    expect "<!DOCTYPE html P"
      [ 1, 16, E (`Bad_token ("P", "doctype", "expected 'PUBLIC' or 'SYSTEM'"));
        1,  1, S (`Doctype (doctype ~name:"html" ~force_quirks:true ()));
        1, 17, S  `EOF]);

  ("html.tokenizer.cdata" >:: fun _ ->
    expect ~foreign:true "<![CDATA[foo&lt;<bar>]]>"
      ((char_sequence ~start:10 ~no_eof:true "foo&lt;<bar>") @
       [ 1, 25, S  `EOF]);

    expect ~foreign:true "<![CDATA[foo" (char_sequence ~start:10 "foo");

    expect ~foreign:true "<![CDATA[foo]foo]]foo]]>"
      ((char_sequence ~start:10 ~no_eof:true "foo]foo]]foo") @
       [ 1, 25, S  `EOF]));

  ("html.tokenizer.bad-cdata" >:: fun _ ->
    expect "<![CDATA[foo&lt;<bar]]>"
      [ 1,  1, E (`Bad_token ("<![CDATA[", "content",
                              "CDATA sections not allowed in HTML"));
        1,  1, S (`Comment "[CDATA[foo&lt;<bar]]");
        1, 24, S  `EOF]);

  ("html.tokenizer.start-tag" >:: fun _ ->
    expect "text<foO>text"
      ((char_sequence ~no_eof:true "text") @
       [ 1,  5, S (`Start (tag "foo" []))] @
       (char_sequence ~start:10 "text"));

    expect "<foo >"
      [ 1,  1, S (`Start (tag "foo" []));
        1,  7, S  `EOF];

    expect "<FoO  >"
      [ 1,  1, S (`Start (tag "foo" []));
        1,  8, S  `EOF];

    expect "<foo bar='baz'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 16, S  `EOF];

    expect "<foo  BaR  = 'baz'  quux\t=\t\"lulz\"  enabled>"
      [ 1,  1, S (`Start
                  (tag "foo" ["bar", "baz"; "quux", "lulz"; "enabled", ""]));
        1, 44, S  `EOF];

    expect "<foo enabled >"
      [ 1,  1, S (`Start (tag "foo" ["enabled", ""]));
        1, 15, S  `EOF];

    expect "<foo enabled bar='baz'>"
      [ 1,  1, S (`Start (tag "foo" ["enabled", ""; "bar", "baz"]));
        1, 24, S  `EOF];

    expect "<foo bar=baz>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 14, S  `EOF]);

  ("html.tokenizer.self-closing-tag" >:: fun _ ->
    expect "<foo/>"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1,  7, S  `EOF];

    expect "<foO />"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" []));
        1,  8, S  `EOF];

    expect "<foo bar='baz'/>"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" ["bar", "baz"]));
        1, 17, S  `EOF];

    expect "<foo bar='baz' />"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" ["bar", "baz"]));
        1, 18, S  `EOF];

    expect "<foo bar/>"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" ["bar", ""]));
        1, 11, S  `EOF];

    expect "<foo bar />"
      [ 1,  1, S (`Start (tag ~self_closing:true "foo" ["bar", ""]));
        1, 12, S  `EOF]);

  ("html.tokenizer.end-tag" >:: fun _ ->
    expect "</foo>"
      [ 1,  1, S (`End (tag "foo" []));
        1,  7, S  `EOF];

    expect "</foO >"
      [ 1,  1, S (`End (tag "foo" []));
        1,  8, S  `EOF]);

  ("html.tokenizer.reference-in-attribute" >:: fun _ ->
    let reference = "entity reference" in

    expect "<foo bar='&lt;'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "<"]));
        1, 17, S  `EOF];

    expect "<foo bar='&'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "&"]));
        1, 14, S  `EOF];

    expect "<foo bar='&lt'>"
      [ 1, 11, E (`Bad_token ("&lt", reference, "missing ';' at end"));
        1,  1, S (`Start (tag "foo" ["bar", "<"]));
        1, 16, S  `EOF];

    expect "<foo bar='&ltz'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "&ltz"]));
        1, 17, S  `EOF];

    expect "<foo bar='&lt='>"
      [ 1, 11, E (`Bad_token ("&lt=", "attribute",
                              "unterminated entity reference followed by '='"));
        1,  1, S (`Start (tag "foo" ["bar", "&lt="]));
        1, 17, S  `EOF];

    expect "<foo bar='&image='>"
      [ 1, 11, E (`Bad_token ("&image=", "attribute",
                              "unterminated entity reference followed by '='"));
        1,  1, S (`Start (tag "foo" ["bar", "&image="]));
        1, 20, S  `EOF];

    expect "<foo bar=&amp;>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "&"]));
        1, 16, S  `EOF];

    expect "<foo bar='&acE;'>"
      [ 1,  1, S (`Start (tag "foo" ["bar", "\xe2\x88\xbe\xcc\xb3"]));
        1, 18, S  `EOF]);

  ("html.tokenizer.bad-attribute-set" >:: fun _ ->
    expect "<foo bar='a' bar='b'>"
      [ 1,  1, E (`Bad_token ("bar", "tag", "duplicate attribute"));
        1,  1, S (`Start (tag "foo" ["bar", "a"]));
        1, 22, S  `EOF];

    expect "<foo BaR='a' bAr='b'>"
      [ 1,  1, E (`Bad_token ("bar", "tag", "duplicate attribute"));
        1,  1, S (`Start (tag "foo" ["bar", "a"]));
        1, 22, S  `EOF];

    expect "</foo bar='a'>"
      [ 1,  1, E (`Bad_token ("bar", "tag", "end tag with attributes"));
        1,  1, S (`End (tag "foo" ["bar", "a"]));
        1, 15, S  `EOF]);

  ("html.tokenizer.bad-start-tag" >:: fun _ ->
    expect "< "
      ([ 1,  2, E (`Bad_token (" ", "tag", "invalid start character"))] @
       (char_sequence "< "));

    expect "<"
      [ 1,  2, E (`Unexpected_eoi "tag");
        1,  1, S (`Char 0x3C);
        1,  2, S  `EOF];

    expect "<f"
      [ 1,  3, E (`Unexpected_eoi "tag");
        1,  3, S  `EOF];

    expect "<f\x00>"
      [ 1,  3, E (`Bad_token ("U+0000", "tag name", "null"));
        1,  1, S (`Start (tag "f\xef\xbf\xbd" []));
        1,  5, S  `EOF];

    expect "<foo "
      [ 1,  6, E (`Unexpected_eoi "tag");
        1,  6, S  `EOF];

    expect "<foo bar=''"
      [ 1, 12, E (`Unexpected_eoi "tag");
        1, 12, S  `EOF];

    expect "<foo bar=''baz=''>"
      [ 1, 12, E (`Bad_token ("b", "tag",
                              "expected whitespace before attribute"));
        1,  1, S (`Start (tag "foo" ["bar", ""; "baz", ""]));
        1, 19, S  `EOF]);

  ("html.tokenizer.bad-self-closing-tag" >:: fun _ ->
    expect "<foo/"
      [ 1,  6, E (`Unexpected_eoi "tag");
        1,  6, S  `EOF];

    expect "<foo / bar='baz'>"
      [ 1,  7, E (`Bad_token (" ", "tag", "expected '/>'"));
        1,  1, S (`Start (tag "foo" ["bar", "baz"]));
        1, 18, S  `EOF]);

  ("html.tokenizer.bad-end-tag" >:: fun _ ->
    expect "</foo/>"
      [ 1,  1, E (`Bad_token ("/>", "tag", "end tag cannot be self-closing"));
        1,  1, S (`End (tag ~self_closing:true "foo" []));
        1,  8, S  `EOF];

    expect "</"
      ([ 1,  3, E (`Unexpected_eoi "tag")] @
       (char_sequence "</"));

    expect "</>foo"
      ([ 1,  1, E (`Bad_token ("</>", "tag", "no tag name"))] @
       (char_sequence ~start:4 "foo"));

    expect "</ foo>"
      [ 1,  3, E (`Bad_token (" ", "tag", "invalid start character"));
        1,  1, S (`Comment "foo");
        1,  8, S  `EOF];

    expect "</f"
      [ 1,  4, E (`Unexpected_eoi "tag");
        1,  4, S  `EOF];

    expect "</f\x00>"
      [ 1,  4, E (`Bad_token ("U+0000", "tag name", "null"));
        1,  1, S (`End (tag "f\xef\xbf\xbd" []));
        1,  6, S  `EOF]);

  ("html.tokenizer.bad-attribute" >:: fun _ ->
    let name = "attribute name" in

    expect "<foo \x00bar=''>"
      [ 1,  6, E (`Bad_token ("U+0000", name, "null"));
        1,  1, S (`Start (tag "foo" ["\xef\xbf\xbdbar", ""]));
        1, 14, S  `EOF];

    expect "<foo \"bar=''>"
      [ 1,  6, E (`Bad_token ("\"", name, "invalid start character"));
        1,  1, S (`Start (tag "foo" ["\"bar", ""]));
        1, 14, S  `EOF];

    expect "<foo 'bar=''>"
      [ 1,  6, E (`Bad_token ("'", name, "invalid start character"));
        1,  1, S (`Start (tag "foo" ["'bar", ""]));
        1, 14, S  `EOF];

    expect "<foo <bar=''>"
      [ 1,  6, E (`Bad_token ("<", name, "invalid start character"));
        1,  1, S (`Start (tag "foo" ["<bar", ""]));
        1, 14, S  `EOF];

    expect "<foo =bar=''>"
      [ 1,  6, E (`Bad_token ("=", name, "invalid start character"));
        1,  1, S (`Start (tag "foo" ["=bar", ""]));
        1, 14, S  `EOF];

    expect "<foo b\"'<\x00ar=''>"
      [ 1,  7, E (`Bad_token ("\"", name, "invalid name character"));
        1,  8, E (`Bad_token ("'", name, "invalid name character"));
        1,  9, E (`Bad_token ("<", name, "invalid name character"));
        1, 10, E (`Bad_token ("U+0000", name, "null"));
        1,  1, S (`Start (tag "foo" ["b\"'<\xef\xbf\xbdar", ""]));
        1, 17, S  `EOF];

    expect "<foo bar"
      [ 1,  9, E (`Unexpected_eoi "tag");
        1,  9, S  `EOF];

    expect "<foo bar \x00='baz'>"
      [ 1, 10, E (`Bad_token ("U+0000", name, "null"));
        1,  1, S (`Start (tag "foo" ["bar", ""; "\xef\xbf\xbd", "baz"]));
        1, 18, S  `EOF];

    expect "<foo bar \" \' <>"
      [ 1, 10, E (`Bad_token ("\"", name, "invalid start character"));
        1, 12, E (`Bad_token ("'", name, "invalid start character"));
        1, 14, E (`Bad_token ("<", name, "invalid start character"));
        1,  1, S (`Start (tag "foo" ["bar", ""; "\"", ""; "'", ""; "<", ""]));
        1, 16, S  `EOF];

    expect "<foo bar "
      [ 1, 10, E (`Unexpected_eoi "tag");
        1, 10, S  `EOF];

    let value = "attribute value" in

    expect "<foo bar=\x00 baz=< quux== lulz=` omg=>"
      [ 1, 10, E (`Bad_token ("U+0000", value, "null"));
        1, 16, E (`Bad_token ("<", value, "invalid start character"));
        1, 23, E (`Bad_token ("=", value, "invalid start character"));
        1, 30, E (`Bad_token ("`", value, "invalid start character"));
        1, 36, E (`Bad_token (">", "tag",
                              "expected attribute value after '='"));
        1,  1, S (`Start (tag "foo" ["bar", "\xef\xbf\xbd"; "baz", "<";
                                     "quux", "="; "lulz", "`"; "omg", ""]));
        1, 37, S  `EOF];

    expect "<foo bar="
      [ 1, 10, E (`Unexpected_eoi "tag");
        1, 10, S  `EOF];

    expect "<foo bar='\x00'>"
      [ 1, 11, E (`Bad_token ("U+0000", value, "null"));
        1,  1, S (`Start (tag "foo" ["bar", "\xef\xbf\xbd"]));
        1, 14, S  `EOF];

    expect "<foo bar='"
      [ 1, 11, E (`Unexpected_eoi value);
        1, 11, S  `EOF];

    expect "<foo bar=b\x00\"'<=`az>"
      [ 1, 11, E (`Bad_token ("U+0000", value, "null"));
        1, 12, E (`Bad_token ("\"", value, "invalid character"));
        1, 13, E (`Bad_token ("'", value, "invalid character"));
        1, 14, E (`Bad_token ("<", value, "invalid character"));
        1, 15, E (`Bad_token ("=", value, "invalid character"));
        1, 16, E (`Bad_token ("`", value, "invalid character"));
        1,  1, S (`Start (tag "foo" ["bar", "b\xef\xbf\xbd\"'<=`az"]));
        1, 20, S  `EOF];

    expect "<foo bar=b"
      [ 1, 11, E (`Unexpected_eoi "tag");
        1, 11, S  `EOF]);

  ("html.tokenizer.processing-instruction" >:: fun _ ->
    expect "<?foo?>"
      [ 1,  1, E (`Bad_token ("<?", "content",
                              "HTML does not have processing instructions"));
        1,  1, S (`Comment "foo?");
        1,  8, S  `EOF];

    expect "<?foo>"
      [ 1,  1, E (`Bad_token ("<?", "content",
                              "HTML does not have processing instructions"));
        1,  1, S (`Comment "foo");
        1,  7, S  `EOF];

    expect "<?foo\x00?>"
      [ 1,  1, E (`Bad_token ("<?", "content",
                              "HTML does not have processing instructions"));
        1,  1, S (`Comment "foo\xef\xbf\xbd?");
        1,  9, S  `EOF];

    expect "<?>"
      [ 1,  1, E (`Bad_token ("<?", "content",
                              "HTML does not have processing instructions"));
        1,  1, S (`Comment "");
        1,  4, S  `EOF];

    expect "<?foo"
      [ 1,  1, E (`Bad_token ("<?", "content",
                              "HTML does not have processing instructions"));
        1,  1, S (`Comment "foo");
        1,  6, S  `EOF])
]
