(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

open Markup__Kstream
open Markup__Stream_io
open Markup__Detect

let ok = wrong_k "failed"

let _check_rewound chars s =
  if String.length s > 0 then
    next_option chars ok (assert_equal (Some s.[0]))
  else
    next_option chars ok (assert_equal None)

let _check_encoding_guess f s guess =
  let chars = string s in
  f chars ok (assert_equal guess);
  _check_rewound chars s

let tests = [
  ("detect.normalize_name" >:: fun _ ->
    normalize_name true "l2" |> assert_equal "iso-8859-2";
    normalize_name true "utf8" |> assert_equal "utf-8";
    normalize_name true "\t utf-8 " |> assert_equal "utf-8";
    normalize_name true "sjis" |> assert_equal "shift_jis";
    normalize_name true "foobar" |> assert_equal "foobar";
    normalize_name true " foobar " |> assert_equal "foobar");

  ("detect.guess_from_bom_html" >:: fun _ ->
    let check = _check_encoding_guess guess_from_bom_html in

    check "\xfe\xff\x00f\x00o\x00o" (Some "utf-16be");
    check "\xff\xfef\x00o\x00o\x00" (Some "utf-16le");
    check "\xef\xbb\xbffoo" (Some "utf-8");
    check "foo" None;
    check "\xfe\xff" (Some "utf-16be");
    check "\xff\xfe" (Some "utf-16le");
    check "\xef\xbb\xbf" (Some "utf-8");
    check "" None);

  ("detect.guess_from_bom_xml" >:: fun _ ->
    let check = _check_encoding_guess guess_from_bom_xml in

    check "\x00\x00\xfe\xff\x00\x00\x00f" (Some "ucs-4be");
    check "\x00\x00\xfe\xff" (Some "ucs-4be");
    check "\xff\xfe\x00\x00f\x00\x00\x00" (Some "ucs-4le");
    check "\xff\xfe\x00\x00" (Some "ucs-4le");
    check "\x00\x00\xff\xfe\x00\x00f\x00" (Some "ucs-4be-transposed");
    check "\x00\x00\xff\xfe" (Some "ucs-4be-transposed");
    check "\xfe\xff\x00\x00\x00f\x00\x00" (Some "ucs-4le-transposed");
    check "\xfe\xff\x00\x00" (Some "ucs-4le-transposed");
    check "\xfe\xff\x00f\x00o\x00o" (Some "utf-16be");
    check "\xff\xfef\x00o\x00o\x00" (Some "utf-16le");
    check "\xef\xbb\xbffoo" (Some "utf-8");
    check "foo" None;
    check "\xfe\xff" (Some "utf-16be");
    check "\xff\xfe" (Some "utf-16le");
    check "\xef\xbb\xbf" (Some "utf-8");
    check "" None);

  ("detect.guess_family_xml" >:: fun _ ->
    let check = _check_encoding_guess guess_family_xml in

    check "\x00\x00\x00\x3c" (Some "ucs-4be");
    check "\x3c\x00\x00\x00" (Some "ucs-4le");
    check "\x00\x00\x3c\x00" (Some "ucs-4be-transposed");
    check "\x00\x3c\x00\x00" (Some "ucs-4le-transposed");
    check "\x00\x3c\x00\x3f" (Some "utf-16be");
    check "\x3c\x00\x3f\x00" (Some "utf-16le");
    check "\x3c\x3f\x78\x6d" (Some "utf-8");
    check "\x4c\x6f\xa7\x94" (Some "ebcdic");
    check "foo" None;
    check "" None);

  ("detect.meta_tag_prescan" >:: fun _ ->
    let check ?supported ?limit s result =
      let chars = string s in
      meta_tag_prescan ?supported ?limit chars ok (assert_equal result);
      _check_rewound chars s
    in

    check "" None;
    check "foobar" None;
    check "   " None;

    check "<meta charset='shift_jis'>" (Some "shift_jis");
    check "<mEtA cHaRsEt='SHIFT_JIS'>" (Some "shift_jis");
    check "<meta http-equiv='content-type' content='charset=shift_jis'>"
      (Some "shift_jis");
    check "<meta content='charset=shift_jis' http-equiv='content-type'>"
      (Some "shift_jis");
    check "<meta content='charset=shift_jis'>" None;
    check "<meta http-equiv=' content-type' content='charset=shift_jis'>" None;
    check "<meta http-equiv= content-type content='charset=shift_jis'>"
      (Some "shift_jis");
    check "<meta http-equiv='content-type' content='charset = shift_jis'>"
      (Some "shift_jis");
    check "<meta http-equiv='content-type' content='foo charset = shift_jis'>"
      (Some "shift_jis");
    check "<meta http-equiv='content-type' content='cHaRsEt=SHIFT_JIS'>"
      (Some "shift_jis");
    check "<meta http-equiv='content-type' content='charset=shift_jis; foo'>"
      (Some "shift_jis");
    check "<meta http-equiv='content-type' content='charset=shift_jis foo'>"
      (Some "shift_jis");
    check
      ("<meta http-equiv='content-type' content='text/html' " ^
       "charset='iso-8859-15'>")
      (Some "iso-8859-15");
    check "<meta http-equiv='content-type' content='charset=\"\"'>" None;
    check "<meta>" None;
    check "<meta charset=\"shift_jis\">" (Some "shift_jis");
    check "<meta charset='sjis'>" (Some "shift_jis");
    check "<meta charset='x-user-defined'>" (Some "x-user-defined");
    check "<meta charset=' sjis   '>" (Some "shift_jis");
    check "<meta charset =  'shift_jis'>" (Some "shift_jis");
    check "<meta   charset='shift_jis'>" (Some "shift_jis");
    check "<metacharset='shift_jis'>" None;
    check "<meta charset='shift_jis'" (Some "shift_jis");
    check "<meta charset='shift_jis" None;
    check "<meta charset=shift_jis>" (Some "shift_jis");
    check "<meta charset=sHiFt_JiS>" (Some "shift_jis");
    check "<meta author='shift_jis'>" None;
    check "<meta author='shift_jis'><meta charset='shift_jis'>"
      (Some "shift_jis");
    check "<meta author='foo' charset='shift_jis'>" (Some "shift_jis");

    check "<meta charset='utf-8'><meta charset='shift_jis'>" (Some "utf-8");

    check "<!-- foo --><meta charset='shift_jis'>" (Some "shift_jis");
    check "<!-- <meta charset='utf-8'> --><meta charset='shift_jis'>"
      (Some "shift_jis");
    check "<!-- foo --><html><!-- bar --><meta charset='shift_jis'>"
      (Some "shift_jis");
    check "<!--><meta charset='shift_jis'>" (Some "shift_jis");

    check "<! foo ><meta charset='shift_jis'>" (Some "shift_jis");
    check "<!DOCTYPE html><meta charset='shift_jis'>" (Some "shift_jis");
    check "</   ><meta charset='shift_jis'>" (Some "shift_jis");
    check "<?xml ?><meta charset='shift_jis'>" (Some "shift_jis");

    check " <meta charset='shift_jis'>" (Some "shift_jis");
    check " foobar <meta charset='shift_jis'>" (Some "shift_jis");

    check "<a></a><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a ></a><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href='foo'></a><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href='foo'></a><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href=foo><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a   href = 'foo'  ><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href='foo' alt=hah><meta charset='shift_jis'>" (Some "shift_jis");
    check "<<meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href><meta charset='shift_jis'>" (Some "shift_jis");
    check "<a href  ><meta charset='shift_jis'>" (Some "shift_jis");
    check "</a href><meta charset='shift_jis'>" (Some "shift_jis");

    check "<meta charset='utf-16'>" (Some "utf-8");

    let no_utf_8 =
      fun s k ->
        match s with
        | "utf-8" -> k false
        | _ -> k true
    in

    check ~supported:no_utf_8 "<meta charset='utf-8'>" None;
    check ~supported:no_utf_8 "<meta charset='utf8'>" None;
    check ~supported:no_utf_8 "<meta charset='shift_jis'>" (Some "shift_jis");
    check ~supported:no_utf_8 "<meta charset='utf-8'><meta charset='shift_jis'>"
      (Some "shift_jis");
    check ~supported:no_utf_8 "<meta charset='utf8'><meta charset='shift_jis'>"
      (Some "shift_jis");

    check ~limit:0 "<meta charset='shift_jis'>" None;
    check ~limit:16 "<meta charset='shift_jis'>" None;
    check ~limit:32 "<meta charset='shift_jis'>" (Some "shift_jis"));

  ("detect.read_xml_encoding_declaration" >:: fun _ ->
    let check family s result =
      let chars = string s in
      read_xml_encoding_declaration chars family ok (assert_equal result);
      _check_rewound chars s
    in

    let open Markup__Encoding in

    check utf_8 "<?xml encoding='utf-8'?>" (Some "utf-8");
    check utf_16be "<?xml encoding='utf-8'?>" None;
    check utf_8 "<?xml encoding='us-ascii'?>" (Some "us-ascii");
    check utf_8 "<!-- foo -->   <?xml encoding='utf-8'?>" (Some "utf-8");
    check utf_8 "<!-- foo --> a <?xml encoding='utf-8'?>" None)
]
