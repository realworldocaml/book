(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

open Markup__Common
open Markup__Kstream
open Markup__Stream_io
open Markup__Encoding

let ok = wrong_k "failed"

let test_ucs_4 (f : Markup__Encoding.t) name s1 s2 bad_bytes =
  expect_error (1, 2) (`Decoding_error (bad_bytes, name))
  begin fun report ->
    let chars = s1 |> string |> f ~report in
    next_option chars ok (assert_equal (Some (Char.code 'f')));
    next_option chars ok (assert_equal (Some u_rep));
    next_option chars ok (assert_equal (Some (Char.code 'o')));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None)
  end;

  expect_error (2, 2) (`Decoding_error ("\x00\x00\x00", name))
  begin fun report ->
    let chars = s2 |> string |> f ~report in
    next_option chars ok (assert_equal (Some (Char.code 'f')));
    next_option chars ok (assert_equal (Some 0x000A));
    next_option chars ok (assert_equal (Some (Char.code 'o')));
    next_option chars ok (assert_equal (Some u_rep));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None)
  end

let tests = [
  ("encoding.utf_8" >:: fun _ ->
    let s = "\xef\xbb\xbffoo\xf0\x9f\x90\x99bar\xa0more" in
    expect_error (1, 8) (`Decoding_error ("\xa0", "utf-8")) begin fun report ->
      let chars = s |> string |> utf_8 ~report in
      next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
      next_option chars ok (assert_equal (Some 0x1F419));
      next_n 3 chars ok (assert_equal (List.map Char.code ['b'; 'a'; 'r']));
      next_option chars ok (assert_equal (Some u_rep));
      next_n 4 chars ok
        (assert_equal (List.map Char.code ['m'; 'o'; 'r'; 'e']));
      next_option chars ok (assert_equal None);
      next_option chars ok (assert_equal None)
    end);

  ("encoding.utf_16be" >:: fun _ ->
    let s = "\xfe\xff\x00f\x00o\x00o\xd8\x3d\xdc\x19\x00b\xdc\x19\x00a\x00r" in
    expect_error (1, 6) (`Decoding_error ("\xdc\x19", "utf-16be"))
    begin fun report ->
      let chars = s |> string |> utf_16be ~report in
      next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
      next_option chars ok (assert_equal (Some 0x1F419));
      next_option chars ok (assert_equal (Some (Char.code 'b')));
      next_option chars ok (assert_equal (Some u_rep));
      next_n 16 chars ok (assert_equal (List.map Char.code ['a'; 'r']));
      next_option chars ok (assert_equal None);
      next_option chars ok (assert_equal None)
    end);

  ("encoding.utf_16le" >:: fun _ ->
    let s = "\xff\xfef\x00o\x00o\x00\x3d\xd8\x19\xdcb\x00\x19\xdca\x00r\x00" in
    expect_error (1, 6) (`Decoding_error ("\x19\xdc", "utf-16le"))
    begin fun report ->
      let chars = s |> string |> utf_16le ~report in
      next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
      next_option chars ok (assert_equal (Some 0x1F419));
      next_option chars ok (assert_equal (Some (Char.code 'b')));
      next_option chars ok (assert_equal (Some u_rep));
      next_n 16 chars ok (assert_equal (List.map Char.code ['a'; 'r']));
      next_option chars ok (assert_equal None);
      next_option chars ok (assert_equal None)
    end);

  ("encoding.iso_8859_1" >:: fun _ ->
    let chars = string "foo\xa0\xa4" |> iso_8859_1 in
    next_n 5 chars
      ok (assert_equal (List.map Char.code ['f'; 'o'; 'o'; '\xa0'; '\xa4']));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None));

  ("encoding.iso_8859_15" >:: fun _ ->
    let chars = string "foo\xa0\xa4" |> iso_8859_15 in
    next_n 4 chars
      ok (assert_equal (List.map Char.code ['f'; 'o'; 'o'; '\xa0']));
    next_option chars ok (assert_equal (Some 0x20AC));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None));

  ("encoding.us_ascii" >:: fun _ ->
    let s = "foo\xa0bar" in
    expect_error (1, 4) (`Decoding_error ("\xa0", "us-ascii"))
    begin fun report ->
      let chars = s |> string |> us_ascii ~report in
      next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
      next_option chars ok (assert_equal (Some u_rep));
      next_n 3 chars ok (assert_equal (List.map Char.code ['b'; 'a'; 'r']));
      next_option chars ok (assert_equal None);
      next_option chars ok (assert_equal None)
    end);

  ("encoding.windows_1251" >:: fun _ ->
    let chars = string "foo\xe0\xe1\xe2bar" |> windows_1251 in
    next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
    next_n 3 chars ok (assert_equal [0x0430; 0x0431; 0x0432]);
    next_n 3 chars ok (assert_equal (List.map Char.code ['b'; 'a'; 'r']));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None));

  ("encoding.windows_1252" >:: fun _ ->
    let chars = string "foo\x80\x83bar" |> windows_1252 in
    next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
    next_n 2 chars ok (assert_equal [0x20AC; 0x0192]);
    next_n 3 chars ok (assert_equal (List.map Char.code ['b'; 'a'; 'r']));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None));

  ("encoding.ucs_4be" >:: fun _ ->
    test_ucs_4 ucs_4be "ucs-4be"
      "\x00\x00\xfe\xff\x00\x00\x00f\x80\x00\x00\x00\x00\x00\x00o"
      "\x00\x00\x00f\x00\x00\x00\n\x00\x00\x00o\x00\x00\x00"
      "\x80\x00\x00\x00");

  ("encoding.ucs_4le" >:: fun _ ->
    test_ucs_4 ucs_4le "ucs-4le"
      "\xff\xfe\x00\x00f\x00\x00\x00\x00\x00\x00\x80o\x00\x00\x00"
      "f\x00\x00\x00\n\x00\x00\x00o\x00\x00\x00\x00\x00\x00"
      "\x00\x00\x00\x80");

  ("encoding.ucs_4be_transposed" >:: fun _ ->
    test_ucs_4 ucs_4be_transposed "ucs-4be-transposed"
      "\x00\x00\xff\xfe\x00\x00f\x00\x00\x80\x00\x00\x00\x00o\x00"
      "\x00\x00f\x00\x00\x00\n\x00\x00\x00o\x00\x00\x00\x00"
      "\x00\x80\x00\x00");

  ("encoding.ucs_4le_transposed" >:: fun _ ->
    test_ucs_4 ucs_4le_transposed "ucs-4le-transposed"
      "\xfe\xff\x00\x00\x00f\x00\x00\x00\x00\x80\x00\x00o\x00\x00"
      "\x00f\x00\x00\x00\n\x00\x00\x00o\x00\x00\x00\x00\x00"
      "\x00\x00\x80\x00");

  ("encoding.ebcdic" >:: fun _ ->
    let chars = string "\x86\x96\x96" |> ebcdic in
    next_n 3 chars ok (assert_equal (List.map Char.code ['f'; 'o'; 'o']));
    next_option chars ok (assert_equal None);
    next_option chars ok (assert_equal None));
]
