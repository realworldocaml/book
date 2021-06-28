(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

open Markup__Common
open Markup__Kstream
open Markup__Stream_io
open Markup__Encoding
open Markup__Input

let ok = wrong_k "failed"

let tests = [
  ("input.xml" >:: fun _ ->
    expect_error (4, 2) (`Bad_token ("U+0000", "input", "out of range"))
    begin fun report ->
      let s, get_location =
        string "fo\no\xc2\xa0ba\rr\xa0ba\r\nz\x00quux"
        |> utf_8
        |> preprocess is_valid_xml_char report
      in

      to_list s ok (assert_equal [
        (1, 1), 0x66;
        (1, 2), 0x6F;
        (1, 3), 0x0A;
        (2, 1), 0x6F;
        (2, 2), 0xA0;
        (2, 3), 0x62;
        (2, 4), 0x61;
        (2, 5), 0x0A;
        (3, 1), 0x72;
        (3, 2), 0xFFFD;
        (3, 3), 0x62;
        (3, 4), 0x61;
        (3, 5), 0x0A;
        (4, 1), 0x7A;
        (4, 2), 0x00;
        (4, 3), 0x71;
        (4, 4), 0x75;
        (4, 5), 0x75;
        (4, 6), 0x78
      ]);

      get_location () |> assert_equal (4, 7)
    end);

  ("input.html" >:: fun _ ->
    expect_error (1, 8) (`Bad_token ("U+0001", "input", "out of range"))
    begin fun report ->
      let s, get_location =
        string "foo\x00bar\x01"
        |> utf_8
        |> preprocess is_valid_html_char report
      in

      to_list s ok (assert_equal [
        (1, 1), 0x66;
        (1, 2), 0x6F;
        (1, 3), 0x6F;
        (1, 4), 0x00;
        (1, 5), 0x62;
        (1, 6), 0x61;
        (1, 7), 0x72;
        (1, 8), 0x01
      ]);

      get_location () |> assert_equal (1, 9)
    end);

  ("input.bom" >:: fun _ ->
    [0xFEFF; 0x66]
    |> of_list
    |> preprocess is_valid_xml_char Markup__Error.ignore_errors
    |> fst
    |> fun s -> to_list s ok (assert_equal [(1, 1), 0x66]))
]
