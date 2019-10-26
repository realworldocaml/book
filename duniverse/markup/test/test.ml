(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2

let suite =
  "markup.ml" >::: List.flatten [
    Test_kstream.tests;
    Test_stream_io.tests;
    Test_encoding.tests;
    Test_input.tests;
    Test_trie.tests;
    Test_xml_tokenizer.tests;
    Test_xml_parser.tests;
    Test_xml_writer.tests;
    Test_html_tokenizer.tests;
    Test_html_parser.tests;
    Test_html_writer.tests;
    Test_detect.tests;
    Test_utility.tests;
    Test_integration.tests
  ]

let () =
  Printf.printf "\nRunning tests in %s\n" (Filename.basename Sys.argv.(0));
  run_test_tt_main suite
