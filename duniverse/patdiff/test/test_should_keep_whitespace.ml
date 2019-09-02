open! Core
open! Async
open! Import
open Patdiff_lib.Std.Private.Should_keep_whitespace

let test file1 lines1 file2 lines2 =
  let file1 = file1, String.strip lines1 in
  let file2 = file2, String.strip lines2 in
  let test (file1, lines1) (file2, lines2) =
    for_diff ~file1 ~lines1:(String.strip lines1) ~file2 ~lines2:(String.strip lines2)
  in
  let should_keep_whitespace = test file1 file2 in
  require_equal [%here] (module Bool) should_keep_whitespace (test file2 file1);
  print_s [%message (should_keep_whitespace : bool)]
;;

let%expect_test ".txt vs .py" =
  test
    "not_python.txt"
    {|not a python file|}
    "is_python.py"
    {|
from __future__ import division
print 8/7
|};
  [%expect {| (should_keep_whitespace true) |}]
;;

let test1 file1 contents1 = test file1 contents1 file1 contents1

let%expect_test "#!/bin/python" =
  test1 "python.py" {|
#!/bin/python
print "foo"
|};
  [%expect {| (should_keep_whitespace true) |}]
;;

let%expect_test "#!/usr/bin/env python3" =
  test1 "python.py" {|
#!/usr/bin/env python3
print "foo"
|};
  [%expect {| (should_keep_whitespace true) |}]
;;

let%expect_test "#!/bin/bash" =
  test1 "bash.sh" {|
#!/bin/bash
echo foo
|};
  [%expect {| (should_keep_whitespace false) |}]
;;
