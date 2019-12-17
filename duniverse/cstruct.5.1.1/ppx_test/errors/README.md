This tests error cases for `ppx_cstruct`.

It only deals with the errors raised by the ppx itself (such as when the type is
not supported), not errors in the generated code.

To add a test case:

- create a file in this directory named `something.ml` with the error case
- create an empty file named `something.ml.expected`
- don't forget to add these files to git
- run `dune runtest`: it displays a diff on `dune.inc`
- run `dune promote`: it updates `dune.inc` with the generated test case
- run `dune runtest`: it runs the test and displays a diff against the empty
  expected output
- run `dune promote`: it updates `something.ml.expected`
