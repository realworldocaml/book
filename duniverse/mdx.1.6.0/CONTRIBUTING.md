## Setting up your working environment

If you want to contribute to the project you'll first need to install the dependencies.
You can do it via opam:
```
$ git clone git@github.com:realworldocaml/mdx.git
$ cd mdx
$ opam install -t --deps-only .
```

This will install both regular and test dependencies to your active opam switch.

From there you can build mdx by simply running:
```
$ dune build
```
and run the test suite with:
```
$ dune runtest
```

## Tests

In a effort to cover as much of the codebase with tests as possible, new contributions
should come with tests when possible/it makes sense.

MDX has both unit tests and end-to-end tests.

### Unit testing

We should aim at improving the unit tests coverage as much as possible.
Our unit tests can be found in [`test/lib/`](test/lib).
They are written using the [alcotest](https://github.com/mirage/alcotest) testing framework.
If you want to add new tests, we encourage you to reuse the style used in the existing tests
([`test_block.ml`](test/lib/test_block.ml) is a good example).

There should be one test module per actual module there. The test runner is
[`test_mdx_lib.ml`](test/lib/test_mdx_lib.ml).
If you add tests for a new or so far untested module, don't forget to add its test suite to the
runner.

For each function we test, we build a list of Alcotest `unit test_case`. It's important to try to be
consistent in that regard as it makes the output of the test runner more readable and helps with
fixing or investigating broken tests.

For each module, we then have one Alcotest `unit test` that contains the concatenation of all the
test cases.

That results in the following test output for a successful run:
```
$ dune runtest
test_mdx_lib alias test/lib/runtest
Testing Mdx.
This run has ID `6259B41C-868E-4DFB-B2CE-F6DA840D0676`.
[OK]                Block            0   require_from_line: "let x = 2 + 2".
[OK]                Block            1   require_from_line: "#require \"a\"".
[OK]                Block            2   require_from_line: "# #require \"a\";;".
[OK]                Block            3   require_from_line: "#require \"a,b.c,d\"".
[OK]                Library          0   from_string: "some-lib".
[OK]                Library          1   from_string: "some.lib".
[OK]                Library          2   from_string: "some.lib.x".
[OK]                Library          3   from_string: "".
[OK]                Library          4   from_string: ".lib".
[OK]                Library          5   from_string: "some.".
The full test results are available in `.../_build/default/test/lib/_build/_tests/6259B41C-868E-4DFB-B2CE-F6DA840D0676`.
Test Successful in 0.001s. 10 tests run.
```

### End-to-end testing

End-to-end tests directly call the `ocaml-mdx` binary and make sure it behave in the expected way.
They live in the [`test/bin/`](test/bin) directory.
Some of the test there follow a pattern and use generated dune rules. Some more complex test cases
dune rules are written by hand.

The rule generation logic is defined in
[`gen_rule_helpers/`](test/bin/gen_rule_helpers).

We have one folder there per subcommand we test, for instance the tests for `ocaml-mdx test` live in
[`mdx-test/`](test/bin/mdx-test).
More complex tests that involve invoking several subcommands of mdx should be in the
`misc-test-cases` folder.

Each subcommand folder is divided between three categories. In each of these category folders, there
should be one folder per test case. Here are the three categories
- `expect/` for regular tests. For each test case in there, the subcommand is invoked on an
  input file (`test-case` with a `.md` or `.t` extension), using the options specified in
  `test-case.opts`. The command must succeed and its output must be equal to the content of the
  `.expected` file in that directory. If there is no `.expected` file, the output of the command
  must be equal to the input file.
- `failure/` for testing mdx error paths. The subcommand is invoked on an input file (same as above),
  using the options specified in `test-case.opts`. The command must fail and its output must be
  equal to the content of the `.expected` file.
- `misc/` for tests that don't fall into the previous two categories or for which
  the dune rules are too complex to be generated.

To add a new test-case for `failure` or `expect` simply add a new folder in there and fill it with
the appropriate files. The next time you'll run `dune runtest` it should output a diff with the dune
rules for your newly added test case, something similar to this:
```
$ dune runtest
     patdiff (internal) (exit 1)
(cd _build/default && /home/nathan/.opam/4.08.1/bin/patdiff -keep-whitespace -location-style omake -unrefined test/bin/mdx-test/expect/dune.inc test/bin/mdx-test/expect/dune.gen)
------ test/bin/mdx-test/expect/dune.inc
++++++ test/bin/mdx-test/expect/dune.gen
File "test/bin/mdx-test/expect/dune.inc", line 243, characters 0-1:
 |    (run ocaml-mdx test --output - test-case.md)))))
 |
 |(alias
 | (name runtest)
 | (action (diff multilines/test-case.md multilines.actual)))
 |
 |(rule
+| (target new-test-case.actual)
+| (deps (package mdx) (source_tree new-test-case))
+| (action
+|  (with-stdout-to %{target}
+|   (chdir new-test-case
+|    (run ocaml-mdx test --output - test-case.md)))))
+|
+|(alias
+| (name runtest)
+| (action (diff new-test-case/test-case.md new-test-case.actual)))
+|
+|(rule
 | (target non-det.actual)
 | (deps (package mdx) (source_tree non-det))
 | (action
 |  (with-stdout-to %{target}
```

Running `dune promote` will update the generated rules and from there running `dune runtest` will
now run your new test as well.
