This is a regression test suite. It is *not* a performance test suite.

The subdirectory `good/` contains a series of test grammars which Menhir is
supposed to accept. We test that the following commands:

```
  menhir --only-preprocess
  menhir --explain -lg 2 -la 2 -lc 2
```

both succeed and produce the expected output. We do *not* test that the
generated parsers behave correctly.

The subdirectory `bad/` contains a series of test grammars which Menhir is
supposed to reject. We test that the following command:

```
  menhir
```

fails and produces the expected output.

Some tests involve multiple files. By convention, if several files have the
same name up to a numeric suffix, then they belong in a single group and
should be fed together to Menhir.
